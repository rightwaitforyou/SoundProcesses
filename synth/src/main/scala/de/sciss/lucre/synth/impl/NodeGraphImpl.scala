/*
 *  NodeGraphImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth
package impl

import java.io.{ByteArrayOutputStream, DataOutputStream}

import de.sciss.lucre.synth.Txn.Bundles
import de.sciss.osc
import de.sciss.synth.ugen.{Constant, ControlUGenOutProxy, UGenOutProxy}
import de.sciss.synth.{SynthDef => SSynthDef, Escape, UGen, Rate, message, UGenGraph}
import NodeGraph.Edge
import de.sciss.topology.Topology

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.stm.{TMap, InTxn, Ref}
import scala.util.{Failure, Try}

object NodeGraphImpl {
  final val DEBUG = false

  private val uniqueDefID = Ref(0)

  def reduceFutures(futures: Vec[Future[Unit]])(implicit executionContext: ExecutionContext): Future[Unit] =
    futures match {
      case Vec()        => Future.successful(())
      case Vec(single)  => single
      case more         => Future.reduce(futures)((_, _) => ())
    }

  private def allCharsOk(name: String): Boolean = {
    val len = name.length
    var i   = 0
    while (i < len) {
      val c   = name.charAt(i).toInt
      val ok  = c > 36 && c < 123 || c != 95 // in particular, disallow underscore
      if (!ok) return false
      i += 1
    }
    true
  }

  def mkName(nameHint: Option[String])(implicit tx: InTxn): String =
    abbreviate(s"${nameHint.getOrElse("proc")}_${nextDefID()}")

  private def abbreviate(name: String): String = {
    val len = name.length
    if ((len <= 16) && allCharsOk(name)) return name

    val sb  = new StringBuffer(16)
    var i   = 0
    while (i < len && sb.length() < 16) {
      val c = name.charAt(i).toInt
      val ok = c > 36 && c < 123 || c != 95 // in particular, disallow underscore
      if (ok) sb.append(c.toChar)
      i += 1
    }
    sb.toString
  }

  private def nextDefID()(implicit tx: InTxn): Int = {
    val res = uniqueDefID.get
    uniqueDefID += 1
    res
  }

  private final case class UGenEq(name: String, rate: Rate, specialIndex: Int, var inputs: Any, outputRates: Vec[Rate])

  final class GraphEquality(val graph: UGenGraph) extends Proxy {
    private def mapUGen(ugen: UGen, map: java.util.IdentityHashMap[UGen, UGenEq]): Any = {
      val ex  = map.get(ugen); if (ex != null) return ex
      val res = UGenEq(ugen.name, ugen.rate, ugen.specialIndex, (), ugen.outputRates)
      map.put(ugen, res)
      // log(s"GraphEquality.self - mapUGen($ugen)")
      val inStruct = ugen.inputs.map {
        //         case up: UGenProxy => mapUGen( up.source )
        case ugen1: UGen.SingleOut      => mapUGen(ugen1, map)
        case proxy: UGenOutProxy        => (mapUGen(proxy.source, map), proxy.outputIndex)
        case ctl  : ControlUGenOutProxy => ctl
        case c    : Constant            => c
      }
      res.inputs = inStruct
      res
    }

    val self: Any = {
      val map = new java.util.IdentityHashMap[UGen, UGenEq]
      // log("GraphEquality.self")
      val uStructs = graph.ugens.map { rich =>
        (mapUGen(rich.ugen, map), rich.inputSpecs)
      }

      (graph.constants, graph.controlValues, graph.controlNames, uStructs)
    }

    override val hashCode: Int = self.hashCode // make it a val
  }
}
object DummyNodeGraphImpl extends NodeGraph {
  def acquireSynthDef(graph: UGenGraph, nameHint: Option[String])(implicit tx: Txn): SynthDef =
    SynthDefImpl(null /* !!! XXX TODO */,
      SSynthDef("offline", UGenGraph(Vector.empty, Vector.empty, Vector.empty, Vector.empty)))

  def releaseSynthDef(sd: SynthDef)(implicit tx: Txn): Unit = ()

  private[synth] def messageTimeStamp: Ref[Int] = Ref(0)

  def addNode   (node: NodeRef)(implicit tx: Txn) = ()
  def removeNode(node: NodeRef)(implicit tx: Txn) = ()
  def addEdge   (edge: Edge   )(implicit tx: Txn) = Failure(Topology.CycleDetected())
  def removeEdge(edge: Edge   )(implicit tx: Txn) = ()

  def send(bundles: Bundles): Future[Unit] = Future.successful(())

  def topology(implicit tx: Txn): Topology[NodeRef, Edge] = Topology.empty
}

private[impl] final class SynthDefUse(val df: SynthDef, val equ: IndexedSeq[Byte], val count: Int) {
  def increment: SynthDefUse = new SynthDefUse(df, equ, count + 1)
  def decrement: SynthDefUse = new SynthDefUse(df, equ, count - 1)
}

final class NodeGraphImpl(server: Server) extends NodeGraph {
  import NodeGraphImpl._

  private type T = Topology[NodeRef, Edge]

  private[this] val ugenGraphMap  = TMap.empty[IndexedSeq[Byte], SynthDefUse]
  private[this] val synthDefMap   = TMap.empty[SynthDef        , SynthDefUse]

  // number of defs only
  private[this] val numDefsOnline = Ref(0)

  // limit on number of online defs XXX TODO -- head room rather arbitrary
  private[this] val maxDefs       = math.max(128, server.config.maxSynthDefs - 128)

  private val topologyRef = Ref[T](Topology.empty)

  def topology(implicit tx: Txn): T = topologyRef.get(tx.peer)

  def acquireSynthDef(graph: UGenGraph, nameHint: Option[String])(implicit tx: Txn): SynthDef = {
    implicit val itx = tx.peer

    val bos   = new ByteArrayOutputStream
    val dos   = new DataOutputStream(bos)
    Escape.write(graph, dos)
    dos.flush()
    dos.close()
    val bytes = bos.toByteArray
    val equ: IndexedSeq[Byte] = bytes // opposed to plain `Array[Byte]`, this has correct definition of `equals`
    log(s"request for synth graph ${equ.hashCode()}")

    val use0 = ugenGraphMap.getOrElse(equ, {
      log(s"synth graph ${equ.hashCode()} is new")
      val name  = mkName(nameHint)
      val peer  = SSynthDef(name, graph)
      val rd    = impl.SynthDefImpl(server, peer)
      val num   = numDefsOnline()
      val free  = if (num < maxDefs) 0
      else {
        // purge unused defs -- there are two options:
        // either purge an individual def, or purge all currently unused.
        // we go for the second approach at the moment.
        // an even more sophisticated approach would sort things by LRU
        val keysB   = Vector.newBuilder[IndexedSeq[Byte]]
        val defsB   = Vector.newBuilder[SynthDef]

        ugenGraphMap.foreach { case (key, use1) =>
          // a more sophisticated search could include
          // _any_ def as long as it was not submitted
          // during the ongoing transaction
          if (use1.count == 0) {
            keysB += key
            defsB += use1.df
          }
        }

        val keysToPurge = keysB.result()
        log(s"purging ${keysToPurge.size} unused synth-defs")
        if (keysToPurge.isEmpty) throw new IndexOutOfBoundsException("Reached maximum number of online synth defs")
        ugenGraphMap --= keysToPurge
        val defsToPurge = defsB.result()
        synthDefMap  --= defsToPurge
        defsToPurge.foreach(_.dispose())
        keysToPurge.size
      }
      rd.recv()
      val use = new SynthDefUse(rd, equ, count = 1)
      ugenGraphMap.put(equ, use.increment)
      numDefsOnline() = num - free + 1
      use
    })
    val use = use0.increment
    ugenGraphMap.put(use.equ, use)
    synthDefMap .put(use.df , use)
    use.df
  }

  def releaseSynthDef(sd: SynthDef)(implicit tx: Txn): Unit = {
    implicit val itx = tx.peer
    val use0 = synthDefMap.getOrElse(sd, throw new NoSuchElementException(s"SynthDef ${sd.name} was not acquired"))
    val use  = use0.decrement
    ugenGraphMap.put(use.equ, use)
    synthDefMap .put(use.df , use)
  }

  def addNode(node: NodeRef)(implicit tx: Txn): Unit = {
    log(s"NodeGraph.addNode($node)")
    topologyRef.transform(_.addVertex(node))(tx.peer)
  }

  def removeNode(node: NodeRef)(implicit tx: Txn): Unit = {
    log(s"NodeGraph.removeNode($node)")
    topologyRef.transform(_.removeVertex(node))(tx.peer)
  }

  def addEdge(edge: Edge)(implicit tx: Txn): Try[(T, Option[Topology.Move[NodeRef]])] = { // Option[(T, NodeRef, Vec[NodeRef])]
    log(s"NodeGraph.addEdge($edge)")
    val res = topologyRef.get(tx.peer).addEdge(edge)
    res.foreach(tup => topologyRef.set(tup._1)(tx.peer))
    res
  }

  def removeEdge(edge: Edge)(implicit tx: Txn): Unit = {
    log(s"NodeGraph.removeEdge($edge)")
    topologyRef.transform(_.removeEdge(edge))(tx.peer)
  }

  private val msgStampRef     = Ref(0)

  private[synth] def messageTimeStamp: Ref[Int] = msgStampRef

  private val sync            = new AnyRef
  private var bundleWaiting   = Map.empty[Int, Vec[Scheduled]]
  private var bundleReplySeen = -1

  import server.executionContext

  private final class Scheduled(val bundle: Txn.Bundle, promise: Promise[Unit]) {
    def apply(): Future[Unit] = {
      val fut = sendNow(bundle)
      promise.completeWith(fut)
      fut
    }
  }

  private def sendAdvance(stamp: Int): Future[Unit] = {
    if (DEBUG) println(s"ADVANCE $stamp")
    val futures: Vec[Future[Unit]] = sync.synchronized {
      val i = bundleReplySeen + 1
      if (i <= stamp) {
        bundleReplySeen = stamp
        val funs = (i to stamp).flatMap { j =>
          bundleWaiting.get(j) match {
            case Some(_funs)  => bundleWaiting -= j; _funs
            case _            => Vector.empty
          }
        }
        funs.map(_.apply())
      }
      else Vector.empty
    }
    reduceFutures(futures)
  }

  private def sendNow(bundle: Txn.Bundle): Future[Unit] = {
    import bundle.{msgs, stamp}
    if (msgs.isEmpty) return sendAdvance(stamp) // Future.successful(())

    val allSync = (stamp & 1) == 1
    if (DEBUG) println(s"SEND NOW $msgs - allSync? $allSync; stamp = $stamp")
    if (allSync) {
      val p = msgs match {
        case Vec(msg) /* if allSync */ => msg
        case _ => osc.Bundle.now(msgs: _*)
      }
      server ! p
      sendAdvance(stamp)

    } else {
      val bndl  = osc.Bundle.now(msgs: _*)
      val fut   = server.!!(bndl)
      val futR  = fut.recover {
        case message.Timeout() =>
          log("TIMEOUT while sending OSC bundle!")
      }
      futR.flatMap(_ => sendAdvance(stamp))
    }
  }

  def send(bundles: Txn.Bundles): Future[Unit] = {
    val res = sync.synchronized {
      val (now, later) = bundles.partition(bundleReplySeen >= _.depStamp)
      // it is important to process the 'later' bundles first,
      // because now they might rely on some `bundleReplySeen` that is
      // increased by processing the `now` bundles.
      val futuresLater  = later.map { m =>
        val p   = Promise[Unit]()
        val sch = new Scheduled(m, p)
        bundleWaiting += m.depStamp -> (bundleWaiting.getOrElse(m.depStamp, Vector.empty) :+ sch)
        p.future
      }
      val futuresNow    = now.map(sendNow)
      reduceFutures(futuresNow ++ futuresLater)
    }

    server.commit(res)
    res

    //    bundles.payload.zipWithIndex.foreach {
    //      case (msgs, idx) =>
    //        val cnt     = cntOff + idx
    //        val depCnt  = cnt - 1
    //        val allSync = msgs.forall(_.isSynchronous)
    //        sync.synchronized {
    //          if (bundleReplySeen >= depCnt /* || allSync */ ) {
    //            sendNow(msgs, allSync, cnt)
    //          } else {
    //            if (DEBUG) println("WAIT FOR DEP " + depCnt + " TO SEND " + msgs)
    //            bundleWaiting += depCnt -> (bundleWaiting.getOrElse(depCnt, Vec.empty) :+ { () =>
    //              sendNow(msgs, allSync, cnt)
    //            })
    //          }
    //        }
    //    }
  }
}