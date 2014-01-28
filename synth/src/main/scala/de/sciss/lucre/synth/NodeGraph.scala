/*
 *  ProcWorld.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package synth

import concurrent.stm.{Ref, TMap, InTxn, TSet}
import de.sciss.synth.{SynthDef => SSynthDef, addAfter, addBefore, UGen, SynthGraph, UGenGraph, message}
import de.sciss.{synth, osc}
import collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.{ExecutionContext, Promise, Future}
import de.sciss.synth.ugen.Constant
import de.sciss.synth.ugen.ControlUGenOutProxy
import de.sciss.synth.ugen.UGenOutProxy

final class NodeGraph(val server: Server) {
  import NodeGraph._

  private type Topo = Topology[AuralNode, Edge]

  val ugenGraphs = Ref(Map.empty[NodeGraph.GraphEquality, SynthDef])

  private val topologyRef = Ref[Topo](Topology.empty)

  def addNode(node: AuralNode)(implicit tx: Txn): Unit =
    topologyRef.transform(_.addVertex(node))(tx.peer)

  def removeNode(node: AuralNode)(implicit tx: Txn): Unit =
    topologyRef.transform(_.removeVertex(node))(tx.peer)

  def addEdge(edge: Edge)(implicit tx: Txn): Option[(Topo, AuralNode, Vec[AuralNode])] = {
    val res = topologyRef.get(tx.peer).addEdge(edge)
    res.foreach(tup => topologyRef.set(tup._1)(tx.peer))
    res
  }

  def removeEdge(edge: Edge)(implicit tx: Txn): Unit =
    topologyRef.transform(_.removeEdge(edge))(tx.peer)

  private val msgStampRef     = Ref(0)

  private[synth] def messageTimeStamp: Ref[Int] = msgStampRef

  private val sync            = new AnyRef
  private var bundleWaiting   = Map.empty[Int, Vec[Scheduled]]
  private var bundleReplySeen = -1

  import server.executionContext

  private final class Scheduled(val msgs: Vec[osc.Message with message.Send], allSync: Boolean, cnt: Int,
                                promise: Promise[Unit]) {
    def apply(): Future[Unit] = {
      val fut = sendNow(msgs, allSync, cnt)
      promise.completeWith(fut)
      fut
    }
  }

  private def sendAdvance(cnt: Int): Future[Unit] = {
    if (DEBUG) println("ADVANCE " + cnt)
    val futs: Vec[Future[Unit]] = sync.synchronized {
      val i = bundleReplySeen + 1
      if (i <= cnt) {
        bundleReplySeen = cnt
        val funs = (i to cnt).flatMap { j =>
          bundleWaiting.get(j) match {
            case Some(_funs)  => bundleWaiting -= j; _funs
            case _            => Vec.empty
          }
        }
        funs.map(_.apply())
      }
      else Vec.empty
    }
    reduceFutures(futs)
  }

  private def sendNow(msgs: Vec[osc.Message with message.Send], allSync: Boolean, cnt: Int): Future[Unit] = {
    // val peer = server.peer
    if (DEBUG) println("SEND NOW " + msgs + " - allSync? " + allSync + "; cnt = " + cnt)
    if (allSync) {
      val p = msgs match {
        case Vec(msg) if allSync  => msg
        case _                        => osc.Bundle.now(msgs: _*)
      }
      server ! p
      sendAdvance(cnt)

    } else {
      val bndl  = osc.Bundle.now(msgs: _*)
      val fut   = server.!!(bndl)
      val futR  = fut.recover {
        case message.Timeout() =>
          log("TIMEOUT while sending OSC bundle!")
      }
      futR.flatMap(_ => sendAdvance(cnt))
    }
  }

  def send(bundles: Txn.Bundles): Future[Unit] = {
    // basically:
    // bundles.payload.zipWithIndex.foreach { case (msgs, idx) =>
    //   val dep = bundles.firstCnt - 1 + idx
    //   if( seen( dep ) || msgs.forall( _.isSynchronous ) {
    //     sendOutStraight()
    //     notifySeen( dep )
    //   } else {
    //     addToWaitList()
    //   }

    val cntOff = bundles.firstCnt
    val mapped = bundles.payload.zipWithIndex.map { case (msgs, idx) =>
      val cnt     = cntOff + idx
      val depCnt  = cnt - 1
      val allSync = msgs.forall(_.isSynchronous)
      (depCnt, msgs, allSync, cnt)
    }
    val res = sync.synchronized {
      val (now, later) = mapped.partition(bundleReplySeen >= _._1)
      val futsNow   = now.map   { case (_     , msgs, allSync, cnt) => sendNow(msgs, allSync, cnt) }
      val futsLater = later.map { case (depCnt, msgs, allSync, cnt) =>
        val p   = Promise[Unit]()
        val sch = new Scheduled(msgs, allSync, cnt, p)
        bundleWaiting += depCnt -> (bundleWaiting.getOrElse(depCnt, Vec.empty) :+ sch)
        p.future
      }
      reduceFutures(futsNow ++ futsLater)
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

object NodeGraph /* MMM extends TxnModel[ NodeGraphUpdate ] */ {
  demi =>

  var DEBUG = false

  def reduceFutures(futures: Vec[Future[Unit]])(implicit executionContext: ExecutionContext): Future[Unit] =
    futures match {
      case Vec()        => Future.successful()
      case Vec(single)  => single
      case more         => Future.reduce(futures)((_, _) => ())
    }
  
  final case class Edge(source: AuralNode, sourceKey: String, sink: AuralNode, sinkKey: String)
    extends Topology.Edge[AuralNode] {
    
    def sourceVertex = source
    def targetVertex = sink
  }
  
  var verbose = false

  private val servers     = TSet.empty[Server]
  private val uniqueDefID = Ref(0)

  private def nextDefID()(implicit tx: InTxn): Int = {
    val res = uniqueDefID.get
    uniqueDefID += 1
    res
  }

  def addServer(server: Server)(implicit tx: Txn): Unit = {
    implicit val itx = tx.peer
    if (servers.contains(server)) return
    servers += server
    worlds  += server -> new NodeGraph(server)
  }

  def removeServer(server: Server)(implicit tx: Txn): Unit = {
    implicit val itx = tx.peer
    servers -= server
    worlds  -= server
  }

  // commented out for debugging inspection
  private val worlds = TMap.empty[Server, NodeGraph]

  def addNode(node: AuralNode)(implicit tx: Txn): Unit = {
    val world = worlds(node.server)(tx.peer)
    world.addNode(node)
  }

  def removeNode(node: AuralNode)(implicit tx: Txn): Unit = {
    val world = worlds(node.server)(tx.peer)
    world.removeNode(node)
  }

  def addEdge(edge: Edge)(implicit tx: Txn): Unit = {
    val world                 = worlds(edge.sourceVertex.server)(tx.peer)
    val res                   = world.addEdge(edge)
    val (_, source, affected) = res.getOrElse(sys.error(s"Edge $edge is cyclic"))

    // if (verbose) println("NEW TOPO = " + newTopo + "; SOURCE = " + source + "; AFFECTED = " + affected)
    if (affected.isEmpty) return

    val srcGroup  = source.groupOption
    val tgtGroups = affected.map(p => (p, p.groupOption))
    val isAfter   = source == edge.sourceVertex

    def startMoving(g: Group): Unit = {
      var succ = g
      var pred: Group = null
      val iter = tgtGroups.iterator
      while (iter.hasNext) {
        pred = succ
        val (target, tgtGroup) = iter.next()
        tgtGroup match {
          case Some(g2) =>
            if (isAfter) {
              g2.moveAfter (audible = true, target = pred)
            } else {
              g2.moveBefore(audible = true, target = pred)
            }
            succ = g2

          case None =>
            val g2 = Group(pred, if (isAfter) addAfter else addBefore)
            target.group_=(g2)
            succ = g2
        }
      }
    }

    srcGroup match {
      case None =>
        val g = Group(source.server)
        // g.play(RichGroup.default(g.server))
        source.group_=(g)
        startMoving(g)

      case Some(g) => startMoving(g)
    }
  }

  def removeEdge(edge: Edge)(implicit tx: Txn): Unit = {
    val world = worlds(edge.sourceVertex.server)(tx.peer)
    world.removeEdge(edge)
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

  protected def abbreviate(name: String): String = {
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

  private[synth] def send(server: Server, bundles: Txn.Bundles): Unit = {
    val w = worlds.single.get(server).getOrElse(sys.error("Trying to access unregistered server " + server))
    w.send(bundles)
  }

  private[synth] def messageTimeStamp(server: Server)(implicit tx: Txn): Ref[Int] = {
    val w = worlds.get(server)(tx.peer).getOrElse(sys.error("Trying to access unregistered server " + server))
    w.messageTimeStamp
  }

  private[synth] def getSynthDef(server: Server, graph: SynthGraph, nameHint: Option[String])(implicit tx: Txn): SynthDef =
    getSynthDef(server, graph.expand(synth.impl.DefaultUGenGraphBuilderFactory), nameHint)

  private[synth] def getSynthDef(server: Server, graph: UGenGraph, nameHint: Option[String])(implicit tx: Txn): SynthDef = {
    implicit val itx = tx.peer
    val w = worlds.get(server).getOrElse(sys.error("Trying to access unregistered server " + server))

    // XXX note: unfortunately we have sideeffects in the expansion, such as
    // includeParam for ProcAudioOutput ... And anyways, we might allow for
    // indeterminate GE.Lazies, thus we need to check for UGenGraph equality,
    // not SynthGraph equality
    //      val u = graph.expand

    val equ = new GraphEquality(graph)
    log("request for synth graph " + equ.hashCode)

    w.ugenGraphs.get.get(equ).getOrElse {
      log("synth graph " + equ.hashCode + " is new")
      val name = abbreviate(nameHint.getOrElse("proc")) + "_" + nextDefID()
      val peer = SSynthDef(name, graph)
      val rd = impl.SynthDefImpl(server, peer)
      rd.recv()
      w.ugenGraphs.transform(_ + (equ -> rd))
      rd
    }
  }

  final class GraphEquality(val graph: UGenGraph) extends Proxy {
    private def mapUGen(ugen: UGen): Any = {
      val inStruct = ugen.inputs.map {
        //         case up: UGenProxy => mapUGen( up.source )
        case ugen: UGen.SingleOut => mapUGen(ugen)
        case UGenOutProxy(source, outputIndex: Int) => (mapUGen(source), outputIndex)
        case ctl: ControlUGenOutProxy => ctl
        case c: Constant => c
      }
      (ugen.name, ugen.rate, ugen.specialIndex, inStruct, ugen.outputRates)
    }

    val self: Any = {
      val uStructs = graph.ugens.map { rich =>
        (mapUGen(rich.ugen), rich.inputSpecs)
      }

      (graph.constants, graph.controlValues, graph.controlNames, uStructs)
    }

    override val hashCode: Int = self.hashCode // make it a val
  }
}