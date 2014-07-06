/*
 *  AuralProcDataImpl.scala
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

package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.{Node, Txn, Group, NodeRef, AudioBus, Sys}
import de.sciss.model.Change
import de.sciss.synth.{SynthGraph, addBefore}
import de.sciss.synth.proc.AuralObj.ProcData
import de.sciss.synth.proc.Scan.Link
import de.sciss.synth.proc.UGenGraphBuilder.{State => UState, Complete, Incomplete, MissingIn}
import de.sciss.synth.proc.{logAural => logA}

import scala.collection.generic.CanBuildFrom
import scala.concurrent.stm.{TMap, Ref, TxnLocal}

object AuralProcDataImpl {
  def apply[S <: Sys[S]](proc: Obj.T[S, Proc.Elem])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.ProcData[S] =
    context.acquire[AuralObj.ProcData[S]](proc) {
      val ugenInit = UGenGraphBuilder.init(proc)
      val data0 = new Impl(tx.newHandle(proc), ugenInit)
      data0.init(proc)
      data0.tryBuild()
      data0
    }

  private type ObjSource[S <: Sys[S]] = stm.Source[S#Tx, Obj.T[S, Proc.Elem]]

  private sealed trait MapEntryChange[+K, +V]
  private case class MapEntryRemoved[K   ](key: K)                      extends MapEntryChange[K, Nothing]
  private case class MapEntryAdded  [K, V](key: K, value : V)           extends MapEntryChange[K, V]
  private case class MapEntryUpdated[K, V](key: K, before: V, after: V) extends MapEntryChange[K, V]

  def mapEntryChanges[K, V, That](before: Map[K, V], after: Map[K, V], incremental: Boolean)
                                 (implicit cbf: CanBuildFrom[Nothing, MapEntryChange[K, V], That]): That = {
    val b = cbf() // .newBuilder[MapEntryChange[K, V]]
    if (before eq after) return b.result()

    if (!incremental) {
      before.foreach { case (k, vb) =>
        after.get(k) match {
          case Some(va) if vb != va => b += MapEntryUpdated(k, vb, va)
          case None                 => b += MapEntryRemoved(k)
          case _ =>
        }
      }
    }
    after.foreach { case (k, va) =>
      if (!before.contains(k)) b += MapEntryAdded(k, va)
    }
    b.result()
  }

  // dynamically flips between single proc and multiple procs
  // (wrapping them in one common group)
  private final class GroupImpl(in0: NodeRef) extends NodeRef {
    val server = in0.server

    private val instancesRef  = Ref(in0 :: Nil)
    private val nodeRef       = Ref(in0)

    def node(implicit tx: Txn): Node = nodeRef.get(tx.peer).node

    def addInstanceNode(n: NodeRef)(implicit tx: Txn): Unit = {
      implicit val itx = tx.peer
      val old = instancesRef.getAndTransform(n :: _)
      old match {
        case single :: Nil =>
          val g = Group(single.node, addBefore)
          nodeRef() = NodeRef(g)
          single.node.moveToHead(audible = true, group = g)
          n     .node.moveToHead(audible = true, group = g)

        case _ =>
      }
    }

    def removeInstanceNode(n: NodeRef)(implicit tx: Txn): Boolean = {
      implicit val itx = tx.peer
      val after = instancesRef.transformAndGet(_.filterNot(_ == n))
      after match {
        case single :: Nil =>
          val group = nodeRef.swap(single).node
          single.node.moveBefore(audible = true, target = group)
          group.free(audible = true)
          false

        case Nil  => true
        case _    => false
      }
    }
  }

  private final class Impl[S <: Sys[S]](val obj: ObjSource[S], state0: UState[S])
                                       (implicit context: AuralContext[S])
    extends ProcData[S] {

    import context.server

    private val stateRef  = Ref[UState[S]](state0)
    private val nodeRef   = Ref(Option.empty[GroupImpl])
    private val scanViews = TMap.empty[String, AuralScan.Owned[S]]

    private val procLoc   = TxnLocal[Obj.T[S, Proc.Elem]]()   // cache-only purpose

    private var procObserver: Disposable[S#Tx] = _

    def init(proc: Obj.T[S, Proc.Elem])(implicit tx: S#Tx): Unit = {
      procObserver = proc.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case Obj.ElemChange(Proc.Update(_, pCh)) =>
            pCh.foreach {
              case Proc.GraphChange(Change(_, newGraph)) => newSynthGraph(newGraph)
              case Proc.ScanAdded  (key, scan) => scanAdded  (key, scan)
              case Proc.ScanRemoved(key, scan) => scanRemoved(key, scan)
              case Proc.ScanChange (key, scan, sCh) => // handles by AuralScan
            }
          case _ =>
        }
      }
    }

    def nodeOption(implicit tx: S#Tx): Option[NodeRef] = nodeRef.get(tx.peer)

    private def playScans(n: NodeRef)(implicit tx: S#Tx): Unit =
      scanViews.foreach { case (_, view) =>
        view.play(n)
      } (tx.peer)

    private def stopScans()(implicit tx: S#Tx): Unit =
      scanViews.foreach { case (_, view) =>
        view.stop()
      } (tx.peer)

    private def newSynthGraph(g: SynthGraph)(implicit tx: S#Tx): Unit = {
      logA(s"--todo-- GraphChange ${procCached()}")
    }

    private def scanAdded(key: String, scan: Scan[S])(implicit tx: S#Tx): Unit = {
      logA(s"ScanAdded  to   ${procCached()} ($key)")
      val st = state
      st.scanIns.get(key).foreach { in =>
        val newCh = scanInNumChannels(scan)
        if (newCh != in.numChannels) {
          logA(s"...numChannels is $newCh but graph currently assumes ${in.numChannels}")
          rebuild()
        }
      }
    }

    private def scanRemoved(key: String, scan: Scan[S])(implicit tx: S#Tx): Unit = {
      logA(s"ScanRemoved from ${procCached()} ($key)")
    }

    private def rebuild()(implicit tx: S#Tx): Unit = {
      logA(s"--todo-- rebuild ${procCached()}")
    }

    def addInstanceNode(n: NodeRef)(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      nodeRef().fold {
        val groupImpl = new GroupImpl(n)
        nodeRef() = Some(groupImpl)
        playScans(groupImpl)

      } { groupImpl =>
        groupImpl.addInstanceNode(n)
      }
    }

    def removeInstanceNode(n: NodeRef)(implicit tx: S#Tx): Unit = {
      val groupImpl = nodeRef.get(tx.peer).getOrElse(sys.error(s"Removing unregistered AuralProc node instance $n"))
      if (groupImpl.removeInstanceNode(n)) {
        nodeRef.set(None)(tx.peer)
        stopScans()
      }
    }

    //    def addView(view: AuralObj.Proc[S])(implicit tx: S#Tx): Unit = ...
    //    def removeView(view: AuralObj.Proc[S])(implicit tx: S#Tx): Unit = ...

    def dispose()(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      procObserver.dispose()
      scanViews.foreach { case (_, view) => view.dispose() }
      scanViews.retain((_, _) => false) // no `clear` method
    }

    def state(implicit tx: S#Tx) = stateRef.get(tx.peer)

    def tryBuild()(implicit tx: S#Tx): Unit = {
      state match {
        case s0: Incomplete[S] =>
          logA(s"try build ${procCached()}")
          val s1 = s0.retry(this)
          stateRef.set(s1)(tx.peer)
          buildAdvanced(before = s0, now = s1)

        case s0: Complete[S] => // nada
      }
    }

    private def buildAdvanced(before: UState[S], now: UState[S])(implicit tx: S#Tx): Unit = {
      if (before.scanOuts eq now.scanOuts) return

      implicit val itx = tx.peer

      // detect which new scan outputs have been determined in the last iteration
      // (newOuts is a map from `name: String` to `numChannels Int`)
      val newOuts = now.scanOuts.filterNot {
        case (key, _) => before.scanOuts.contains(key)
      }

      logA(s"buildAdvanced ${procCached()}; newOuts = ${newOuts.mkString(",")}")

      val scans = procCached().elem.peer.scans
      newOuts.foreach { case (key, numCh) =>
        scanViews.get(key).fold {
          scans.get(key).foreach { scan =>
            val view = AuralScan(data = this, key = key, scan = scan, numChannels = numCh)
            scanViews.put(key, view)
          }
        } { view =>
          val numCh1 = view.bus.numChannels
          if (numCh1 != numCh) sys.error(s"Trying to access scan with competing numChannels ($numCh1, $numCh)")
        }
      }
    }

    def scanInBusChanged(sinkKey: String, bus: AudioBus)(implicit tx: S#Tx): Unit =
      state match {
        case in: Incomplete[S] if in.missingIns.contains(sinkKey) => tryBuild()
        case _ => // XXX TODO: if playing
      }

    def getScanInBus (key: String)(implicit tx: S#Tx): Option[AudioBus] = scanViews.get(key)(tx.peer).map(_.bus)

    // def getScanOutBus(key: String)(implicit tx: S#Tx): Option[AudioBus] = ...

    def procCached()(implicit tx: S#Tx): Obj.T[S, Proc.Elem] = {
      implicit val itx = tx.peer
      if (procLoc.isInitialized) procLoc.get
      else {
        val proc = obj()
        procLoc.set(proc)
        proc
      }
    }

    private def scanView(scan: Scan[S])(implicit tx: S#Tx): Option[AuralScan[S]] =
      context.getAux[AuralScan[S]](scan.id)

    // called by UGenGraphBuilderImpl
    def attrNumChannels(key: String)(implicit tx: S#Tx): Int = {
      val procObj = procCached()
      procObj.attr.getElem(key).fold(1) {
        case a: DoubleVecElem[S]     => a.peer.value.size // XXX TODO: would be better to write a.peer.size.value
        case a: AudioGraphemeElem[S] => a.peer.spec.numChannels
        case _ => 1
      }
    }
    // called by UGenGraphBuilderImpl
    def scanInNumChannels(key: String, numChannels: Int)(implicit tx: S#Tx): Int = {
      val procObj = procCached()
      val proc    = procObj.elem.peer
      val numCh   = proc.scans.get(key).fold(-1) { scan =>
        scanInNumChannels(scan)
      }
      if (numCh == -1) throw MissingIn(key) else numCh
    }

    private def scanInNumChannels(scan: Scan[S])(implicit tx: S#Tx): Int = {
      val chans = scan.sources.toList.map {
        case Link.Grapheme(peer) =>
          // val chansOpt = peer.valueAt(time).map(_.numChannels)
          // chansOpt.getOrElse(numChannels)
          peer.numChannels

        case Link.Scan(peer) =>
          // val sourceOpt = scanMap.get(peer.id)
          val sourceOpt = scanView(peer)
          val chansOpt = sourceOpt.map { sourceView =>
            // val sourceObj = sourceObjH()
            // getOutputBus(sourceObj, sourceKey)
            sourceView.bus.numChannels // data.state.scanOuts.get(sourceView.key)
          }
          chansOpt.getOrElse(-1)
      }
      if (chans.isEmpty) -1 else chans.max
    }
  }
}
