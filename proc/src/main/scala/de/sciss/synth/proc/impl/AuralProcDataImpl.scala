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
import de.sciss.lucre.synth.{Bus, NodeGraph, Node, Txn, Group, NodeRef, AudioBus, Sys}
import de.sciss.model.Change
import de.sciss.synth.{SynthGraph, addBefore}
import de.sciss.synth.proc.AuralObj.ProcData
import de.sciss.synth.proc.Scan.Link
import de.sciss.synth.proc.UGenGraphBuilder.{State => UState, Complete, Incomplete, MissingIn}
import de.sciss.synth.proc.{logAural => logA}

import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{TSet, TMap, Ref, TxnLocal}

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

  private def GroupImpl(name: String, in0: NodeRef)(implicit tx: Txn): GroupImpl = {
    val res = new GroupImpl(name, in0)
    NodeGraph.addNode(res)
    res
  }

  // dynamically flips between single proc and multiple procs
  // (wrapping them in one common group)
  private final class GroupImpl(name: String, in0: NodeRef) extends NodeRef {
    val server = in0.server

    override def toString = name

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

        case Nil  =>
          dispose()
          true

        case _ => false
      }
    }

    def dispose()(implicit tx: Txn): Unit = {
      implicit val itx = tx.peer
      if (instancesRef.swap(Nil).size > 1) {
        val group = nodeRef.swap(null).node
        group.free(audible = false)
      }
      NodeGraph.removeNode(this)
    }
  }

  private final class Impl[S <: Sys[S]](val obj: ObjSource[S], state0: UState[S])
                                       (implicit context: AuralContext[S])
    extends ProcData[S] {

    private val stateRef  = Ref[UState[S]](state0)
    private val nodeRef   = Ref(Option.empty[GroupImpl])
    private val scanBuses = TMap.empty[String, AudioBus]
    private val scanViews = TMap.empty[String, AuralScan.Owned[S]]
    private val procViews = TSet.empty[AuralObj.Proc[S]]

    private val procLoc   = TxnLocal[Obj.T[S, Proc.Elem]]() // cache-only purpose

    private var procObserver: Disposable[S#Tx] = _

    def init(proc: Obj.T[S, Proc.Elem])(implicit tx: S#Tx): Unit = {
      procObserver = proc.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case Obj.ElemChange(Proc.Update(_, pCh)) =>
            pCh.foreach {
              case Proc.GraphChange(Change(_, newGraph)) => newSynthGraph(newGraph)
              case Proc.ScanAdded(key, scan) => scanAdded(key, scan)
              case Proc.ScanRemoved(key, scan) => scanRemoved(key, scan)
              case Proc.ScanChange(key, scan, sCh) => scanChange(key, scan, sCh)

            }
          case _ =>
        }
      }
    }

    def nodeOption(implicit tx: S#Tx): Option[NodeRef] = nodeRef.get(tx.peer)

    private def playScans(n: NodeRef)(implicit tx: S#Tx): Unit = {
      logA(s"playScans ${procCached()}")
      scanViews.foreach { case (_, view) =>
        view.play(n)
      }(tx.peer)
    }

    private def stopScans()(implicit tx: S#Tx): Unit = {
      logA(s"stopScans ${procCached()}")
      scanViews.foreach { case (_, view) =>
        view.stop()
      }(tx.peer)
    }

    private def newSynthGraph(g: SynthGraph)(implicit tx: S#Tx): Unit = {
      logA(s"--todo-- GraphChange ${procCached()}")
    }

    private def scanAdded(key: String, scan: Scan[S])(implicit tx: S#Tx): Unit = {
      logA(s"ScanAdded  to   ${procCached()} ($key)")
      testInScan (key, scan)
      testOutScan(key, scan)
    }

    // if a scan was added or a source was added to an existing scan,
    // check if the scan is used as currently missing input. if so,
    // try to build the ugen graph again.
    private def testInScan(key: String, scan: Scan[S])(implicit tx: S#Tx): Unit = {
      if (state.missingIns.contains(key)) {
        val numCh = scanInNumChannels(scan)
        // println(s"testInScan($key) -> numCh = $numCh")
        if (numCh >= 0) {
          // the scan is ready to be used and was missing before
          tryBuild()
        }
      }
    }

    private def testOutScan(key: String, scan: Scan[S])(implicit tx: S#Tx): Unit = {
      state.scanOuts.get(key).foreach { numCh =>
        scanViews.get(key)(tx.peer).fold[Unit] {
          mkAuralScan(key, scan, numCh)
        } { view =>
          checkScanNumChannels(view, numCh)
        }
      }
    }

    private def scanRemoved(key: String, scan: Scan[S])(implicit tx: S#Tx): Unit = {
      logA(s"ScanRemoved from ${procCached()} ($key)")
    }

    private def scanChange(key: String, scan: Scan[S], changes: Vec[Scan.Change[S]])(implicit tx: S#Tx): Unit = {
      logA(s"ScanChange in   ${procCached()} ($key)")
      changes.foreach {
        case Scan.SourceAdded(_) =>
          testInScan(key, scan)
        case _ =>
      }
    }

    //    private def rebuild()(implicit tx: S#Tx): Unit = {
    //      logA(s"--todo-- rebuild ${procCached()}")
    //    }

    def addInstanceNode(n: NodeRef)(implicit tx: S#Tx): Unit = {
      logA(s"addInstanceNode ${procCached()} : $n")
      implicit val itx = tx.peer
      nodeRef().fold {
        val groupImpl = GroupImpl(name = s"Group-NodeRef ${procCached()}", in0 = n)
        nodeRef() = Some(groupImpl)
        playScans(groupImpl)

      } { groupImpl =>
        groupImpl.addInstanceNode(n)
      }
    }

    def removeInstanceNode(n: NodeRef)(implicit tx: S#Tx): Unit = {
      logA(s"removeInstanceNode ${procCached()} : $n")
      val groupImpl = nodeRef.get(tx.peer).getOrElse(sys.error(s"Removing unregistered AuralProc node instance $n"))
      if (groupImpl.removeInstanceNode(n)) {
        nodeRef.set(None)(tx.peer)
        stopScans()
      }
    }

    def addInstanceView   (view: AuralObj.Proc[S])(implicit tx: S#Tx): Unit = procViews.add   (view)(tx.peer)
    def removeInstanceView(view: AuralObj.Proc[S])(implicit tx: S#Tx): Unit = procViews.remove(view)(tx.peer)

    def dispose()(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      nodeRef.swap(None).foreach(_.dispose())
      procObserver.dispose()
      scanViews.foreach { case (_, view) => view.dispose()}
      clearMap(scanViews)
      clearMap(scanBuses)
    }

    private def clearMap[A, B](m: TMap[A, B])(implicit tx: S#Tx): Unit =
      m.retain((_, _) => false)(tx.peer) // no `clear` method

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
      implicit val itx = tx.peer

      if (now.missingIns.isEmpty) {
        logA(s"buildAdvanced ${procCached()}; complete? ${now.isComplete}")
      } else {
        logA(s"buildAdvanced ${procCached()}; missingIns = ${now.missingIns.mkString(",")}")
      }

      // handle newly visible outputs
      if (before.scanOuts ne now.scanOuts) {
        // detect which new scan outputs have been determined in the last iteration
        // (newOuts is a map from `name: String` to `numChannels Int`)
        val newOuts = now.scanOuts.filterNot {
          case (key, _) => before.scanOuts.contains(key)
        }
        logA(s"...newOuts = ${newOuts.mkString(",")}")

        newOuts.foreach { case (key, numCh) =>
          activateAuralScan(key, numCh)
        }
      }

      // handle newly visible inputs
      if (before.scanIns ne now.scanIns) {
        val newIns = now.scanIns.filterNot {
          case (key, _) => before.scanIns.contains(key)
        }
        logA(s"...newIns  = ${newIns.mkString(",")}")

        newIns.foreach { case (key, meta) =>
          val numCh = meta.numChannels
          activateAuralScan(key, numCh)
        }
      }

      now match {
        case c: Complete[S] =>
          procViews.foreach { view =>
            if (view.targetState == AuralObj.Playing) {
              // ugen graph became ready and view wishes to play.
              view.play()
            }
          }
        case _ =>
      }
    }

    /* Ensures that an aural-scan for a given key exists. If it exists,
     * checks that the number of channels is correct. Otherwise, checks
     * if a scan for the key exists. If yes, instantiates the aural-scan,
     * if no, creates an audio-bus for later use.
     */
    private def activateAuralScan(key: String, numChannels: Int)(implicit tx: S#Tx): Unit = {
      scanViews.get(key)(tx.peer).fold {
        val scans = procCached().elem.peer.scans
        scans.get(key).fold[Unit] {
          mkBus(key, numChannels)
        } { scan =>
          mkAuralScan(key, scan, numChannels)
        }
      } { view =>
        checkScanNumChannels(view, numChannels)
      }
    }

    private def mkBus(key: String, numChannels: Int)(implicit tx: S#Tx): AudioBus = {
      implicit val itx = tx.peer
      val bus = scanBuses.get(key).getOrElse {
        val res = Bus.audio(context.server, numChannels = numChannels)
        scanBuses.put(key, res)
        res
      }
      if (bus.numChannels != numChannels)
        sys.error(s"Scan bus channels changed from ${bus.numChannels} to $numChannels")

      bus
    }

    /* Creates a new aural scan */
    private def mkAuralScan(key: String, scan: Scan[S], numChannels: Int)(implicit tx: S#Tx): AuralScan[S] = {
      val bus   = mkBus(key, numChannels)
      val view  = AuralScan(data = this, key = key, scan = scan, bus = bus)
      scanViews.put(key, view)(tx.peer)
      // note: the view will iterate over the
      //       sources and sink itself upon initialization,
      //       and establish the playing links if found
      //
      //      nodeOption.foreach { n =>
      //        view.play(n)
      //      }
      view
    }

    private def checkScanNumChannels(view: AuralScan[S], numCh: Int): Unit = {
      val numCh1 = view.bus.numChannels
      if (numCh1 != numCh) sys.error(s"Trying to access scan with competing numChannels ($numCh1, $numCh)")
    }

    def scanInBusChanged(sinkKey: String, bus: AudioBus)(implicit tx: S#Tx): Unit = {
      if (state.missingIns.contains(sinkKey)) tryBuild()
    }

    // def getScanBus(key: String)(implicit tx: S#Tx): Option[AudioBus] = scanViews.get(key)(tx.peer).map(_.bus)

    def getScanBus(key: String)(implicit tx: S#Tx): Option[AudioBus] = scanBuses.get(key)(tx.peer)

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
          val sourceOpt = scanView(peer)
          val chansOpt  = sourceOpt.map { sourceView =>
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
