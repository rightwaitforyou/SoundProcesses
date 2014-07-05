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
import de.sciss.lucre.synth.{Bus, AudioBus, Sys}
import de.sciss.model.Change
import de.sciss.synth.proc.AuralObj.ProcData
import de.sciss.synth.proc.Scan.Link
import de.sciss.synth.proc.UGenGraphBuilder.MissingIn
import UGenGraphBuilder.{State => UState}

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

  private final class Impl[S <: Sys[S]](val obj: ObjSource[S], state0: UState[S])
                                           (implicit context: AuralContext[S])
    extends ProcData[S] {

    import context.server

    private val procLoc  = TxnLocal[Obj.T[S, Proc.Elem]]()
    private val stateRef = Ref[UState[S]](state0)

    // def server: Server = context.server

    private var procObserver: Disposable[S#Tx] = _

    def init(proc: Obj.T[S, Proc.Elem])(implicit tx: S#Tx): Unit = {
      procObserver = proc.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case Obj.ElemChange(Proc.Update(_, pCh)) =>
            pCh.foreach {
              case Proc.GraphChange(Change(_, newGraph)) =>
              case Proc.ScanAdded  (key, scan) =>
              case Proc.ScanRemoved(key, scan) =>
              case Proc.ScanChange (key, scan, sCh) =>

            }
          case _ =>
        }
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      procObserver.dispose()
    }

    def state(implicit tx: S#Tx) = stateRef.get(tx.peer)

    def tryBuild()(implicit tx: S#Tx): Unit = {
      state match {
        case s0: UGenGraphBuilder.Incomplete[S] =>
          val s1 = s0.retry(this)
          stateRef.set(s1)(tx.peer)
          buildAdvanced(before = s0, now = s1)

        case s0: UGenGraphBuilder.Complete[S] => // nada
      }
    }

    private val outputBuses = TMap.empty[String, AudioBus]

    private def buildAdvanced(before: UState[S], now: UState[S])(implicit tx: S#Tx): Unit = {
      if (before.scanOuts eq now.scanOuts) return

      // detect which new scan outputs have been determined in the last iteration
      // (newOuts is a map from `name: String` to `numChannels Int`)
      val newOuts = now.scanOuts.filterNot {
        case (key, _) => before.scanOuts.contains(key)
      }

      val newBuses = newOuts.map { case (key, numCh) =>
        val bus = Bus.audio(server, numCh)
        outputBuses.put(key, bus)(tx.peer)
        (key, bus)
      }

      val scans = procCached().elem.peer.scans
      newBuses.foreach { case (key, bus) =>
        scans.get(key).foreach { scan =>
          scan.sinks.foreach {
            case Link.Scan(peer) =>
              scanView(peer).foreach { case (sinkKey, sinkData) =>
                sinkData.scanInBusChanged(sinkKey, bus)
              }
            case _ =>
          }
        }
      }
    }



    //    private def processStateChange(before: UState[S], now: UState[S], incremental: Boolean)(implicit tx: S#Tx): Unit = {


    //    private def processStateChange(before: UState[S], now: UState[S], incremental: Boolean)(implicit tx: S#Tx): Unit = {
    //      // attributeIns, scanIns, scanOuts, streamIns
    //      // `ne` is cheap. try that before calculating the diffs
    //
    //      //      val scanInsCh = mapEntryChanges(before.scanIns, now.scanIns, incremental = incremental)
    //      //      if (scanInsCh.nonEmpty) scanInsCh.foreach {
    //      //        case MapEntryAdded(key, scan)
    //      //      }
    //      val scanOutsCh = mapEntryChanges(before.scanOuts, now.scanOuts, incremental = incremental)
    //
    //
    //      ...
    //      val newBuses = newOuts.mapValuesX(numCh => new OutputBuilder(Bus.audio(server, numCh)))
    //    }

    def scanInBusChanged(sinkKey: String, bus: AudioBus)(implicit tx: S#Tx): Unit =
      state match {
        case in: UGenGraphBuilder.Incomplete[S] if in.missingIns.contains(sinkKey) => tryBuild()
        case _ => // XXX TODO: if playing
      }

    def procCached()(implicit tx: S#Tx): Obj.T[S, Proc.Elem] = {
      implicit val itx = tx.peer
      if (procLoc.isInitialized) procLoc.get
      else {
        val proc = obj()
        procLoc.set(proc)
        proc
      }
    }

    private def scanView(scan: Scan[S])(implicit tx: S#Tx): Option[(String, ProcData[S])] =
      context.getAux[(String, ProcData[S])](scan.id)

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
      val numCh   = proc.scans.get(key).fold(0) { scan =>
        val chans = scan.sources.toList.map {
          case Link.Grapheme(peer) =>
            // val chansOpt = peer.valueAt(time).map(_.numChannels)
            // chansOpt.getOrElse(numChannels)
            peer.numChannels

          case Link.Scan(peer) =>
            // val sourceOpt = scanMap.get(peer.id)
            val sourceOpt = scanView(peer)
            val chansOpt = sourceOpt.flatMap {
              case (sourceKey, sourceData) =>
                // val sourceObj = sourceObjH()
                // getOutputBus(sourceObj, sourceKey)
                sourceData.state.scanOuts.get(sourceKey)
            }
            chansOpt.getOrElse(throw MissingIn(key))
        }
        if (chans.isEmpty) 0 else chans.max
      }
      math.max(1, numCh)
    }

    //    private def getOutputBus(obj0: Obj.T[S, Proc.Elem], key: String)(implicit tx: S#Tx): Option[AudioBus] =
    //      context.get(obj0) match {
    //        case Some(data0) =>
    //          ... // data0.getOutputBus(key)
    //        case _ =>
    //          ...
    //          //          assert(ongoingBuild.isInitialized(tx.peer))
    //          //          val ob = ongoingBuild.get(tx.peer)
    //          //          for {
    //          //            map <- ob.idMap
    //          //            pb  <- map.get(timedID)
    //          //            out <- pb.outputs.get(key)
    //          //          } yield out.bus
    //      }
  }
}