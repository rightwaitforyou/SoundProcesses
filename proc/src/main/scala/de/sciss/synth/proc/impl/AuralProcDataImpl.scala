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
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.AuralObj.ProcData
import de.sciss.synth.proc.Scan.Link
import de.sciss.synth.proc.UGenGraphBuilder.MissingIn

import scala.concurrent.stm.{Ref, TxnLocal}

object AuralProcDataImpl {
  def apply[S <: Sys[S]](proc: Obj.T[S, Proc.Elem])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.ProcData[S] =
    context.acquire[AuralObj.ProcData[S]](proc) {
      val ugenInit = UGenGraphBuilder.init(proc)
      val data0 = new Impl(tx.newHandle(proc), ugenInit)
      data0.tryBuild()
      data0
    }

  private type ObjSource[S <: Sys[S]] = stm.Source[S#Tx, Obj.T[S, Proc.Elem]]

  private final class Impl[S <: Sys[S]](val obj: ObjSource[S], state0: UGenGraphBuilder.State[S])
                                           (implicit context: AuralContext[S])
    extends ProcData[S] {

    private val procLoc  = TxnLocal[Obj.T[S, Proc.Elem]]()
    private val stateRef = Ref[UGenGraphBuilder.State[S]](state0)

    // def server: Server = context.server

    def dispose()(implicit tx: S#Tx): Unit = {
      // nothing yet
    }

    def state(implicit tx: S#Tx) = stateRef.get(tx.peer)

    def tryBuild()(implicit tx: S#Tx): Unit = {
      state match {
        case s0: UGenGraphBuilder.Incomplete[S] =>
          val s1 = s0.retry(this)
          stateRef.set(s1)(tx.peer)

        case s0: UGenGraphBuilder.Complete[S] => // nada
      }
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
            val sourceOpt = context.getAux[(String, ProcData[S])](peer.id)
            val chansOpt = sourceOpt.flatMap {
              case (sourceKey, sourceData) =>
                // val sourceObj = sourceObjH()
                // getOutputBus(sourceObj, sourceKey)
                sourceData.state.scanOuts.get(sourceKey)
            }
            chansOpt.getOrElse(throw MissingIn(peer))
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
