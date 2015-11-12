/*
 *  AuralEnsembleImpl.scala
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

package de.sciss.synth.proc
package impl

import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.Sys
import de.sciss.model.Change

import scala.concurrent.stm.Ref

object AuralEnsembleImpl {
  def apply[S <: Sys[S]](obj: Ensemble[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Ensemble[S] = {
    val transport = Transport[S]
    val ensemble  = obj
    ensemble.folder.iterator.foreach(transport.addObject)
    new Impl(tx.newHandle(obj), transport).init(ensemble)
  }
  
  private final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Ensemble[S]], transport: Transport[S])
    extends AuralObj.Ensemble[S] with ObservableImpl[S, AuralView.State] {
    
    def typeID = Ensemble.typeID

    private var observer: Disposable[S#Tx] = _

    private val currentStateRef = Ref[AuralView.State](AuralView.Stopped)

    def init(ens: Ensemble[S])(implicit tx: S#Tx): this.type = {
      observer = ens.changed.react { implicit tx => upd =>
        val ens = upd.ensemble
        upd.changes.foreach {
          case Ensemble.Folder (fUpd) =>
            fUpd.changes.foreach {
              case Folder.Added  (idx, elem) => transport.addObject   (elem)
              case Folder.Removed(idx, elem) => transport.removeObject(elem)
              case _ =>
            }

          // case Ensemble.Offset (Change(_, newOffset )) =>
          case Ensemble.Playing(Change(_, newPlaying)) =>
            logTransport(s"AuralEnsemble - new playing.value = $newPlaying")
            if (newPlaying) {
              if (state == AuralView.Playing) startTransport(ens)
            } else {
              transport.stop()
            }
          case _ =>
        }
      }
      this
    }

    def views(implicit tx: S#Tx): Set[AuralObj[S]] = transport.views

    def stop()(implicit tx: S#Tx): Unit = {
      transport.stop()
      state = AuralView.Stopped
    }

    def state(implicit tx: S#Tx): AuralView.State = currentStateRef.get(tx.peer)

    private def state_=(value: AuralView.State)(implicit tx: S#Tx): Unit = {
      val old = currentStateRef.swap(value)(tx.peer)
      if (value != old) {
        // println(s"------ENSEMBLE STATE $old > $value")
        fire(value)
      }
    }

    private def ensemble(implicit tx: S#Tx): Ensemble[S] = obj()

    private def startTransport(ens: Ensemble[S])(implicit tx: S#Tx): Unit = {
      transport.stop()
      transport.seek(ens.offset.value)  // XXX TODO -- should we incorporate timeRef.frame) ?
      transport.play()                  // XXX TODO -- should we be able to pass the timeRef?
    }

    def play(timeRef: TimeRef, unit: Unit)(implicit tx: S#Tx): Unit = {
      if (state == AuralView.Playing) return

      val ens = ensemble
      val p   = ens.playing.value
      logTransport(s"AuralEnsemble.play() - playing.value = $p")

      if (p) startTransport(ens)
      state = AuralView.Playing
    }

    def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      if (state != AuralView.Stopped) return
      Console.err.println("TODO: AuralEnsemble.prepare") // XXX TODO
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      observer .dispose()
      transport.dispose()
    }
  }
}
