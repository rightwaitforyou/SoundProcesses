/*
 *  AuralEnsembleImpl.scala
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
import de.sciss.lucre.synth.Sys
import de.sciss.model.Change

object AuralEnsembleImpl {
  def apply[S <: Sys[S]](obj: Ensemble.Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Ensemble[S] = {
    val transport = Transport[S]
    new Impl(tx.newHandle(obj), transport)
  }
  
  private final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Ensemble.Obj[S]], transport: Transport[S])
    extends AuralObj.Ensemble[S] with ObservableImpl[S, AuralObj.State] {
    
    def typeID = Ensemble.typeID

    private var observer: Disposable[S#Tx] = _

    def init(ens: Ensemble[S])(implicit tx: S#Tx): Unit = {
      observer = ens.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case Ensemble.Folder (fUpd) =>
          case Ensemble.Offset (Change(_, newOffset)) =>
          case Ensemble.Playing(Change(_, newPlaying)) =>
        }
      }
    }

    def views(implicit tx: S#Tx): Set[AuralObj[S]] = transport.views

    def stop()(implicit tx: S#Tx): Unit = transport.stop()

    def state(implicit tx: S#Tx): AuralObj.State = ???

    def play(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      transport.stop()
      ??? // transport.position = timeRef.frame
      transport.play()
    }

    def prepare()(implicit tx: S#Tx): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = {
      observer .dispose()
      transport.dispose()
    }
  }
}
