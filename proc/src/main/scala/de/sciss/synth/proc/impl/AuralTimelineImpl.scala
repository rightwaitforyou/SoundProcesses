/*
 *  AuralTimelineImpl.scala
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

object AuralTimelineImpl {
  def apply[S <: Sys[S]](proc: Timeline.Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline[S] = {
    ???
  }

  private final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Timeline.Obj[S]])
    extends AuralObj.Timeline[S] with ObservableImpl[S, AuralObj.State] {

    def typeID: Int = Timeline.typeID

    def stop()(implicit tx: S#Tx): Unit = ???

    def state(implicit tx: S#Tx): AuralObj.State = ???

    def play()(implicit tx: S#Tx): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = ???
  }
}
