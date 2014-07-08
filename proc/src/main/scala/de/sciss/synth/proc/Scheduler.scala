/*
 *  Scheduler.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.synth.Sys
import impl.{SchedulerImpl => Impl}

object Scheduler {
  def apply[S <: Sys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S]): Scheduler[S] = Impl[S]
}
trait Scheduler[S <: Sys[S]] {
  /** Logical time frame based on `Timeline.SampleRate` and with zero
    * corresponding to creation time. Frames elapsed with wall-clock
    * but are stable within a transaction.
    */
  def time(implicit tx: S#Tx): Long

  /** Opaque type to identify scheduled functions. */
  type Token

  /** Schedules the execution of a function at a given time. Time is given
    * as an "absolute" frame in the sense of `AuralContext.time`.
    * Returns a token that can be used to cancel the action.
    */
  def schedule(time: Long)(fun: S#Tx => Unit)(implicit tx: S#Tx): Token

  /** Cancels a scheduled action. */
  def cancel(token: Token)(implicit tx: S#Tx): Unit
}