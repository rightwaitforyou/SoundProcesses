/*
 *  AuralContext.scala
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

import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.{Server, Sys}
import impl.{AuralContextImpl => Impl}

object AuralContext {
  def apply[S <: Sys[S]](server: Server)(implicit tx: S#Tx): AuralContext[S] = Impl(server)
}
trait AuralContext[S <: Sys[S]] {
  def server: Server

  def acquire[A <: Disposable[S#Tx]](obj: Obj[S])(init: => A)(implicit tx: S#Tx): A

  def release(obj: Obj[S])(implicit tx: S#Tx): Unit

  def get[A](obj: Obj[S])(implicit tx: S#Tx): Option[A]

  def putAux[A](id: S#ID, value: A)(implicit tx: S#Tx): Unit
  def getAux[A](id: S#ID)(implicit tx: S#Tx): Option[A]

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
