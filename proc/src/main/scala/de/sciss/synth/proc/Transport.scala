/*
 *  Transport.scala
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

package de.sciss.synth
package proc

import de.sciss.lucre.stm
import de.sciss.lucre.synth.Sys
import stm.{Disposable, Cursor}
import de.sciss.lucre.event.Observable
import impl.{TransportImpl => Impl}

object Transport {
  def apply[S <: Sys[S]](aural: AuralSystem, scheduler: Scheduler[S])(implicit tx: S#Tx): Transport[S] =
    Impl(aural, scheduler)

  def apply[S <: Sys[S]](aural: AuralSystem)(implicit tx: S#Tx, cursor: Cursor[S]): Transport[S] = {
    val sched = Scheduler[S]
    apply(aural, sched)
  }

  sealed trait Update[S <: Sys[S]] {
    def transport: Transport[S]
    def position : Long
  }

  final case class Play[S <: Sys[S]](transport: Transport[S], position: Long) extends Update[S]
  final case class Stop[S <: Sys[S]](transport: Transport[S], position: Long) extends Update[S]
  final case class Seek[S <: Sys[S]](transport: Transport[S], position: Long, isPlaying: Boolean) extends Update[S]
}

/** New reduced definition of a t_(P) transport mechanism. */
trait Transport[S <: Sys[S]]
  extends Disposable[S#Tx] with Observable[S#Tx, Transport.Update[S]] {

  def play()(implicit tx: S#Tx): Unit
  def stop()(implicit tx: S#Tx): Unit

  def seek(position: Long)(implicit tx: S#Tx): Unit
  def position(implicit tx: S#Tx): Long

  def isPlaying(implicit tx: S#Tx): Boolean

  //  def addView   (view: AuralObj[S])(implicit tx: S#Tx): Unit
  //  def removeView(view: AuralObj[S])(implicit tx: S#Tx): Unit

  def addObject   (obj: Obj[S])(implicit tx: S#Tx): Unit
  def removeObject(obj: Obj[S])(implicit tx: S#Tx): Unit

  // not sure if the transport should generate the context or if use site should provide it?
  def contextOption(implicit tx: S#Tx): Option[AuralContext[S]]

  def scheduler: Scheduler[S]
}