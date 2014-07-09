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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.{Server, Sys}
import impl.{AuralContextImpl => Impl}

object AuralContext {
  def apply[S <: Sys[S]](server: Server, scheduler: Scheduler[S])(implicit tx: S#Tx): AuralContext[S] =
    Impl(server, scheduler)

  def apply[S <: Sys[S]](server: Server)(implicit tx: S#Tx, cursor: stm.Cursor[S]): AuralContext[S] = {
    val sched = Scheduler[S]
    apply(server, sched)
  }
}
trait AuralContext[S <: Sys[S]] {
  def server: Server

  def acquire[A <: Disposable[S#Tx]](obj: Obj[S])(init: => A)(implicit tx: S#Tx): A

  def release(obj: Obj[S])(implicit tx: S#Tx): Unit

  def get[A](obj: Obj[S])(implicit tx: S#Tx): Option[A]

  def putAux[A](id: S#ID, value: A)(implicit tx: S#Tx): Unit

  def getAux[A](id: S#ID)(implicit tx: S#Tx): Option[A]

  def scheduler: Scheduler[S]
}
