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
import de.sciss.lucre.synth.Sys
import impl.{AuralContextImpl => Impl}

object AuralContext {
  def apply[S <: Sys[S]](implicit tx: S#Tx): AuralContext[S] = Impl[S]
}
trait AuralContext[S <: Sys[S]] {
  def acquire[A <: Disposable[S#Tx]](obj: Obj[S])(init: => A)(implicit tx: S#Tx): A

  def release(obj: Obj[S])(implicit tx: S#Tx): Unit

  def put[A](id: S#ID, value: A)(implicit tx: S#Tx): Unit
  def get[A](id: S#ID)(implicit tx: S#Tx): Option[A]
}
