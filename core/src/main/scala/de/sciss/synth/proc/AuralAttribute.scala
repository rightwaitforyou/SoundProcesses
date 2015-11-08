/*
 *  AuralAttribute.scala
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

import de.sciss.lucre.stm.{Obj, Disposable, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import impl.{AuralAttributeImpl => Impl}

import scala.language.higherKinds

object AuralAttribute {
  trait Factory {
    def typeID: Int

    type Repr[~ <: Sys[~]] <: Obj[~]

    def apply[S <: SSys[S]](value: Repr[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S]
  }

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  def apply[S <: SSys[S]](value: Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] = Impl(value)
}
trait AuralAttribute[S <: Sys[S]] extends Disposable[S#Tx] {
  def preferredNumChannels(implicit tx: S#Tx): Int

  def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit
  def play   (timeRef: TimeRef, builder: AuralAttributeTarget[S], numChannels: Int)(implicit tx: S#Tx): Unit

  // def stop   ()(implicit tx: S#Tx): Unit
}
