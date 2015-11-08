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
import de.sciss.lucre.synth.{Sys => SSys, NodeRef, AudioBus}
import de.sciss.synth.ControlSet
import impl.{AuralAttributeImpl => Impl}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.{implicitConversions, higherKinds}

object AuralAttribute {
  trait Factory {
    def typeID: Int

    type Repr[~ <: Sys[~]] <: Obj[~]

    def apply[S <: SSys[S]](value: Repr[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S]
  }

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  def apply[S <: SSys[S]](value: Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] = Impl(value)

  // ---- Target ----

  object Target {
    def apply[S <: Sys[S]](): Target[S] = ??? // new impl.AuralAttributeTargetImpl[S]
  }
  trait Target[S <: Sys[S]] {
    def put(source: AuralAttribute[S], value: Value)(implicit tx: S#Tx): Unit

    def remove(source: AuralAttribute[S])(implicit tx: S#Tx): Unit
  }

  // ---- Value ----

  object Value {
    implicit def fromFloat (x : Float     ): ScalarValue  = new ScalarValue (x )
    implicit def fromFloats(xs: Vec[Float]): ScalarVector = new ScalarVector(xs)
  }
  sealed trait Value
  sealed trait Scalar extends Value {
    def toControl(key: String): ControlSet
  }

  final case class ScalarValue (x : Float     ) extends Scalar {
    def toControl(key: String): ControlSet = ControlSet.Value(key, x)
  }
  final case class ScalarVector(xs: Vec[Float]) extends Scalar {
    def toControl(key: String): ControlSet = ControlSet.Vector(key, xs)
  }

  final case class Stream(source: NodeRef, bus: AudioBus) extends Value
}
trait AuralAttribute[S <: Sys[S]] extends Disposable[S#Tx] {
  def preferredNumChannels(implicit tx: S#Tx): Int

  def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit
  def play   (timeRef: TimeRef, target: AuralAttribute.Target[S], numChannels: Int)(implicit tx: S#Tx): Unit

  // def stop   ()(implicit tx: S#Tx): Unit
}