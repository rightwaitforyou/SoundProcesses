/*
 *  AuralAttribute.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.synth.{AudioBus, NodeRef, Sys => SSys}
import de.sciss.synth.ControlSet
import de.sciss.synth.proc.impl.{AuralAttributeImpl => Impl}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.{higherKinds, implicitConversions}

object AuralAttribute {
  def apply[S <: SSys[S]](key: String, value: Obj[S], observer: Observer[S])
                         (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
    Impl(key, value, observer)

  // ---- Observer ----

  trait Observer[S <: Sys[S]] {
    def attrNumChannelsChanged(attr: AuralAttribute[S])(implicit tx: S#Tx): Unit
  }

  // ---- Factory ----

  trait Factory {
    def typeID: Int

    type Repr[~ <: Sys[~]] <: Obj[~]

    def apply[S <: SSys[S]](key: String, value: Repr[S], observer: Observer[S])
                           (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S]
  }

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  // ---- Target ----

  object Target {
    def apply[S <: SSys[S]](nodeRef: NodeRef.Full[S], key: String, targetBus: AudioBus)
                           (implicit tx: S#Tx): Target[S] = {
      val res = new impl.AuralAttributeTargetImpl[S](nodeRef, key, targetBus)
      // nodeRef.addUser(res)
      res
    }
  }
  trait Target[S <: Sys[S]] {
    def put   (attr: AuralAttribute[S], value: Value)(implicit tx: S#Tx): Unit
    def remove(attr: AuralAttribute[S]              )(implicit tx: S#Tx): Unit
  }

  // ---- Value ----

  object Value {
    implicit def fromFloat (value : Float     ): ScalarValue  = new ScalarValue (value )
    implicit def fromFloats(values: Vec[Float]): ScalarVector = new ScalarVector(values)
  }
  sealed trait Value { def isScalar: Boolean }
  /** Value for which a no synth is needed, but only a scalar value
    * that needs to be set on the target node.
    */
  sealed trait Scalar extends Value {
    def toControl(key: String, numChannels: Int): ControlSet
    def values: Vec[Float]
    final def isScalar = true
  }

  final case class ScalarValue(value: Float) extends Scalar {
    def toControl(key: String, numChannels: Int): ControlSet =
      if (numChannels == 1) ControlSet.Value (key, value)
      else                  ControlSet.Vector(key, Vector.fill(numChannels)(value))

    def values: Vec[Float] = Vector(value)
  }
  final case class ScalarVector(values: Vec[Float]) extends Scalar {
    def toControl(key: String, numChannels: Int): ControlSet = {
      val sz = values.size
      val xs = if (numChannels == sz) values else Vector.tabulate(numChannels)(i => values(i % sz))
      ControlSet.Vector(key, xs)
    }
  }

  /** Value for which a `Synth` is required that writes its signal to a bus,
    * and the bus is then somehow mapped to the target node's control.
    */
  final case class Stream(source: NodeRef, bus: AudioBus) extends Value {
    def isScalar = false
  }
}
trait AuralAttribute[S <: Sys[S]] extends AuralView[S, AuralAttribute.Target[S]] {
  def key: String

  def preferredNumChannels(implicit tx: S#Tx): Int
}