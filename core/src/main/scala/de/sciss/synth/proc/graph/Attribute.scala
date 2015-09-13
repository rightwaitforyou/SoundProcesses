/*
 *  Attribute.scala
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

package de.sciss.synth
package proc
package graph

import de.sciss.synth.proc.UGenGraphBuilder.Input
import de.sciss.synth.ugen.{AudioControlProxy, ControlProxy}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

object Attribute {
  object Default {
    implicit def scalar(in:     Double ): Default = Scalar(in)
    implicit def vector(in: Vec[Double]): Default = Vector(in)
  }
  /** Magnet pattern */
  sealed trait Default extends Product {
    def numChannels: Int
    def tabulate(n: Int): Vec[Float]
  }
  final case class Scalar(value: Double) extends Default {
    def numChannels = 1

    def tabulate(n: Int): Vec[Float] = scala.Vector.fill(n)(value.toFloat)

    // serialization!
    override def productPrefix: String = "Attribute$Scalar"
  }

  final case class Vector(values: Vec[Double]) extends Default {
    def numChannels: Int = values.size

    def tabulate(n: Int): Vec[Float] = scala.Vector.tabulate(n)(idx => values(idx % values.size).toFloat)

    // serialization!
    override def productPrefix: String = "Attribute$Vector"
  }

  /* private[proc] */ def controlName(key: String): String = s"$$at_$key"

  def ir(key: String, default: Default = 0.0): Attribute = new Attribute(scalar , key, default)
  def kr(key: String, default: Default = 0.0): Attribute = new Attribute(control, key, default)
  def ar(key: String, default: Default = 0.0): Attribute = new Attribute(audio  , key, default)
}
final case class Attribute(rate: Rate, key: String, default: Attribute.Default) extends GE.Lazy {
  def makeUGens: UGenInLike = {
    val b       = UGenGraphBuilder.get
    val numCh   = b.requestInput(Input.Attribute(key, numChannels = -1)).numChannels
    val ctlName = Attribute.controlName(key)
    val values  = default.tabulate(numCh)
    val nameOpt = Some(ctlName)
    val ctl     = if (rate == audio)
      AudioControlProxy(values, nameOpt)
    else
      ControlProxy(rate, values, nameOpt)
    ctl.expand
  }
}