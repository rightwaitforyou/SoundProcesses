/*
 *  Attribute.scala
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
package graph

import de.sciss.synth.proc.UGenGraphBuilder.Input
import de.sciss.synth.ugen.ControlProxy

import scala.collection.immutable.{IndexedSeq => Vec}

object Attribute {
  private[proc] def controlName(key: String): String = "$attr_"  + key

  private final case class In(rate: Rate, key: String, default: Double) extends GE.Lazy {

    override def productPrefix  = "Attribute$In"
    override def toString       = s"""Attribute("$key").${rate.methodName}($default)"""

    def makeUGens: UGenInLike = {
      val b       = UGenGraphBuilder.get
      val numCh   = b.requestInput(Input.Attribute(key, numChannels = -1)).value
      // val numCh   = b.addAttributeIn(key)
      val ctlName = controlName(key)
      val ctl     = ControlProxy(rate, Vec.fill(numCh)(default.toFloat), Some(ctlName))
      // val ctl     = if (numCh == 1) ctlName.ir(default) else ctlName.ir(Vec.fill(numCh)(default))
      ctl.expand
    }
  }
}
final case class Attribute(key: String) {
  def ir: GE = ir(0.0)
  def ir(default: Double): GE = Attribute.In(scalar , key, default)

  def kr: GE = kr(0.0)
  def kr(default: Double): GE = Attribute.In(control, key, default)

  def ar: GE = ar(0.0)
  def ar(default: Double): GE = Attribute.In(audio  , key, default)
}