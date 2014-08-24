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
  private[proc] def controlName(key: String): String = s"$$attr_$key"

  def ir(key: String, default: Double = 0.0): Attribute = new Attribute(scalar , key, default)
  def kr(key: String, default: Double = 0.0): Attribute = new Attribute(control, key, default)
  def ar(key: String, default: Double = 0.0): Attribute = new Attribute(audio  , key, default)
}
final case class Attribute(rate: Rate, key: String, default: Double) extends GE.Lazy {
  def makeUGens: UGenInLike = {
    val b       = UGenGraphBuilder.get
    val numCh   = b.requestInput(Input.Attribute(key, numChannels = -1)).numChannels
    val ctlName = Attribute.controlName(key)
    val ctl     = ControlProxy(rate, Vec.fill(numCh)(default.toFloat), Some(ctlName))
    ctl.expand
  }
}