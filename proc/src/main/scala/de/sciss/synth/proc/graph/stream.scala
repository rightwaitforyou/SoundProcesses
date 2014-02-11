/*
 *  stream.scala
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

import de.sciss.synth
import de.sciss.synth.ugen.Constant

object stream {
  // must be a two channel signal - (buf-id, gain)
  private[proc] def controlName(key: String, idx: Int): String = s"$$str_${idx}_$key"

  sealed trait GE extends synth.GE.Lazy {
    protected def makeUGen(numChannels: Int, buf: synth.GE, gain: synth.GE): UGenInLike

    protected def key: String

    protected def maxSpeed: Double

    protected def interp: Int

    def makeUGens: UGenInLike =
      UGenGraph.builder match {
        case b: UGenGraphBuilder[_] =>
          val info          = UGenGraphBuilder.StreamIn(maxSpeed, interp)
          val (numCh, idx)  = b.addStreamIn(key, info)
          val ctlName       = controlName  (key, idx )
          val ctl           = ctlName.ir(Seq(0, 0))
          val buf           = ctl \ 0
          val gain          = ctl \ 1
          makeUGen(numCh, buf, gain)

        case _ => UGenGraphBuilder.outsideOfContext()
      }
  }
}
final case class stream(key: String)

// ---- ugen wrappers ----

object DiskIn {
  def ar(buf: stream, loop: synth.GE = 0): DiskIn = apply(audio, key = buf.key, loop = loop)
}
case class DiskIn(rate: Rate, key: String, loop: synth.GE) extends stream.GE /* with HasSideEffect */ with IsIndividual {
  protected def interp    = 1
  protected def maxSpeed  = 1.0

  protected def makeUGen(numChannels: Int, buf: synth.GE, gain: synth.GE): UGenInLike =
    ugen.DiskIn(rate, numChannels = numChannels, buf = buf, loop = loop) * gain
}

object VDiskIn {
  def ar(buf: stream, speed: synth.GE = 1, loop: synth.GE = 0, sendID: synth.GE = 0, interp: Int = 4,
         maxSpeed: Double = 0.0): VDiskIn = {
    // XXX TODO: match against UserValue ?
    val maxSpeed1 = speed match {
      case Constant(c)  => c
      case _            => maxSpeed
    }
    apply(audio, key = buf.key, speed = speed, loop = loop, sendID = sendID, interp = interp, maxSpeed = maxSpeed1)
  }
}
case class VDiskIn(rate: Rate, key: String, speed: synth.GE, loop: synth.GE, sendID: synth.GE, interp: Int,
                   maxSpeed: Double)
  extends stream.GE /* with HasSideEffect */ with IsIndividual {

  require(interp == 1 || interp == 2 || interp == 4, s"Unsupported interpolation: $interp")

  protected def makeUGen(numChannels: Int, buf: synth.GE, gain: synth.GE): UGenInLike = {
    val reader = if (interp == 4) {
      ugen.VDiskIn(rate, numChannels = numChannels, buf = buf, speed = speed, loop = loop, sendID = sendID)
    } else {
      ???
    }
    reader * gain
  }
}

object BufChannels {
  def ir(buf: stream): BufChannels = apply(scalar , key = buf.key)
  def kr(buf: stream): BufChannels = apply(control, key = buf.key)
}
case class BufChannels(rate: Rate, key: String) extends stream.GE {
  protected def interp    = 0
  protected def maxSpeed  = 0.0

  protected def makeUGen(numChannels: Int, buf: synth.GE, gain: synth.GE): UGenInLike =
    ugen.BufChannels(rate, buf) // or just Constant(numChannels), ha?
}

object BufRateScale {
  def ir(buf: stream): BufRateScale = apply(scalar , key = buf.key)
  def kr(buf: stream): BufRateScale = apply(control, key = buf.key)
}
case class BufRateScale(rate: Rate, key: String) extends stream.GE {
  protected def interp    = 0
  protected def maxSpeed  = 0.0

  protected def makeUGen(numChannels: Int, buf: synth.GE, gain: synth.GE): UGenInLike =
    ugen.BufRateScale(rate, buf)
}

object BufSampleRate {
  def ir(buf: stream): BufSampleRate = apply(scalar , key = buf.key)
  def kr(buf: stream): BufSampleRate = apply(control, key = buf.key)
}
case class BufSampleRate(rate: Rate, key: String) extends stream.GE {
  protected def interp    = 0
  protected def maxSpeed  = 0.0

  protected def makeUGen(numChannels: Int, buf: synth.GE, gain: synth.GE): UGenInLike =
    ugen.BufSampleRate(rate, buf)
}