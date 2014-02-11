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

object stream {
  // must be a two channel signal - (buf-id, gain)
  private[proc] def controlName(key: String): String = "$stream_"  + key

  trait GE extends synth.GE.Lazy with HasSideEffect with IsIndividual {
    protected def makeUGen(numChannels: Int, buf: synth.GE, gain: synth.GE): UGenInLike

    protected def key: String

    def makeUGens: UGenInLike =
      UGenGraph.builder match {
        case b: UGenGraphBuilder[_] =>
          val numCh   = b.addStreamIn(key)
          val ctlName = controlName  (key)
          val ctl     = ctlName.ir(Seq(0, 0))
          val buf     = ctl \ 0
          val gain    = ctl \ 1
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
case class DiskIn(rate: Rate, key: String, loop: synth.GE) extends stream.GE {
  // override def productPrefix  = "stream$DiskIn"

  protected def makeUGen(numChannels: Int, buf: synth.GE, gain: synth.GE): UGenInLike =
    ugen.DiskIn(rate, numChannels = numChannels, buf = buf, loop = loop) * gain
}

object VDiskIn {
  def ar(buf: stream, speed: synth.GE = 1, loop: synth.GE = 0, sendID: synth.GE = 0): VDiskIn =
    apply(audio, key = buf.key, speed = speed, loop = loop, sendID = sendID)
}
case class VDiskIn(rate: Rate, key: String, speed: synth.GE, loop: synth.GE, sendID: synth.GE) extends stream.GE {
  // override def productPrefix  = "stream$VDiskIn"

  protected def makeUGen(numChannels: Int, buf: synth.GE, gain: synth.GE): UGenInLike =
    ugen.VDiskIn(rate, numChannels = numChannels, buf = buf, speed = speed, loop = loop, sendID = sendID) * gain
}

object BufChannels {
  def ir(buf: stream): BufChannels = apply(scalar , key = buf.key)
  def kr(buf: stream): BufChannels = apply(control, key = buf.key)
}
case class BufChannels(rate: Rate, key: String) extends stream.GE {
  // override def productPrefix  = "stream$BufChannels"

  protected def makeUGen(numChannels: Int, buf: synth.GE, gain: synth.GE): UGenInLike =
    ugen.BufChannels(rate, buf) // or just Constant(numChannels), ha?
}

object BufRateScale {
  def ir(buf: stream): BufRateScale = apply(scalar , key = buf.key)
  def kr(buf: stream): BufRateScale = apply(control, key = buf.key)
}
case class BufRateScale(rate: Rate, key: String) extends stream.GE {
  // override def productPrefix  = "stream$BufRateScale"

  protected def makeUGen(numChannels: Int, buf: synth.GE, gain: synth.GE): UGenInLike =
    ugen.BufRateScale(rate, buf)
}

object BufSampleRate {
  def ir(buf: stream): BufSampleRate = apply(scalar , key = buf.key)
  def kr(buf: stream): BufSampleRate = apply(control, key = buf.key)
}
case class BufSampleRate(rate: Rate, key: String) extends stream.GE {
  // override def productPrefix  = "stream$BufSampleRate"

  protected def makeUGen(numChannels: Int, buf: synth.GE, gain: synth.GE): UGenInLike =
    ugen.BufSampleRate(rate, buf)
}