/*
 *  stream.scala
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

package de.sciss.synth
package proc
package graph

import de.sciss.synth
import de.sciss.synth.proc.UGenGraphBuilder.Input
import synth.ugen.Constant
import de.sciss.synth.proc.impl.StreamBuffer
import impl.Stream

// ---- ugen wrappers ----

object DiskIn {
  def ar(key: String, loop: synth.GE = 0): DiskIn = apply(audio, key = key, loop = loop)
}
final case class DiskIn(rate: Rate, key: String, loop: synth.GE)
  extends Stream with IsIndividual {

  // protected def info = UGenGraphBuilder.StreamIn(1.0, -
  protected def maxSpeed  = 1.0
  protected def interp    = -1

  protected def makeUGen(numChannels: Int, idx: Int, buf: synth.GE, gain: synth.GE): UGenInLike =
    ugen.DiskIn(rate, numChannels = numChannels, buf = buf, loop = loop) * gain
}

object VDiskIn {
  def ar(key: String, speed: synth.GE = 1, loop: synth.GE = 0, interp: Int = 4, maxSpeed: Double = 0.0): VDiskIn = {
    // XXX TODO: match against UserValue ?
    val maxSpeed1 = speed match {
      case Constant(c)  => c
      case _            => maxSpeed
    }
    apply(audio, key = key, speed = speed, loop = loop, interp = interp, maxSpeed = maxSpeed1)
  }
}
final case class VDiskIn(rate: Rate, key: String, speed: synth.GE, loop: synth.GE, interp: Int, maxSpeed: Double)
  extends Stream with IsIndividual {

  if (interp != 1 && interp != 2 && interp != 4) sys.error(s"Unsupported interpolation: $interp")

  // VDiskIn uses cubic interpolation. Thus provide native streaming if that interpolation
  // is chosen; otherwise use the `StreamBuffer` functionality.
  // protected def info = UGenGraphBuilder.StreamIn(maxSpeed, if (interp == 4) -1 else interp)

  protected def makeUGen(numChannels: Int, idx: Int, buf: synth.GE, gain: synth.GE): UGenInLike = {
    val reader = if (interp == 4) {
      ugen.VDiskIn(rate, numChannels = numChannels, buf = buf, speed = speed, loop = loop)
    } else {
      StreamBuffer.makeUGen(key = key, idx = idx, buf = buf, numChannels = numChannels, speed = speed, interp = interp)
    }
    reader * gain
  }
}

object DiskOut {
  /* private[proc] */ def controlName(key: String): String = s"$$disk_$key"

  def ar(key: String, in: GE): DiskOut = apply(audio, key = key, in = in)
}

/** A graph element that creates a `DiskOut` writing to a file
  * designated by an object attribute with a given `key` and the
  * value being an `Artifact`.
  *
  * The file-type is determined by this artifact. For example, if the
  * artifact's path ends in `".aif"`, the AIFF format will used, if
  * the path ends in `".w64"`, then Wave64 will be used. The default is AIFF.
  * The sample format is currently always Float-32.
  *
  * @param key  the key into the enclosing object's attribute map, pointing to an `Artifact`
  * @param in   the signal to write
  */
final case class DiskOut(rate: Rate, key: String, in: GE)
  extends synth.GE.Lazy with WritesBuffer {

  protected def makeUGens: UGenInLike = {
    val b         = UGenGraphBuilder.get
    val ins       = in.expand.flatOutputs
    b.requestInput(Input.DiskOut(key, numChannels = ins.size))
    val ctlName   = DiskOut.controlName(key)
    val buf       = ctlName.ir
    ugen.DiskOut(rate, buf = buf, in = ins)
  }
}

object BufChannels {
  def ir(key: String): BufChannels = apply(scalar , key = key)
  def kr(key: String): BufChannels = apply(control, key = key)
}
final case class BufChannels(rate: Rate, key: String) extends Stream.Info {
  protected def makeUGen(numChannels: Int, idx: Int, buf: synth.GE, gain: synth.GE): UGenInLike =
    ugen.BufChannels(rate, buf) // or just Constant(numChannels), ha?
}

object BufRateScale {
  def ir(key: String): BufRateScale = apply(scalar , key = key)
  def kr(key: String): BufRateScale = apply(control, key = key)
}
final case class BufRateScale(rate: Rate, key: String) extends Stream.Info {
  protected def makeUGen(numChannels: Int, idx: Int, buf: synth.GE, gain: synth.GE): UGenInLike =
    ugen.BufRateScale(rate, buf)
}

object BufSampleRate {
  def ir(key: String): BufSampleRate = apply(scalar , key = key)
  def kr(key: String): BufSampleRate = apply(control, key = key)
}
final case class BufSampleRate(rate: Rate, key: String) extends Stream.Info {
  protected def makeUGen(numChannels: Int, idx: Int, buf: synth.GE, gain: synth.GE): UGenInLike =
    ugen.BufSampleRate(rate, buf)
}