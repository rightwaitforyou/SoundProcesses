/*
 *  AudioArtifactScalarWriter.scala
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
package impl

import de.sciss.lucre.synth.{ControlBus, DynamicBusUser, Buffer, Synth, Resource, Txn}
import de.sciss.synth.{addToHead, ControlSetMap, SynthGraph}

object AudioArtifactScalarWriter {
  def apply(bus: ControlBus, audioVal: Grapheme.Value.Audio)
           (implicit tx: Txn): AudioArtifactScalarWriter = {
    val numChannels = audioVal.spec.numChannels
    val sg  = SynthGraph {
      import de.sciss.synth._
      import ugen._
      val buf   = "buf".ir
      val out   = "out".kr
      val amp   = "amp".kr(1)
      val sig0  = BufRd.ir(numChannels, buf = buf, loop = 0, interp = 1)
      val sig   = sig0 * amp
      // Line.kr(start = 0, end = 0, dur = dur, doneAction = freeSelf)
      Out.ir(out, sig)
      FreeSelf.kr(Impulse.kr(0))  // note: doesn't work: `DC.kr(1)`
    }
    val synth = Synth(bus.server, sg, nameHint = Some("audio-artifact"))
    val res = new AudioArtifactScalarWriter(synth, bus, audioVal)
    res.britzelAdd()
    res
  }
}
final class AudioArtifactScalarWriter private (synth: Synth, val bus: ControlBus, audioVal: Grapheme.Value.Audio)
  extends DynamicBusUser with Resource.Source {

  def resource(implicit tx: Txn) = synth

  def server = synth.server

  def add()(implicit tx: Txn) = ()

  def britzelAdd()(implicit tx: Txn): Unit = {
    val file      = audioVal.artifact
    val path      = file.getAbsolutePath
    val target    = server.defaultGroup // XXX
    val rb        = Buffer(server)(numFrames = 1, numChannels = bus.numChannels)
    rb.read(path, fileStartFrame = audioVal.offset, numFrames = 1)
    val args      = List[ControlSetMap]("buf" -> rb.id, "amp" -> audioVal.gain)

    // val rs = rd.play(target = target, args = args, buffers = rb :: Nil)
    synth.play(target = target, args = args, addAction = addToHead, dependencies = rb :: Nil)

    synth.onEndTxn { implicit tx =>
      rb   .dispose() // XXX TODO: why was this not in the code before? Is this causing any trouble?
      synth.dispose()
    }

    synth.write(bus -> "out")
  }

  def remove()(implicit tx: Txn): Unit = if (synth.isOnline) synth.free()
}