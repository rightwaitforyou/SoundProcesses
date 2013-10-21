/*
 *  AudioArtifactWriter.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import collection.immutable.{Seq => ISeq}
import de.sciss.lucre.synth.{AudioBus, DynamicBusUser, Buffer, Synth, Resource, Txn}
import de.sciss.synth.{addToHead, ControlSetMap, SynthGraph}
import de.sciss.span.Span

object AudioArtifactWriter {
  def apply(bus: AudioBus, segm: Grapheme.Segment.Audio, time: Long, sampleRate: Double)
           (implicit tx: Txn): AudioArtifactWriter = {
    val numChannels = segm.numChannels
    // val bus         = RichBus.audio(server, numChannels)
    val sg  = SynthGraph {
      import de.sciss.synth._
      import ugen._
      val buf = "buf".ir
      val dur = "dur".ir(1)
      val out = "out".kr
      val amp = "amp".kr(1)
      val sig = DiskIn.ar(numChannels, buf) * amp
      Line.kr(start = 0, end = 0, dur = dur, doneAction = freeSelf)
      Out.ar(out, sig)
    }
    val synth = Synth(bus.server, sg, nameHint = Some("audio-artifact"))
    val res = new AudioArtifactWriter(synth, bus, segm, time, sampleRate)
    res.britzelAdd()
    res
  }
}
final class AudioArtifactWriter private (synth: Synth, val bus: AudioBus, segm: Grapheme.Segment.Audio, time: Long,
                                         sampleRate: Double)
  extends DynamicBusUser with Resource.Source {

  // private val synthRef  = Ref(Option.empty[Synth])

  // def synth(implicit tx: Txn): Option[Synth] = synthRef()(tx.peer)

  def resource(implicit tx: Txn) = synth

  def server = synth.server

  def add()(implicit tx: Txn) = ()

  def britzelAdd()(implicit tx: Txn): Unit = {
    val audioVal  = segm.value
    val file      = audioVal.artifact
    val path      = file.getAbsolutePath
    val fileFrames= audioVal.spec.numFrames
    val target    = server.defaultGroup // XXX
    val fStart    = math.max(0L, math.min(fileFrames, audioVal.offset + (time - segm.span.start)))
    val fStop     = math.min(fileFrames, segm.span match {
      case Span.HasStop(stop) => fStart + (stop - time)
      case _ => Long.MaxValue
    })
    val dur       = (fStop - fStart) / sampleRate // XXX TODO: could use SRC at some point
    // println(f"AudioArtifactWriter. fStart = $fStart, fStop = $fStop, $dur = $dur%1.3f")
    val rb        = Buffer.diskIn(server)(path, startFrame = fStart, numChannels = bus.numChannels)
    val args: ISeq[ControlSetMap] = ISeq("buf" -> rb.id, "dur" -> dur, "amp" -> audioVal.gain)

    // val rs = rd.play( target = target, args = args, buffers = rb :: Nil )
    synth.play(target = target, args = args, addAction = addToHead, dependencies = rb :: Nil)

    synth.onEndTxn { implicit tx =>
      // bufPeer.close( bufPeer.freeMsg )
      synth.dispose()
    }

    // rs.play( target = target, args = args, buffers = rb :: Nil )
    synth.write(bus -> "out")

    // val oldSynth = synthRef.swap(Some(rs))(tx.peer)
    // require(oldSynth.isEmpty, "AudioArtifactWriter.add() : old synth still playing")
  }

  def remove()(implicit tx: Txn): Unit = {
    if (synth.isOnline) synth.free()
  }
}