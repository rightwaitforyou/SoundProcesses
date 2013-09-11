/*
 *  SegmentWriter.scala
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

import de.sciss.synth.Curve.parametric
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.lucre.synth.{DynamicBusUser, AudioBus, Synth, Resource, Txn}
import de.sciss.synth.{addToHead, ControlSetMap, SynthGraph}

object SegmentWriter {
  def apply(bus: AudioBus, segm: Grapheme.Segment.Curve, time: Long, sampleRate: Double)
           (implicit tx: Txn): SegmentWriter = {

    val (usesShape, sg) = graph(segm)
    val synth = Synth(bus.server, sg, nameHint = Some("grapheme-segm"))
    // val bus   = RichBus.audio(server, segm.numChannels)
    val res = new SegmentWriter(synth, usesShape, bus, segm, time, sampleRate)
    res.britzelAdd()
    res
  }

  private def graph(segm: Grapheme.Segment.Curve): (Boolean, SynthGraph) = {
    var usesShape = false // XXX TODO dirty variable
    val sg = SynthGraph {
      import de.sciss.synth._
      import ugen._

      val zero        = Vec.fill(segm.numChannels)(0f)
      val start       = "start".ir(zero)
      val stop        = "stop" .ir(zero)
      val dur         = "dur".ir

      lazy val shape      = "shape".ir(zero)
      lazy val curvature  = "curve".ir(zero)

      val sig: GE = segm.values.zipWithIndex.map {
        case ((segmStart, segmStop, segmShape), ch) =>
          val doneAction = if (ch == 0) freeSelf else doNothing
          segmShape match {
            case Curve.linear =>
              Line.ar(start, stop, dur, doneAction = doneAction)
            case Curve.exponential =>
              if (segmStart != 0f && segmStop != 0f && segmStart * segmStop > 0f) {
                XLine.ar(start, stop, dur, doneAction = doneAction)
              } else {
                Line.ar(0, 0, dur, doneAction = doneAction)
              }
            case _ =>
              usesShape = true
              val curve = Env.Curve(shape \ ch, curvature \ ch)
              val env   = Env(start, Env.Segment(dur = dur, targetLevel = stop, curve = curve) :: Nil)
              EnvGen.ar(env, doneAction = doneAction)
          }
      }
      // sig.poll(4)
      Out.ar("out".kr, sig)
    }
    (usesShape, sg)
  }
}
final class SegmentWriter private (synth: Synth, usesShape: Boolean, val bus: AudioBus,
                                   segm: Grapheme.Segment.Curve, time: Long, sampleRate: Double)
  extends DynamicBusUser with Resource.Source {

  def resource(implicit tx: Txn) = synth

  def server = synth.server

  def add()(implicit tx: Txn) = ()

  def britzelAdd()(implicit tx: Txn): Unit = {
    type Ctl = List[ControlSetMap]

    val target    = server.defaultGroup // XXX
    val durSecs   = segm.span.length / sampleRate
    val (vStart, vStop, vShape) = segm.values.unzip3
    val ctl0: Ctl = List("start" -> vStart.map(_.toFloat), "stop" -> vStop.map(_.toFloat), "dur" -> durSecs)
    val ctl1: Ctl = if (usesShape) "shape" -> vShape.map(_.id.toFloat) :: ctl0 else ctl0
    val args: Ctl = if (usesShape) "curve" -> vShape.map { case parametric(c) => c; case _ => 0f } :: ctl1 else ctl1

    synth.play(target = target, args = args, addAction = addToHead, dependencies = Nil)

    synth.write(bus -> "out")
  }

  def remove()(implicit tx: Txn): Unit = {
    synth.free()
  }
}
