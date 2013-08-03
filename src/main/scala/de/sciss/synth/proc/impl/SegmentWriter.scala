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

package de.sciss
package synth
package proc
package impl

import de.sciss.synth.Curve.parametric

object SegmentWriter {
  def apply(segm: Grapheme.Segment.Curve, time: Long, server: Server, sampleRate: Double)
           (implicit tx: Txn): SegmentWriter = {

    val (usesShape, sg) = graph(segm)
    val synth = Synth(server, sg, nameHint = Some("grapheme-segm"))
    val bus   = RichBus.audio(server, segm.numChannels)
    val res = new SegmentWriter(synth, usesShape, bus, segm, time, sampleRate)
    res.britzelAdd()
    res
  }

  // ... until ScalaCollider has better methods in ControlProxyFactory
  @inline private def mkMultiCtl(segm: Grapheme.Segment.Curve, name: String): GE = {
    // import ugen._
    // ControlProxy(scalar, Vector.fill(segm.numChannels)(0f), Some(name))(ControlProxyFactory.controlIrFactory)
    name.ir(Vector.fill(segm.numChannels)(0f))
  }

  private def graph(segm: Grapheme.Segment.Curve): (Boolean, SynthGraph) = {
    var usesShape = false // XXX TODO dirty variable
    val sg = SynthGraph {
      import ugen._

      // val start       = "start".ir
      val start       = mkMultiCtl(segm, "start")
      // val stop        = "stop".ir
      val stop        = mkMultiCtl(segm, "stop")
      val dur         = "dur".ir

      lazy val shape      = mkMultiCtl(segm, "shape")
      lazy val curvature  = mkMultiCtl(segm, "curve")

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
final class SegmentWriter private (synth: Synth, usesShape: Boolean, val bus: RichAudioBus,
                                   segm: Grapheme.Segment.Curve, time: Long, sampleRate: Double)
  extends DynamicBusUser /* DynamicAudioBusUser with RichAudioBus.User with TxnPlayer */ with Resource.Source {

  // private val synthRef  = Ref(Option.empty[Synth])
  // val bus               = RichBus.audio(server, segm.numChannels)

  def resource(implicit tx: Txn) = synth

  //  protected def synth(implicit tx: Txn): Option[Synth] = synthRef.get(tx.peer)
  //  protected def synth_=(value: Option[Synth])(implicit tx: Txn): Unit = {
  //    val oldSynth = synthRef.swap(value)(tx.peer)
  //    value.foreach(addMapBusConsumer)
  //    oldSynth.foreach(_.free(audible = true))
  //  }
  //
  //  protected def addMapBusConsumer(rs: Synth)(implicit tx: Txn): Unit = {
  //    //         val rb = mapBus
  //    //         rs.write( rb -> "$out" )
  //    rs.write(bus -> "$out")
  //  }

  def server = synth.server

  def add()(implicit tx: Txn) = ()

  def britzelAdd()(implicit tx: Txn): Unit = {
    type Ctl = List[ControlSetMap]

    // val rsd       = SynthDef(aural.server, g)
    val target    = server.defaultGroup // XXX
    val durSecs   = segm.span.length / sampleRate
    val (vStart, vStop, vShape) = segm.values.unzip3
    val ctl0: Ctl = List("start" -> vStart.map(_.toFloat), "stop" -> vStop.map(_.toFloat), "dur" -> durSecs)
    val ctl1: Ctl = if (usesShape) ("shape" -> vShape.map(_.id.toFloat)) :: ctl0 else ctl0
    val args: Ctl = if (usesShape) ("curve" -> vShape.map { case parametric(c) => c; case _ => 0f } :: ctl1) else ctl1

    // val rs        = rsd.play(aural.preGroup, ctl)
    synth.play(target = target, args = args, addAction = addToHead, dependencies = Nil)

    // rs.onEndTxn { implicit tx =>
    //   synth.foreach { rs2 => if (rs == rs2) ctrl.glidingDone() }
    // }

    synth.write(bus -> "out")

    // val oldSynth = synthRef.swap(Some(rs))(tx.peer)
    // require(oldSynth.isEmpty, "SegmentWriter.add() : old synth still playing")
  }

  def remove()(implicit tx: Txn): Unit = {
    //    val rs = synthRef.swap(None)(tx.peer).getOrElse(
    //      sys.error("SegmentWriter.remove() : there was no synth playing")
    //    )
    //    rs.free()
    synth.free()
  }

  //  def stop(implicit tx: Txn): Unit = {
  //    synthRef.swap(None)(tx.peer).foreach(_.free(audible = true))
  //  }
  //
  //  def isPlaying(implicit tx: Txn): Boolean = synth.map(_.isOnline).getOrElse(false)
  //
  //  // ---- RichAudioBus.User ----
  //
  //  def busChanged(bus: AudioBus)(implicit tx: Txn): Unit = {
  //    ...
  //  }
  //
  //  // ---- DynamicAudioBusUser ----
  //
  //  def add()(implicit tx: Txn): Unit = {
  //    bus.addWriter(this)
  //  }
  //
  //  def remove()(implicit tx: Txn): Unit = {
  //    bus.removeWriter(this)
  //  }
  //
  //  def migrateTo(newBus: RichAudioBus)(implicit tx: Txn): DynamicAudioBusUser = {
  //    ...
  //  }
}
