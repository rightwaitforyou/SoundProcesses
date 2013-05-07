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

import scala.concurrent.stm.Ref

final class SegmentWriter(segm: Grapheme.Segment.Curve, time: Long, server: Server, sampleRate: Double)
  extends DynamicBusUser /* DynamicAudioBusUser with RichAudioBus.User with TxnPlayer */ {

  private val synthRef  = Ref(Option.empty[Synth])
  val bus               = RichBus.audio(server, segm.numChannels)

  //  protected def synth(implicit tx: Txn): Option[Synth] = synthRef.get(tx.peer)
  //  protected def synth_=(value: Option[Synth])(implicit tx: Txn) {
  //    val oldSynth = synthRef.swap(value)(tx.peer)
  //    value.foreach(addMapBusConsumer)
  //    oldSynth.foreach(_.free(audible = true))
  //  }
  //
  //  protected def addMapBusConsumer(rs: Synth)(implicit tx: Txn) {
  //    //         val rb = mapBus
  //    //         rs.write( rb -> "$out" )
  //    rs.write(bus -> "$out")
  //  }

  // ... until ScalaCollider has better methods in ControlProxyFactory
  @inline private def mkMultiCtl(name: String): GE = {
    // import ugen._
    // ControlProxy(scalar, Vector.fill(segm.numChannels)(0f), Some(name))(ControlProxyFactory.controlIrFactory)
    name.ir(0f, Vector.fill(segm.numChannels - 1)(0f): _*)
  }

  private var usesShape = false // XXX TODO dirty variable

  protected def graph = SynthGraph {
    import ugen._

    // val start       = "start".ir
    val start       = mkMultiCtl("start")
    // val stop        = "stop".ir
    val stop        = mkMultiCtl("stop")
    val dur         = "dur".ir

    lazy val shape  = mkMultiCtl("shape")
    lazy val curve  = mkMultiCtl("curve")

    val sig: GE = segm.values.zipWithIndex.map {
      case ((segmStart, segmStop, segmShape), ch) =>
        val doneAction = if (ch == 0) freeSelf else doNothing
        segmShape match {
          case `linShape` =>
            Line.ar(start, stop, dur, doneAction = doneAction)
          case `expShape` =>
            if (segmStart != 0f && segmStop != 0f && segmStart * segmStop > 0f) {
              XLine.ar(start, stop, dur, doneAction = doneAction)
            } else {
              Line.ar(0, 0, dur, doneAction = doneAction)
            }
          case _ =>
            usesShape = true
            val env = Env(start, Env.Seg(dur = dur, targetLevel = stop,
              shape = varShape(shape \ ch, curve \ ch)) :: Nil)
            EnvGen.ar(env, doneAction = doneAction)

        }
    }
    // sig.poll(4)
    Out.ar("out".kr, sig)
  }

  def add()(implicit tx: Txn) {
    type Ctl = List[ControlSetMap]

    val sg        = graph
    // val rsd       = SynthDef(aural.server, g)
    val target    = server.defaultGroup // XXX
    val durSecs   = segm.span.length / sampleRate
    val (vStart, vStop, vShape) = segm.values.unzip3
    val ctl0: Ctl = List("start" -> vStart.map(_.toFloat), "stop" -> vStop.map(_.toFloat), "dur" -> durSecs)
    val ctl1: Ctl = if (usesShape) ("shape" -> vShape.map(_.id.toFloat)) :: ctl0 else ctl0
    val args: Ctl = if (usesShape) ("curve" -> vShape.map(_.curvature )) :: ctl1 else ctl1

    // val rs        = rsd.play(aural.preGroup, ctl)
    val rs = Synth(sg, nameHint = Some("grapheme-segm"))(target = target, args = args /* , dependencies = rb :: Nil */)

    // rs.onEndTxn { implicit tx =>
    //   synth.foreach { rs2 => if (rs == rs2) ctrl.glidingDone() }
    // }

    rs.write(bus -> "out")

    val oldSynth = synthRef.swap(Some(rs))(tx.peer)
    // bus.addWriter( this )

    require(oldSynth.isEmpty, "SegmentWriter.add() : old synth still playing")
  }

  def remove()(implicit tx: Txn) {
    val rs = synthRef.swap(None)(tx.peer).getOrElse(
      sys.error("SegmentWriter.remove() : there was no synth playing")
    )
    rs.free()
  }

  //  def stop(implicit tx: Txn) {
  //    synthRef.swap(None)(tx.peer).foreach(_.free(audible = true))
  //  }
  //
  //  def isPlaying(implicit tx: Txn): Boolean = synth.map(_.isOnline).getOrElse(false)
  //
  //  // ---- RichAudioBus.User ----
  //
  //  def busChanged(bus: AudioBus)(implicit tx: Txn) {
  //    ...
  //  }
  //
  //  // ---- DynamicAudioBusUser ----
  //
  //  def add()(implicit tx: Txn) {
  //    bus.addWriter(this)
  //  }
  //
  //  def remove()(implicit tx: Txn) {
  //    bus.removeWriter(this)
  //  }
  //
  //  def migrateTo(newBus: RichAudioBus)(implicit tx: Txn): DynamicAudioBusUser = {
  //    ...
  //  }
}
