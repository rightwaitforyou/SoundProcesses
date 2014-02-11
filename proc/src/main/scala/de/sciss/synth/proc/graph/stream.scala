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

  case class DiskIn(rate: Rate, key: String, loop: synth.GE) extends GE {
    override def productPrefix  = "stream$DiskIn"

    protected def makeUGen(numChannels: Int, buf: synth.GE, gain: synth.GE): UGenInLike =
      ugen.DiskIn(rate, numChannels = numChannels, buf = buf, loop = loop) * gain
  }

  case class VDiskIn(rate: Rate, key: String, speed: synth.GE, loop: synth.GE) extends GE {
    override def productPrefix  = "stream$VDiskIn"

    protected def makeUGen(numChannels: Int, buf: synth.GE, gain: synth.GE): UGenInLike =
      ugen.VDiskIn(rate, numChannels = numChannels, buf = buf, speed = speed, loop = loop) * gain
  }
}
final case class stream(key: String)