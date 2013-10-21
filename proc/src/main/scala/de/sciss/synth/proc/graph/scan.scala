/*
 *  scan.scala
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

package de.sciss.synth
package proc
package graph

import collection.immutable.{IndexedSeq => Vec}
import de.sciss.synth.ugen.UGenInGroup

object scan {
  private[proc] def outControlName(key: String): String = "$out_" + key
  private[proc] def inControlName (key: String): String = "$in_"  + key

  sealed trait InLike extends GE.Lazy with AudioRated {
    protected def key: String
    protected def numChannels: Int

    final def makeUGens: UGenInLike = {
      UGenGraph.builder match {
        case b: UGenGraphBuilder[_] =>
          val numCh   = b.addScanIn(key, numChannels)
          val ctlName = inControlName(key)
          mkUGen(ctlName, numCh)

        case _ => UGenGraphBuilder.outsideOfContext()
      }
    }

    protected def mkUGen(ctlName: String, numCh: Int): UGenInLike
  }

  final case class In(key: String, default: Double = 0.0)
    extends InLike {

    override def toString = s"""scan.In("$key", $default)"""

    override def productPrefix = "scan$In"

    protected def numChannels = -1

    protected def mkUGen(ctlName: String, numCh: Int): UGenInLike =
      if (numCh == 1) {
        ctlName.ar(default).expand
      } else if (numCh > 1) {
        ctlName.ar(Vector.fill(numCh)(default)).expand
      } else {
        UGenInGroup.empty
      }
  }

  final case class InFix(key: String, numChannels: Int)
    extends InLike {

    override def toString = s"""scan.InFix("$key", $numChannels)"""

    override def productPrefix = "scan$InFix"

    protected def mkUGen(ctlName: String, numCh: Int): UGenInLike =
      ugen.In.ar(ctlName.kr, numCh)
  }

  final case class Out(key: String, in: GE)
    extends UGenSource.ZeroOut with WritesBus {

    override def productPrefix = "scan$Out"

    override def toString = s"""scan("$key") := $in"""

    protected def makeUGens {
      val bus = outControlName(key).kr
      unwrap(Vector(bus.expand) ++ in.expand.outputs)
    }

    // first arg: bus control, remaining args: signal to write; thus numChannels = _args.size - 1
    protected def makeUGen(_args: Vec[UGenIn]): Unit = {
      val busArg      = _args.head
      val sigArg      = _args.tail
      val numChannels = sigArg.size
      UGenGraph.builder match {
        case b: UGenGraphBuilder[_] =>
          b.addScanOut(key, numChannels)
        case other => UGenGraphBuilder.outsideOfContext()
      }
      val sigArgAr = sigArg.map { ui =>
        if (ui.rate == audio) ui else new UGen.SingleOut("K2A", audio, Vector(ui))
      }
      new UGen.ZeroOut("Out", audio, busArg +: sigArgAr, isIndividual = true)
    }
  }
}

//final case class scan(key: String) {
//  def ar                 : GE = ar(0.0)
//  def ar(default: Double): GE = scan.In(key, default)
//  def ar(default: Double, numChannels: Int): GE = scan.InFix(key, default, numChannels)
//
//  def :=(in: GE): Unit = scan.Out(key, in)
//}