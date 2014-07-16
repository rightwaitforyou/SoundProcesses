/*
 *  scan.scala
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

import de.sciss.synth.proc.UGenGraphBuilder.Input

import collection.immutable.{IndexedSeq => Vec}
import de.sciss.synth.ugen.UGenInGroup

object scan {
  private[proc] def outControlName(key: String): String = "$out_" + key
  private[proc] def inControlName (key: String): String = "$in_"  + key

  sealed trait InLike extends GE.Lazy with AudioRated {
    protected def key: String
    // protected def numChannels: Int

    final def makeUGens: UGenInLike = {
      val b = UGenGraphBuilder.get
      val numCh   = b.requestInput(Input.Scan(key))
      // val numCh   = b.addScanIn(key, numChannels)
      val ctlName = inControlName(key)
      mkUGen(ctlName, numCh)
    }

    protected def mkUGen(ctlName: String, numCh: Int): UGenInLike
  }

  final case class In(key: String /*, default: Double = 0.0 */)
    extends InLike {

    // override def toString = s"""scan.In("$key", $default)"""
    override def toString = s"""scan.In("$key")"""

    override def productPrefix = "scan$In"

    // protected def numChannels = -1

    protected def mkUGen(ctlName: String, numCh: Int): UGenInLike =
      if (numCh == 1) {
        ctlName.ar(0.0f).expand
      } else if (numCh > 1) {
        ctlName.ar(Vector.fill(numCh)(0.0f)).expand
      } else {
        UGenInGroup.empty
      }
  }

  //  final case class InFix(key: String, numChannels: Int)
  //    extends InLike {
  //
  //    override def toString = s"""scan.InFix("$key", $numChannels)"""
  //
  //    override def productPrefix = "scan$InFix"
  //
  //    protected def mkUGen(ctlName: String, numCh: Int): UGenInLike =
  //      ugen.In.ar(ctlName.kr, numCh)
  //  }

  final case class Out(key: String, in: GE)
    extends UGenSource.ZeroOut with WritesBus {

    override def productPrefix = "scan$Out"

    override def toString = s"""scan("$key") := $in"""

    protected def makeUGens: Unit = {
      val bus = outControlName(key).kr
      unwrap(Vector(bus.expand) ++ in.expand.outputs)
    }

    // first arg: bus control, remaining args: signal to write; thus numChannels = _args.size - 1
    protected def makeUGen(_args: Vec[UGenIn]): Unit = {
      val busArg      = _args.head
      val sigArg      = _args.tail
      val numChannels = sigArg.size
      val b = UGenGraphBuilder.get
      b.addScanOut(key, numChannels)
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
