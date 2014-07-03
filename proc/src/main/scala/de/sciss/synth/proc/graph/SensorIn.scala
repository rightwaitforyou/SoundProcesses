///*
// *  SensorIn.scala
// *  (SoundProcesses)
// *
// *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
// *
// *	This software is published under the GNU General Public License v2+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.synth
//package proc
//package graph
//
//import de.sciss.synth.ugen.In
//
//object SensorIn {
//  def kr(off: Int = 0, numChannels: Int = 1): SensorIn = apply(off, numChannels)
//}
//case class SensorIn(off: Int, numChannels: Int) extends GE.Lazy with ControlRated {
//  protected def makeUGens: UGenInLike = UGenGraph.builder match {
//    case b: UGenGraphBuilder[_] =>
//      val bus   = b.sensorBus
//      val stop  = bus.index + bus.numChannels
//      val off1  = math.min(off, bus.numChannels)
//      val num1  = math.min(numChannels, stop - off1)
//      In.kr(bus.index + off1, num1)
//
//    case other => UGenGraphBuilder.outsideOfContext()
//  }
//}