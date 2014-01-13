/*
 *  AudioLink.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.lucre.synth.{AudioBus, NodeGraph, DynamicBusUser, Synth, Resource, Txn}
import de.sciss.synth.{addToHead, SynthGraph}

object AudioLink {
  def apply(edge: NodeGraph.Edge, sourceBus: AudioBus, sinkBus: AudioBus)(implicit tx: Txn): AudioLink = {
    val numCh = sourceBus.numChannels
    require(numCh == sinkBus.numChannels,  s"Source has $numCh channels while sink has ${sinkBus.numChannels}")
    val sg    = graph(numCh)
    val synth = Synth(sourceBus.server, sg, nameHint = Some("audio-link"))
    val res   = new AudioLink(edge, sourceBus, sinkBus, synth)
    res.britzelAdd()
    res
  }

  private def graph(numChannels: Int): SynthGraph = SynthGraph {
    import de.sciss.synth._
    import ugen._

    // val sig = "in".ar(Vec.fill(numChannels)(0f))
    val sig = In.ar("in".kr, numChannels) // InFeedback?
    Out.ar("out".kr, sig)
  }
}
final class AudioLink private (edge: NodeGraph.Edge, sourceBus: AudioBus, sinkBus: AudioBus, synth: Synth)
  extends DynamicBusUser with Resource.Source {

  def resource(implicit tx: Txn) = synth

  def server = synth.server

  def add()(implicit tx: Txn): Unit = {
    synth.moveToHead(audible = false, group = edge.sink.preGroup())
    NodeGraph.addEdge(edge)
  }

  def bus: AudioBus = sourceBus  // XXX whatever

  def britzelAdd()(implicit tx: Txn): Unit = {
    // synth.play(target = edge.sink.preGroup(), args = Nil, addAction = addToHead, dependencies = Nil)
    synth.play(target = server.defaultGroup, args = Nil, addAction = addToHead, dependencies = Nil)
    synth.read (sourceBus -> "in")
    synth.write(sinkBus   -> "out")
    // ProcDemiurg.addEdge(edge)
  }

  def remove()(implicit tx: Txn): Unit = {
    synth.free()
    NodeGraph.removeEdge(edge)
  }
}