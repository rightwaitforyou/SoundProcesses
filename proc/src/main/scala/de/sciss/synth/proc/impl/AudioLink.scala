/*
 *  AudioLink.scala
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

import de.sciss.lucre.synth.{DynamicUser, AudioBus, NodeGraph, Synth, Resource, Txn}
import de.sciss.synth.{addToHead, SynthGraph}

object AudioLink {
  def apply(edge: NodeGraph.Edge, sourceBus: AudioBus, sinkBus: AudioBus)
           (implicit tx: Txn): Resource with DynamicUser = {
    val numCh = sourceBus.numChannels
    require(numCh == sinkBus.numChannels,  s"Source has $numCh channels while sink has ${sinkBus.numChannels}")
    val sg    = graph(numCh)
    val synth = Synth(sourceBus.server, sg, nameHint = Some("audio-link"))
    val res   = new Impl(edge, sourceBus, sinkBus, synth)
    res.play()
    res
  }

  private def graph(numChannels: Int): SynthGraph = SynthGraph {
    import de.sciss.synth._
    import ugen._

    // val sig = "in".ar(Vec.fill(numChannels)(0f))
    val sig = In.ar("in".kr, numChannels) // InFeedback?
    Out.ar("out".kr, sig)
  }

  private final class Impl(edge: NodeGraph.Edge, sourceBus: AudioBus, sinkBus: AudioBus, synth: Synth)
    extends DynamicUser with Resource.Proxy {

    protected def resourcePeer: Resource = synth

    def add()(implicit tx: Txn): Unit = {
      synth.moveToHead(audible = false, group = edge.sink.preGroup())
      NodeGraph.addEdge(edge)
    }

    def play()(implicit tx: Txn): Unit = {
      // synth.play(target = edge.sink.preGroup(), args = Nil, addAction = addToHead, dependencies = Nil)
      synth.play(target = server.defaultGroup, args = Nil, addAction = addToHead, dependencies = Nil)
      synth.read (sourceBus -> "in")
      synth.write(sinkBus   -> "out")
      // ProcDemiurg.addEdge(edge)
    }

    def remove()(implicit tx: Txn): Unit = NodeGraph.removeEdge(edge)
  }
}