/*
 *  Synth.scala
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

package de.sciss.lucre
package synth

import collection.immutable.{Seq => ISeq}
import de.sciss.synth.{UGenGraph, addToHead, AddAction, ControlSetMap, SynthGraph, Synth => SSynth}
import impl.{SynthImpl => Impl}

object Synth {
  def apply(server: Server, graph: SynthGraph, nameHint: Option[String] = None)(implicit tx: Txn): Synth = {
    val df = NodeGraph.getSynthDef(server, graph, nameHint)
    create(df)
  }

  def play(graph: SynthGraph, nameHint: Option[String] = None)
          (target: Node, args: ISeq[ControlSetMap] = Nil, addAction: AddAction = addToHead,
            dependencies: List[Resource] = Nil)(implicit tx: Txn): Synth = {
    val res = apply(target.server, graph, nameHint)
    res.play(target, args, addAction, dependencies)
    res
  }

  /* private[synth] */ def expanded(server: Server, graph: UGenGraph, nameHint: Option[String] = None)
                            (implicit tx: Txn): Synth = {
    val df = NodeGraph.getSynthDef(server, graph, nameHint)
    create(df)
  }

  private def create(df: SynthDef)(implicit tx: Txn): Synth = {
    val server  = df.server
    val nodeID  = server.nextNodeID()
    new Impl(SSynth(server.peer, nodeID), df)
  }
}

trait Synth extends Node {
  def peer: SSynth

  def definition: SynthDef

  /* private[synth] */ def play(target: Node, args: ISeq[ControlSetMap], addAction: AddAction, dependencies: List[Resource])
                         (implicit tx: Txn): Unit
}