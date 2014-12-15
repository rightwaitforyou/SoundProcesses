/*
 *  NodeGraph.scala
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

import de.sciss.lucre.synth.impl.DummyNodeGraphImpl
import de.sciss.synth.{UGenGraph, SynthGraph}
import de.sciss.synth
import de.sciss.lucre.synth.{Node => LNode}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{Ref, TMap, TSet}
import scala.concurrent.Future

object NodeGraph {

  final case class Edge(source: NodeRef /* , sourceKey: String */, sink: NodeRef /*, sinkKey: String */)
    extends Topology.Edge[NodeRef] {

    def sourceVertex = source
    def targetVertex = sink
  }

  // var verbose = false

  private val servers     = TSet.empty[Server]

  def addServer(server: Server)(implicit tx: Txn): Unit = {
    implicit val itx = tx.peer
    if (servers.contains(server)) return
    servers += server
    worlds  += server -> new impl.NodeGraphImpl(server)
  }

  def removeServer(server: Server)(implicit tx: Txn): Unit = {
    implicit val itx = tx.peer
    servers -= server
    worlds  -= server
    Bus.serverRemoved(server)
  }

  // commented out for debugging inspection
  private val worlds = TMap.empty[Server, NodeGraph]

  def apply(server: Server)(implicit tx: Txn): NodeGraph =
    worlds.get(server)(tx.peer).getOrElse(DummyNodeGraphImpl)

  def addNode(node: NodeRef)(implicit tx: Txn): Unit =
    apply(node.server).addNode(node)

  def removeNode(node: NodeRef)(implicit tx: Txn): Unit =
    apply(node.server).removeNode(node)

  def addEdge(edge: Edge)(implicit tx: Txn): Unit = {
    val world                 = apply(edge.source.server)
    val res                   = world.addEdge(edge)
    val (_, source, affected) = res.getOrElse(sys.error(s"Edge $edge is cyclic"))

    // if (verbose) println("NEW TOPO = " + newTopo + "; SOURCE = " + source + "; AFFECTED = " + affected)
    if (affected.isEmpty) return

    val isAfter = source == edge.source

    var succ = source.node
    var pred: LNode = null
    val iter = affected.iterator
    while (iter.hasNext) {
      pred = succ
      val curr = iter.next().node
      if (isAfter) {
        curr.moveAfter (pred)
      } else {
        curr.moveBefore(pred)
      }
      succ = curr
    }
  }

  def removeEdge(edge: Edge)(implicit tx: Txn): Unit =
    apply(edge.source.server).removeEdge(edge)

  private[synth] def send(server: Server, bundles: Txn.Bundles): Unit = {
    val w = worlds.single.getOrElse(server, DummyNodeGraphImpl) // sys.error(s"Trying to access unregistered server $server")
    w.send(bundles)
  }

  private[synth] def messageTimeStamp(server: Server)(implicit tx: Txn): Ref[Int] =
    apply(server).messageTimeStamp

  private[synth] def getSynthDef(server: Server, graph: SynthGraph, nameHint: Option[String])(implicit tx: Txn): SynthDef =
    getSynthDef(server, graph.expand(synth.impl.DefaultUGenGraphBuilderFactory), nameHint)

  private[synth] def getSynthDef(server: Server, graph: UGenGraph, nameHint: Option[String])(implicit tx: Txn): SynthDef =
    apply(server).getSynthDef(server, graph, nameHint)
}
trait NodeGraph {
  // def server: Server

  def addNode   (node: NodeRef)(implicit tx: Txn): Unit
  def removeNode(node: NodeRef)(implicit tx: Txn): Unit

  def addEdge   (edge: NodeGraph.Edge)(implicit tx: Txn): Option[(Topology[NodeRef, NodeGraph.Edge], NodeRef, Vec[NodeRef])]
  def removeEdge(edge: NodeGraph.Edge)(implicit tx: Txn): Unit

  def send(bundles: Txn.Bundles): Future[Unit]

  private[synth] def messageTimeStamp: Ref[Int]

  def getSynthDef(server: Server, graph: UGenGraph, nameHint: Option[String])(implicit tx: Txn): SynthDef

  /** Queries the current topology */
  def topology(implicit tx: Txn): Topology[NodeRef, NodeGraph.Edge]
}