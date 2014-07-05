/*
 *  AuralScanImpl.scala
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

import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.{Bus, Synth, NodeGraph, AuralNode, AudioBus, Sys}
import de.sciss.synth.proc.Scan.Link
import de.sciss.synth.{addToHead, SynthGraph}

import scala.concurrent.stm.{TMap, Ref}

object AuralScanImpl {
  def apply[S <: Sys[S]](view: AuralObj.Proc[S], key: String, scan: Scan[S], numChannels: Int)
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralScan[S] = {

    scan.sources.foreach {
      case Link.Grapheme(peer) =>
      case Link.Scan    (peer) =>
        context.getAux[AuralScan[S]](peer.id).foreach { sourceView =>
        }
    }
    scan.sinks.foreach {
      case Link.Grapheme(peer) => // nada - currently not supported
      case Link.Scan    (peer) =>
        context.getAux[AuralScan[S]](peer.id).foreach { sinkView =>

        }
    }

    import context.server
    val bus = Bus.audio(server, numChannels = numChannels)
    val res: AuralScan[S] = new Impl(key = key, bus = bus)
    context.putAux(scan.id, res)
    res
  }

  private def LinkNode[S <: Sys[S]](source: AuralScan[S], sourceNode: AuralNode,
                                    sink  : AuralScan[S], sinkNode  : AuralNode)(implicit tx: S#Tx): LinkNode[S] = {
    val edge      = NodeGraph.Edge(sourceNode, source.key, sinkNode, sink.key)

    val sourceBus = source.bus
    val sinkBus   = sink  .bus
    val sourceCh  = sourceBus.numChannels
    val sinkCh    = sinkBus  .numChannels
    val numCh     = math.min(sourceCh, sinkCh)

    val server    = sourceBus.server
    if (sinkBus.server != server) throw new IllegalArgumentException("Trying to link nodes across servers")

    val g = SynthGraph {
      import de.sciss.synth._
      import ugen._

      val sig = In.ar("in".kr, numCh) // InFeedback?
      Out.ar("out".kr, sig)
    }

    val synth = Synth(sourceBus.server, g, nameHint = Some(s"audio-link$numCh"))
    synth.play(target = sinkNode.preGroup(), args = Nil, addAction = addToHead, dependencies = Nil)
    synth.read (sourceBus -> "in" )
    synth.write(sinkBus   -> "out")

    NodeGraph.addEdge(edge)
    new LinkNode(edge, synth)
  }
  private final class LinkNode[S <: Sys[S]](edge: NodeGraph.Edge, synth: Synth)
    extends Disposable[S#Tx] {

    def dispose()(implicit tx: S#Tx): Unit = {
      synth.free()
      NodeGraph.removeEdge(edge)
    }
  }

  private final class Impl[S <: Sys[S]](val key: String, val bus: AudioBus)
    extends AuralScan.Owned[S] /* with ObservableImpl[S, AuralScan.Update[S]] */ {

    private val sources = Ref(Set.empty[AuralScan[S]])
    private val sinks   = Ref(Set.empty[AuralScan[S]])
    private val links   = TMap.empty[AuralScan[S], LinkNode[S]] // key = sink

    def addSource(source: AuralScan[S])(implicit tx: S#Tx): Unit = {
      sources.transform(_ + source)(tx.peer)
    }

    def addSink(sink: AuralScan[S])(implicit tx: S#Tx): Unit = {
      sinks.transform(_ + sink)(tx.peer)
      node.foreach { sourceNode =>
        tryLink(sourceNode, sink)
      }
    }

    private def tryLink(sourceNode: AuralNode, sink: AuralScan[S])(implicit tx: S#Tx): Unit =
      sink.node.foreach { sinkNode =>
        val link = LinkNode[S](this, sourceNode, sink, sinkNode)
        links.put(sink, link)(tx.peer)
      }

    def removeSource(source: AuralScan[S])(implicit tx: S#Tx): Unit = {
      sources.transform(_ - source)(tx.peer)
    }

    def removeSink(sink: AuralScan[S])(implicit tx: S#Tx): Unit = {
      sinks.transform(_ - sink)(tx.peer)
      links.remove(sink)(tx.peer).foreach { link =>
        link.dispose()
      }
    }

    //    def sourceUpdated(source: AuralScan[S])(implicit tx: S#Tx): Unit = {
    //      ...
    //    }
    //
    //    def sinkUpdated(sink: AuralScan[S])(implicit tx: S#Tx): Unit = {
    //      ...
    //    }

    private val nodeRef = Ref(Option.empty[AuralNode])

    def node(implicit tx: S#Tx): Option[AuralNode] = nodeRef.get(tx.peer)

    def node_=(value: Option[AuralNode])(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      nodeRef() = value
      disposeLinks()
      value.foreach { sourceNode =>
        sinks().foreach { sink =>
          tryLink(sourceNode, sink)
        }
      }
    }

    private def disposeLinks()(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      if (!links.isEmpty) {
        links.foreach { case (_, link) => link.dispose() }
        links.retain((_, _) => false) // no `clear` method
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      val sources0  = sources.swap(Set.empty)
      val sinks0    = sinks  .swap(Set.empty)
      disposeLinks()
      sources0.foreach(_.removeSink  (this))
      sinks0  .foreach(_.removeSource(this))
    }
  }
}