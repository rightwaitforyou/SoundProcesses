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
    import context.server
    val bus   = Bus.audio(server, numChannels = numChannels)
    val view  = new Impl[S](key = key, bus = bus)
    context.putAux(scan.id, view)

    scan.sources.foreach {
      case Link.Grapheme(peer) =>
      case Link.Scan    (peer) =>
        context.getAux[AuralScan[S]](peer.id).foreach { sourceView =>
          sourceView.addSink  (view      )
          view      .addSource(sourceView)
        }
    }
    scan.sinks.foreach {
      case Link.Grapheme(peer) => // XXX TODO: currently not supported
      case Link.Scan    (peer) =>
        context.getAux[AuralScan[S]](peer.id).foreach { sinkView =>
          view    .addSink  (sinkView)
          sinkView.addSource(view    )
        }
    }

    // the observer registers source and sink additions and removals.
    // if a view is found for the remote scan, simply invoke the
    // the corresponding add/remove method on our view. do not call
    // into the remote view, because it will from its own side observe
    // this event and call into the symmetric method.
    val obs = scan.changed.react { implicit tx => upd =>
      upd.changes.foreach {
        case Scan.SourceAdded  (peer) => context.getAux[AuralScan[S]](peer.id).foreach(view.addSource   )
        case Scan.SourceRemoved(peer) => context.getAux[AuralScan[S]](peer.id).foreach(view.removeSource)
        case Scan.SinkAdded    (peer) => context.getAux[AuralScan[S]](peer.id).foreach(view.addSink     )
        case Scan.SinkRemoved  (peer) => context.getAux[AuralScan[S]](peer.id).foreach(view.removeSink  )
        case Scan.GraphemeChange(_, _) => // XXX TODO: currently not supported
      }
    }

    view.obs = obs  // will be disposed with the view
    view
  }

  // ----------------------------------

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

  // ----------------------------------

  private final class Impl[S <: Sys[S]](val key: String, val bus: AudioBus)
    extends AuralScan.Owned[S]  {

    private val sources = Ref(Set.empty[AuralScan[S]])
    private val sinks   = Ref(Set.empty[AuralScan[S]])
    private val links   = TMap.empty[AuralScan[S], LinkNode[S]] // key = sink

    private[AuralScanImpl] var obs: Disposable[S#Tx] = _

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
      obs.dispose()
      val sources0  = sources.swap(Set.empty)
      val sinks0    = sinks  .swap(Set.empty)
      disposeLinks()
      sources0.foreach(_.removeSink  (this))
      sinks0  .foreach(_.removeSource(this))
    }
  }
}