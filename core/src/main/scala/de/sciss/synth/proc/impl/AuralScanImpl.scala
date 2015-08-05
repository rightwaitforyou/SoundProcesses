/*
 *  AuralScanImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.{AudioBus, NodeRef, Synth, Sys, expr}
import de.sciss.synth.proc.AuralObj.ProcData
import de.sciss.synth.proc.Scan.Link
import de.sciss.synth.proc.{logAural => logA}
import de.sciss.synth.{SynthGraph, addBefore}

import scala.concurrent.stm.{Ref, TMap}

object AuralScanImpl {
  def apply[S <: Sys[S]](data: ProcData[S], key: String, scan: Scan[S], bus: AudioBus, isInput: Boolean)
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralScan.Owned[S] = {
    val id    = scan.id
    import expr.IdentifierSerializer
    val view  = new Impl[S](data = data, key = key, bus = bus, idH = tx.newHandle(id))
    logA(s"AuralScan(${data.procCached()}, $key, bus = $bus, isInput = $isInput)")
    context.putAux[AuralScan.Proxy[S]](id, view)

    def scanView(peer: S#ID)(implicit tx: S#Tx): Option[AuralScan[S]] =
      context.getAux[AuralScan.Proxy[S]](peer) match {
        case Some(view: AuralScan[S]) => Some(view)
        case _                        => None
      }

    def scanViewProxy(peer: S#ID)(implicit tx: S#Tx): Option[AuralScan.Proxy[S]] =
      context.getAux[AuralScan.Proxy[S]](peer)

    scan.iterator.foreach {
      case Link.Grapheme(peer) => // XXX TODO: currently not supported
      case Link.Scan    (peer) =>
        if (isInput)
          scanView(peer.id).foreach { sourceView =>
            sourceView.addSink  (view      )
            view      .addSource(sourceView)
          }

        else
          scanViewProxy(peer.id).foreach {
            case sinkView: AuralScan[S] =>
              view    .addSink  (sinkView)
              sinkView.addSource(view    )
            case proxy: AuralScan.Incomplete[S] =>
              proxy.data.sinkAdded(proxy.key, view)
          }
    }

    // the observer registers source and sink additions and removals.
    // if a view is found for the remote scan, simply invoke the
    // the corresponding add/remove method on our view. do not call
    // into the remote view, because it will from its own side observe
    // this event and call into the symmetric method.
    val obs = scan.changed.react { implicit tx => upd =>
      upd.changes.foreach {
        case Scan.Added(peer) =>
          if (isInput)
            scanView(peer.id).foreach(view.addSource   )
          else
            scanViewProxy(peer.id).foreach {
              case sinkView: AuralScan[S] => view.addSink(sinkView)
              case proxy: AuralScan.Incomplete[S] => proxy.data.sinkAdded(proxy.key, view)
            }

        case Scan.Removed(peer) =>
          if (isInput)
            scanView(peer.id).foreach(view.removeSource)
          else
            scanView(peer.id).foreach(view.removeSink)
      }
    }

    view.obs = obs  // will be disposed with the view
    view
  }

  // ----------------------------------

  private def LinkNode[S <: Sys[S]](source: AuralScan[S], sourceNode: NodeRef,
                                    sink  : AuralScan[S], sinkNode  : NodeRef)(implicit tx: S#Tx): LinkNode[S] = {
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

    val synth     = Synth(sourceBus.server, g, nameHint = Some(s"a-ln$numCh"))
    val synthRef  = synth: NodeRef
    synth.play(target = sinkNode.node, args = Nil, addAction = addBefore, dependencies = Nil)
    synth.read (sourceBus -> "in" )
    synth.write(sinkBus   -> "out")

    val edge1     = NodeRef.Edge(sourceNode, synthRef)
    val edge2     = NodeRef.Edge(synthRef  , sinkNode)
    server.addVertex(synthRef)
    val foo1 = server.addEdge(edge1).get._2
    val foo2 = server.addEdge(edge2).get._2

    println("EDGE 1")
    println(foo1)
    println("EDGE 2")
    println(foo1)

    new LinkNode(edge1, edge2, synthRef)
  }
  private final class LinkNode[S <: Sys[S]](edge1: NodeRef.Edge, edge2: NodeRef.Edge, synthRef: NodeRef)
    extends Disposable[S#Tx] {

    override def toString = s"LinkNode($edge1, $edge2, $synthRef)"

    def dispose()(implicit tx: S#Tx): Unit = {
      val server = synthRef.server
      server.removeEdge(edge1)
      server.removeEdge(edge2)
      server.removeVertex(synthRef)
      synthRef.node.free()
    }
  }

  // ----------------------------------

  // note: it is crucial that we use `stm.Source[S#Tx, S#ID]` instead of just `S#ID`, because if
  // the view is created in the same transaction as the scan, the id's path will be empty, causing
  // an error in `dispose()` when trying to remove the entry from the ID map!
  private final class Impl[S <: Sys[S]](val data: ProcData[S], val key: String, val bus: AudioBus,
                                        idH: stm.Source[S#Tx, S#ID])
    extends AuralScan.Owned[S]  {

    private val sources = Ref(Set.empty[AuralScan[S]])
    private val sinks   = Ref(Set.empty[AuralScan[S]])
    private val links   = TMap.empty[AuralScan[S], LinkNode[S]] // key = sink

    private[AuralScanImpl] var obs: Disposable[S#Tx] = _

    def addSource(source: AuralScan[S])(implicit tx: S#Tx): Unit = {
      logA(s"AuralScan addSource   (${source.data.procCached()}, ${source.key}); ${data.procCached()}, $key")
      sources.transform(_ + source)(tx.peer)
    }

    def addSink(sink: AuralScan[S])(implicit tx: S#Tx): Unit = {
      logA(s"AuralScan addSink     (${sink.data.procCached()}, ${sink.key}); ${data.procCached()}, $key")
      sinks.transform(_ + sink)(tx.peer)
      sinkPlaying(sink)
    }

    private def tryLink(sourceNode: NodeRef, sink: AuralScan[S])(implicit tx: S#Tx): Unit =
      sink.data.nodeOption.foreach { sinkNode =>
        implicit val itx = tx.peer
        if (!links.contains(sink)) {
          val link = LinkNode[S](this, sourceNode, sink, sinkNode)
          logA(s"AuralScan link; ${data.procCached()}, link")
          links.put(sink, link)
        }
      }

    def removeSource(source: AuralScan[S])(implicit tx: S#Tx): Unit = {
      logA(s"AuralScan removeSource(${source.data.procCached()}, ${source.key}); ${data.procCached()}, $key")
      sources.transform(_ - source)(tx.peer)
    }

    def removeSink(sink: AuralScan[S])(implicit tx: S#Tx): Unit = {
      logA(s"AuralScan removeSink  (${sink.data.procCached()}, ${sink.key}); ${data.procCached()}, $key")
      sinks.transform(_ - sink)(tx.peer)
      sinkStopped(sink)
    }

    def play(n: NodeRef)(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      logA(s"AuralScan play; ${data.procCached()}, $key")
      stop1()
      sinks().foreach { sink =>
        tryLink(n, sink)
      }
      sources().foreach { source =>
        source.sinkPlaying(this)
      }
    }

    def stop()(implicit tx: S#Tx): Unit = {
      logA(s"AuralScan stop; ${data.procCached()}, $key")
      stop1()
    }

    private def stop1()(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      if (!links.isEmpty) {
        links.foreach { case (_, link) => link.dispose() }
        links.clear()
      }
      sources().foreach { source =>
        source.sinkStopped(this)
      }
    }

    def sinkPlaying(sink: AuralScan[S])(implicit tx: S#Tx): Unit =
      data.nodeOption.foreach { sourceNode =>
        tryLink(sourceNode, sink)
      }

    def sinkStopped(sink: AuralScan[S])(implicit tx: S#Tx): Unit =
      links.remove(sink)(tx.peer).foreach { link =>
        link.dispose()
      }

    def dispose()(implicit tx: S#Tx): Unit = {
      logA(s"AuralScan dispose; ${data.procCached()}, $key")
      data.context.removeAux(idH())
      obs.dispose()
      val sources0  = sources.swap(Set.empty)(tx.peer)
      val sinks0    = sinks  .swap(Set.empty)(tx.peer)
      stop1()
      sources0.foreach(_.removeSink  (this))
      sinks0  .foreach(_.removeSource(this))
    }
  }
}