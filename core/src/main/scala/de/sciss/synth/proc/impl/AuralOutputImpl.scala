/*
 *  AuralOutputImpl.scala
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
import de.sciss.lucre.synth.Txn
import de.sciss.lucre.synth.{AudioBus, NodeRef, Sys}
import de.sciss.synth.proc.AuralObj.ProcData
import de.sciss.synth.proc.{logAural => logA}

import scala.concurrent.stm.{TMap, TSet}

object AuralOutputImpl {
  def apply[S <: Sys[S]](data: ProcData[S], output: Output[S], bus: AudioBus)
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralOutput.Owned[S] = {
    val id    = output.id
    val key   = output.key
    val view  = new Impl[S](data = data, key = key, bus = bus, idH = tx.newHandle(id))
    logA(s"AuralOutput(${data.procCached()}, $key, bus = $bus)")
    context.putAux[AuralOutput /* .Proxy */[S]](id, view)

// SCAN
//    def scanView(peer: S#ID)(implicit tx: S#Tx): Option[AuralOutput[S]] =
//      context.getAux[AuralOutput.Proxy[S]](peer) match {
//        case Some(view: AuralOutput[S]) => Some(view)
//        case _                        => None
//      }
//
//    def scanViewProxy(peer: S#ID)(implicit tx: S#Tx): Option[AuralOutput.Proxy[S]] =
//      context.getAux[AuralOutput.Proxy[S]](peer)

// SCAN
//    scan.iterator.foreach {
//// SCAN
////      case Link.Grapheme(peer) => // XXX TODO: currently not supported
//      case Link.Scan(peer) =>
//        if (isInput)
//          scanView(peer.id).foreach { sourceView =>
//            sourceView.addSink  (view      )
//            view      .addSource(sourceView)
//          }
//
//        else
//          scanViewProxy(peer.id).foreach {
//            case sinkView: AuralOutput[S] =>
//              view    .addSink  (sinkView)
//              sinkView.addSource(view    )
//            case proxy: AuralOutput.Incomplete[S] =>
//              proxy.data.sinkAdded(proxy.key, view)
//          }
//    }


// SCAN
//    // the observer registers source and sink additions and removals.
//    // if a view is found for the remote scan, simply invoke the
//    // the corresponding add/remove method on our view. do not call
//    // into the remote view, because it will from its own side observe
//    // this event and call into the symmetric method.
//    val obs = scan.changed.react { implicit tx => upd =>
//      upd.changes.foreach {
//        case Output.Added(link) =>
//          if (isInput)
//            scanView(link.peerID).foreach(view.addSource   )
//          else
//            scanViewProxy(link.peerID).foreach {
//              case sinkView: AuralOutput[S] => view.addSink(sinkView)
//              case proxy: AuralOutput.Incomplete[S] => proxy.data.sinkAdded(proxy.key, view)
//            }
//
//        case Output.Removed(link) =>
//          if (isInput)
//            scanView(link.peerID).foreach(view.removeSource)
//          else
//            scanView(link.peerID).foreach(view.removeSink)
//      }
//    }
//
//    view.obs = obs  // will be disposed with the view
    view
  }

  // ----------------------------------

  private def LinkNode[S <: Sys[S]](source: AuralOutput[S], sourceNode: NodeRef,
                                    sink  : AuralInput [S], sinkNode  : NodeRef)(implicit tx: Txn): LinkNode = {
    val sourceBus = source.bus
//    val sinkBus   = sink  .bus
//    val sourceCh  = sourceBus.numChannels
//    val sinkCh    = sinkBus  .numChannels
//    val numCh     = math.min(sourceCh, sinkCh)

    val server    = sourceBus.server
//    if (sinkBus.server != server) throw new IllegalArgumentException("Trying to link nodes across servers")

//    val g = SynthGraph {
//      import de.sciss.synth._
//      import ugen._
//
//      val sig = InFeedback.ar("in".kr, numCh)
//      Out.ar("out".kr, sig)
//    }
//
//    val synth     = Synth(sourceBus.server, g, nameHint = Some(s"a-ln$numCh"))
//    val synthRef  = synth: NodeRef
//    synth.play(target = sinkNode.node, args = Nil, addAction = addBefore, dependencies = Nil)
//    synth.read (sourceBus -> "in" )
//    synth.write(sinkBus   -> "out")

//    val edge1       = NodeRef.Edge(sourceNode, synthRef)
//    val edge2       = NodeRef.Edge(synthRef  , sinkNode)
//    server.addVertex(synthRef)
//    val edge1Added  = server.addEdge(edge1)
//    val edge2Added  = server.addEdge(edge2)

    val edge      = NodeRef.Edge(sourceNode, sinkNode)
    val edgeAdded = server.addEdge(edge)
    logA(s"link: addEdge $edge ? $edgeAdded")

    new LinkNode(edge = edge, edgeAdded = edgeAdded)
  }

  private final class LinkNode(edge: NodeRef.Edge, edgeAdded: Boolean)
    extends Disposable[Txn] {

//    override def toString =
//      s"LinkNode($edge1${if (edge1Added) "" else " !"}, $edge2${if (edge2Added) "" else " !"}, $synthRef)"

    override def toString =
      s"LinkNode($edge${if (edgeAdded) "" else " !"})"

    def dispose()(implicit tx: Txn): Unit = {
      val server = edge.source.server
      if (edgeAdded) {
        logA(s"link: removeEdge $edge")
        server.removeEdge(edge)
      }
      // if (edge2Added) server.removeEdge(edge2)
      // server.removeVertex(synthRef)
      // synthRef.node.free()
    }
  }

  // ----------------------------------

  // note: it is crucial that we use `stm.Source[S#Tx, S#ID]` instead of just `S#ID`, because if
  // the view is created in the same transaction as the scan, the id's path will be empty, causing
  // an error in `dispose()` when trying to remove the entry from the ID map!
  private final class Impl[S <: Sys[S]](val data: ProcData[S], val key: String, val bus: AudioBus,
                                        idH: stm.Source[S#Tx, S#ID])
    extends AuralOutput.Owned[S]  {

    override def toString: String = s"AuralOutput($data, $key, $bus)"

    // private val sources = Ref(Set.empty[AuralOutput[S]])
    private val sinks   = TSet.empty[AuralInput[S]]
    private val links   = TMap.empty[AuralInput[S], LinkNode] // key = sink

    private[AuralOutputImpl] var obs: Disposable[S#Tx] = _

// SCAN
//    def addSource(source: AuralOutput[S])(implicit tx: S#Tx): Unit = {
//      logA(s"AuralOutput addSource   (${source.data.procCached()}, ${source.key}); ${data.procCached()}, $key")
//      sources.transform(_ + source)(tx.peer)
//    }

    def addSink(sink: AuralInput[S])(implicit tx: Txn): Unit = {
      // logA(s"AuralOutput addSink     (${sink.key}); ${data.procCached()}, $key")
      implicit val itx = tx.peer
      logA(s"AuralOutput addSink     ($sink); $data, $key")
      sinks.add(sink)
      sinkPlaying(sink)
    }

    private def tryLink(sourceNode: NodeRef, sink: AuralInput[S])(implicit tx: Txn): Unit =
      sink.nodeRef.foreach { sinkNode =>
        implicit val itx = tx.peer
        if (!links.contains(sink)) {
          val link = LinkNode[S](this, sourceNode, sink, sinkNode)
          logA(s"AuralOutput link; $data, link")
          links.put(sink, link)
        }
      }

// SCAN
//    def removeSource(source: AuralOutput[S])(implicit tx: S#Tx): Unit = {
//      logA(s"AuralOutput removeSource(${source.data.procCached()}, ${source.key}); ${data.procCached()}, $key")
//      sources.transform(_ - source)(tx.peer)
//    }

    def removeSink(sink: AuralInput[S])(implicit tx: Txn): Unit = {
      implicit val itx = tx.peer
      // logA(s"AuralOutput removeSink  (${sink.key}); ${data.procCached()}, $key")
      logA(s"AuralOutput removeSink  ($sink); $data, $key")
      sinks.remove(sink)
      sinkStopped(sink)
    }

    def play(n: NodeRef)(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      logA(s"AuralOutput play; ${data.procCached()}, $key")
      stop1()
      sinks.foreach { sink =>
        tryLink(n, sink)
      }
// SCAN
//      sources().foreach { source =>
//        source.sinkPlaying(this)
//      }
    }

    def stop()(implicit tx: S#Tx): Unit = {
      logA(s"AuralOutput stop; ${data.procCached()}, $key")
      stop1()
    }

    private def stop1()(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      if (!links.isEmpty) {
        links.foreach { case (_, link) => link.dispose() }
        links.clear()
      }
// SCAN
//      sources().foreach { source =>
//        source.sinkStopped(this)
//      }
    }

    private def sinkPlaying(sink: AuralInput[S])(implicit tx: Txn): Unit =
      data.nodeOption.foreach { sourceNode =>
        tryLink(sourceNode, sink)
      }

    private def sinkStopped(sink: AuralInput[S])(implicit tx: Txn): Unit =
      links.remove(sink)(tx.peer).foreach { link =>
        link.dispose()
      }

    def dispose()(implicit tx: S#Tx): Unit = {
      logA(s"AuralOutput dispose; ${data.procCached()}, $key")
      implicit val itx = tx.peer
      data.context.removeAux(idH())
      obs.dispose()
// SCAN
//      val sources0  = sources.swap(Set.empty)(tx.peer)
//      val sinks0    = sinks  .swap(Set.empty)(tx.peer)
      sinks.clear()
      stop1()
// SCAN
//      sources0.foreach(_.removeSink  (this))
//      sinks0  .foreach(_.removeSource(this))
    }
  }
}