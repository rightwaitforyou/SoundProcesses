package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm
import de.sciss.lucre.synth.impl
import de.sciss.lucre.{event => evt}
import de.sciss.lucre.synth._
import de.sciss.processor.GenericProcessor
import de.sciss.span.SpanLike
import de.sciss.synth.impl.BasicUGenGraphBuilder
import de.sciss.synth.proc.Scan.Link
import de.sciss.synth.{UGenGraph, UGen}
import de.sciss.synth.proc.impl.UGenGraphBuilder.{MissingIn, ScanIn, StreamIn}
import de.sciss.synth.proc.{logAural => logA}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm._

object AuralProcImpl extends AuralObj.Factory {
  type E[S <: evt.Sys[S]] = Proc.Elem[S]

  def typeID = ElemImpl.Proc.typeID

  def apply[S <: Sys[S]](proc: Obj.T[S, Proc.Elem])(implicit tx: S#Tx): AuralObj.Proc[S] = {
    ???
  }

  private final class OutputBuilder(val bus: AudioBus) {
    var sinks = List.empty[(String, AuralNode)]
  }

  private final class AuralProcBuilder[S <: Sys[S]](val ugen: UGenGraphBuilder[S] /*, val name: String */) {
    var outputs = Map.empty[String, OutputBuilder]
  }

  private final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, Proc.Elem]])
    extends AuralObj.Proc[S] {

    private def server: Server = ???

    def typeID: Int = Proc.typeID

    def latencyEstimate(implicit tx: S#Tx): Long = ???

    //    private def addFlush()(implicit tx: S#Tx): Unit = {
    //      logA(s"addFlush (${hashCode.toHexString})")
    //      tx.beforeCommit(flush()(_))
    //      // concurrent.stm.Txn.afterRollback(status => logA(s"rollback $status !!"))(tx.peer)
    //    }
    //
    //    // called before the transaction successfully completes.
    //    // this is the place where we launch completely built procs.
    //    private def flush()(ptx: Txn): Unit = {
    //      val itx = ptx.peer
    //      ongoingBuild.get(itx).seq.foreach { builder =>
    //        val ugen = builder.ugen
    //        if (ugen.isComplete) {
    //          try {
    //            launchProc(builder)
    //          } catch {
    //            case NonFatal(e) =>
    //              e.printStackTrace()
    //              throw e
    //          }
    //
    //        } else {
    //          // XXX TODO: do we need to free buses associated with ugen.scanOuts ?
    //          println("Warning: Incomplete aural proc build for " + ugen.timed.value)
    //        }
    //      }
    //    }

    //    private def procAdded(time: Long, timed: TimedProc[S])(implicit tx: S#Tx): Unit = {
    //      logA(s"added $timed (${hashCode.toHexString})")
    //
    //      val timedID = timed.id
    //      val ugen    = UGenGraphBuilder(this)
    //      val builder = new AuralProcBuilder(ugen /*, name */)
    //      //      val newTxn  = !ongoingBuild.isInitialized(tx.peer)
    //      //      if (newTxn) addFlush() // ( ProcTxn() )   // the next line (`ongoingBuild.get`) will initialise then
    //      //      val ongoing = ongoingBuild.get(tx.peer)
    //      //      ongoing.seq :+= builder
    //      //      // assert(ongoingBuild.isInitialized(tx.peer))
    //
    //      //      // initialise the id-to-builder map if necessary
    //      //      val builderMap = ongoing.idMap.getOrElse {
    //      //        val m = tx.newInMemoryIDMap[AuralProcBuilder[S]]
    //      //        ongoing.idMap = Some(m)
    //      //        m
    //      //      }
    //      //      // add the builder to it.
    //      //      builderMap.put(timedID, builder)
    //
    //      // store the look up information for the scans
    //      // (this is only needed because Scan.Link.Scan reveals
    //      // only the Scan which in turn doesn't currently carry
    //      // key and proc information, so it can't be recovered
    //      // otherwise; in the future this may change)
    //      val proc  = timed.value.elem.peer
    //      val scans = proc.scans
    //      scans.iterator.foreach {
    //        case (key, scan) =>
    //          import de.sciss.lucre.synth.expr.IdentifierSerializer
    //          scanMap.put(scan.id, key -> tx.newHandle(timedID))
    //      }
    //
    //      incrementalBuild(ongoing, builder)
    //    }

    def play(time: SpanLike)(implicit tx: S#Tx): Unit = ???
    def stop(time: Long)(implicit tx: S#Tx): Unit = ???

    def isPrepared(implicit tx: S#Tx): Boolean = ???

    def prepare()(implicit tx: S#Tx): GenericProcessor[Unit] = ???

    //    private def tryBuild()(implicit tx: S#Tx): Unit = {
    //
    //    }

    private val procLoc = TxnLocal[Obj.T[S, Proc.Elem]]()

    private def procCached()(implicit tx: S#Tx): Obj.T[S, Proc.Elem] = {
      implicit val itx = tx.peer
      if (procLoc.isInitialized) procLoc.get
      else {
        val proc = obj()
        procLoc.set(proc)
        proc
      }
    }

    // called by UGenGraphBuilderImpl
    def attrNumChannels(key: String)(implicit tx: S#Tx): Int = {
      val procObj = procCached()
      procObj.attr.getElem(key).fold(1) {
        case a: DoubleVecElem[S]     => a.peer.value.size // XXX TODO: would be better to write a.peer.size.value
        case a: AudioGraphemeElem[S] => a.peer.spec.numChannels
        case _ => 1
      }
    }
    // called by UGenGraphBuilderImpl
    def scanInNumChannels(key: String, numChannels: Int)(implicit tx: S#Tx): Int = {
      val procObj = procCached()
      val proc    = procObj.elem.peer
      val numCh   = proc.scans.get(key).fold(0) { scan =>
        val chans = scan.sources.toList.map {
          case Link.Grapheme(peer) =>
            // val chansOpt = peer.valueAt(time).map(_.numChannels)
            // chansOpt.getOrElse(numChannels)
            peer.numChannels

          case Link.Scan(peer) => ???
//            val sourceOpt = scanMap.get(peer.id)
//            val busOpt    = sourceOpt.flatMap {
//              case (sourceKey, idH) =>
//                val sourceTimedID = idH()
//                getOutputBus(sourceTimedID, sourceKey)
//            }
//            busOpt.fold({
//              if (numChannels < 0) throw MissingIn(peer)
//              numChannels
//            })(_.numChannels)
        }
        if (chans.isEmpty) 0 else chans.max
      }
      math.max(1, numCh)
    }
  }
}