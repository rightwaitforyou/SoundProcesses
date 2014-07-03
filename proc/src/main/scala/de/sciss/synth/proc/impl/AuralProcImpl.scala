package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm
import de.sciss.lucre.{event => evt}
import de.sciss.lucre.synth._
import de.sciss.processor.GenericProcessor
import de.sciss.span.SpanLike
import de.sciss.synth.impl.BasicUGenGraphBuilder
import de.sciss.synth.{UGenGraph, UGen}
import de.sciss.synth.proc.impl.UGenGraphBuilder.{MissingIn, ScanIn, StreamIn}
import de.sciss.synth.proc.{logAural => logA}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{TxnUnknown, InTxn, TMap, Ref}

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

    private def tryBuild()(implicit tx: S#Tx): Unit = {

    }

    // ---- proc UGenGraphBuilder

    /** This method should only be invoked by the `graph.scan.Elem` instances. It requests a scan input, and
      * the method returns the corresponding number of channels, or throws a `MissingIn` exception which
      * is then caught by the main builder body.
      *
      * @param  key           the scan input key
      * @param  numChannels   a given number of channels (if `>= 0`) or `-1` in which case the number of channels
      *                       is determined using the scan.
      */
    def addScanIn(key: String, numChannels: Int): Int = ???

    /** This method should only be invoked by the `graph.attribute.In` instances. It registers a control input. */
    def addAttributeIn(key: String): Int = {
      implicit val tx = findTxn()
      timed.value.attr.getElem(key).fold(1) {
        case a: DoubleVecElem[S]      => a.peer.value.size // XXX TODO: would be better to write a.peer.size.value
        case a: AudioGraphemeElem[S]  => a.peer.spec.numChannels
        case _ => 1
      }
    }

    /** This method should only be invoked by the `graph.stream.X` instances. It registers a control input
      * for a streaming buffer.
      *
      * @return tuple consisting of `_1` number of channel, and `_2` control name index
      */
    def addStreamIn(key: String, info: StreamIn): (Int, Int) = ???

    /** This method should only be invoked by the `graph.scan.Elem` instances. It declares a scan output along
      * with the number of channels written to it.
      */
    def addScanOut(key: String, numChannels: Int): Unit = ???

    // ---- UGenGraph.Builder

    private def findTxn(): InTxn = concurrent.stm.Txn.findCurrent(TxnUnknown).get

    def visit[U](ref: AnyRef, init: => U): U = {
      implicit val tx = findTxn()
      ubSrcMap.get(ref).getOrElse {
        val exp = init
        ubSrcMap.put(ref, exp)
        exp
      } .asInstanceOf[U]
    }

    def addUGen(ugen: UGen): Unit = {
      implicit val tx = findTxn()
      ubUGens.transform(_ :+ ugen)
    }

    def addControl(values: Vec[Float], name: Option[String]): Int = {
      implicit val tx = findTxn()
      val specialIndex = ubCtlVals.getAndTransform(_ ++ values).size
      name.foreach(n => ubCtlNames.transform(_ :+ n -> specialIndex))
      specialIndex
    }

    private val ubUGens       = Ref(Vector.empty[UGen])
    private val ubCtlVals     = Ref(Vector.empty[Float])
    private val ubCtlNames    = Ref(Vector.empty[(String, Int)])
    private val ubSrcMap      = TMap.empty[AnyRef, Any]

    //    private val ubScanOuts    = TMap.empty[String, Int]
    //    private val ubScanIns     = TMap.empty[String, ScanIn]
    //    private val ubMissingIns  = Ref(Set.empty[MissingIn[S]])
    //    private val ubAttrIns     = Ref(Set.empty[String])
    //    private val ubStreamIns   = TMap.empty[String, List[StreamIn]]

    private final class UGenBuilder(attr: AttrMap[S], ugens0: Vec[UGen], controlValues0: Vec[Float],
                                    controlNames0: Vec[(String, Int)], sourceMap0: Map[AnyRef, Any])
      extends BasicUGenGraphBuilder with UGenGraphBuilder {

      override protected var ugens        : Vec[UGen]           = ugens0
      override protected var controlValues: Vec[Float]          = controlValues0
      override protected var controlNames : Vec[(String, Int)]  = controlNames0
      override protected var sourceMap    : Map[AnyRef, Any]    = sourceMap0

      def addScanIn  (key: String, numChannels: Int): Int = ???
      def addScanOut (key: String, numChannels: Int): Unit = ???

      def addStreamIn(key: String, info: StreamIn): (Int, Int) = ???

      def addAttributeIn(key: String): Int = ???
    }
  }
}