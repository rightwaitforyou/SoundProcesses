/*
 *  AuralProcImpl.scala
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

import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.expr.StringObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.{AudioBus, AuralNode, Buffer, BusNodeSetter, Synth, Sys}
import de.sciss.span.Span
import de.sciss.synth.proc.AuralObj.{Playing, Prepared, Preparing, ProcData, Stopped, TargetPlaying, TargetPrepared, TargetState, TargetStop}
import de.sciss.synth.proc.Timeline.SampleRate
import de.sciss.synth.proc.{UGenGraphBuilder => UGB, logAural => logA}

import scala.concurrent.Future
import scala.concurrent.stm.Ref

object AuralProcImpl {
  def apply[S <: Sys[S]](proc: Proc[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Proc[S] = {
    val data  = AuralProcDataImpl(proc)
    val res   = new Impl[S]
    res.init(data)
  }

  private final class OutputBuilder(val bus: AudioBus) {
    var sinks = List.empty[(String, AuralNode)]
  }

  private final class AuralProcBuilder(val ugen: UGB /*, val name: String */) {
    var outputs = Map.empty[String, OutputBuilder]
  }

  // ---------------------------------------------------------------------

  class Impl[S <: Sys[S]](implicit context: AuralContext[S])
    extends AuralObj.Proc[S] with ObservableImpl[S, AuralObj.State] {

    import context.scheduler.cursor
    import context.server

    /* The ongoing build aural node build process, as stored in `playingRef`. */
    private sealed trait PlayingRef extends Disposable[S#Tx]

    private object PlayingNone extends PlayingRef {
      def dispose()(implicit tx: S#Tx) = ()
    }
    private final class PlayingNode(val node: AuralNode) extends PlayingRef {
      def dispose()(implicit tx: S#Tx): Unit = {
        _data.removeInstanceNode(node)
        node.dispose()
      }
    }
    private final class PlayingPrepare(val resources: List[AsyncResource[S]]) extends PlayingRef {
      def dispose()(implicit tx: S#Tx): Unit = resources.foreach(_.dispose())
    }

    private var _data: ProcData[S]  = _
    // XXX TODO - perhaps `currentStateRef` and `playingRef` could be one thing?
    private val currentStateRef     = Ref[AuralObj.State](AuralObj.Stopped)
    private val targetStateRef      = Ref[TargetState   ](TargetStop      )
    private val playingRef          = Ref[PlayingRef    ](PlayingNone     )

    final def obj: stm.Source[S#Tx, Proc[S]] = _data.obj

    final def typeID: Int = Proc.typeID

    // def latencyEstimate(implicit tx: S#Tx): Long = ...

    override def toString = s"AuralObj.Proc@${hashCode().toHexString}"

    /** Sub-classes may override this if invoking the super-method. */
    def init(data: ProcData[S])(implicit tx: S#Tx): this.type = {
      this._data = data
      data.addInstanceView(this)
      this
    }

    final def state      (implicit tx: S#Tx): AuralObj.State = currentStateRef.get(tx.peer)
    final def targetState(implicit tx: S#Tx): AuralObj.State = targetStateRef .get(tx.peer).completed

    private def state_=(value: AuralObj.State)(implicit tx: S#Tx): Unit = {
      val old = currentStateRef.swap(value)(tx.peer)
      if (value != old) {
        // println(s"------PROC STATE $old > $value")
        fire(value)
      }
    }

    def data: ProcData[S] = _data

    final def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      targetStateRef.set(TargetPrepared)(tx.peer)
      // XXX TODO
    }

    final def play(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      val ts = TargetPlaying(context.scheduler.time, timeRef)
      targetStateRef.set(ts)(tx.peer)
      _data.state match {
        case s: UGB.Complete[S] =>
          state match {
            case Stopped   => prepareAndLaunch(s, timeRef)
            case Prepared  => launch          (s, timeRef)
            case _ =>
          }

        case _ =>
      }
    }

    // same as `play` but reusing previous `timeRef`
    final def playAfterRebuild()(implicit tx: S#Tx): Unit = {
      if (state != Stopped) return

      (_data.state, targetStateRef.get(tx.peer)) match {
        case (s: UGB.Complete[S], tp: TargetPlaying) =>
          prepareAndLaunch(s, tp.shiftTo(context.scheduler.time))
        case _ =>
      }
    }

    final def stop(/* time: Long */)(implicit tx: S#Tx): Unit = {
      targetStateRef.set(TargetStop)(tx.peer)
      stopForRebuild()
    }

    // same as `stop` but not touching target state
    final def stopForRebuild()(implicit tx: S#Tx): Unit = {
      freePlayingRef()
      state = Stopped
    }

    // def prepare()(implicit tx: S#Tx): Unit = ...

    /** Sub-classes may override this if invoking the super-method. */
    def dispose()(implicit tx: S#Tx): Unit = {
      freePlayingRef()
      _data.removeInstanceView(this)
      context.release(_data.procCached())
    }

    /** Sub-classes may override this if falling back to the super-method. */
    protected def buildAsyncInput(b: AsyncProcBuilder[S], keyW: UGB.Key, value: UGB.Value)
                                 (implicit tx: S#Tx): Unit = keyW match {
      case UGB.AttributeKey(key) => buildAsyncAttrInput(b, key, value)
      case _                     => throw new IllegalStateException(s"Unsupported async input request $keyW")
    }

    /** Sub-classes may override this if falling back to the super-method. */
    protected def buildSyncInput(b: SynthBuilder[S], keyW: UGB.Key, value: UGB.Value)
                                (implicit tx: S#Tx): Unit = keyW match {
      case UGB.AttributeKey(key) =>
        _data.buildAttrInput(b, key, value)
//        b.storeKey(key)

// SCAN
//      case UGB.ScanKey(key) =>
//        buildScanInput(b, key, value)

      case _ =>
        throw new IllegalStateException(s"Unsupported input request $keyW")
    }

// SCAN
//    /** Sub-classes may override this if falling back to the super-method. */
//    protected def buildScanInput(b: SynthBuilder[S], key: String, value: UGB.Value)
//                                (implicit tx: S#Tx): Unit = value match {
//      case UGB.Input.Scan.Value(numCh) =>
//        // ---- scans ----
//        // XXX TODO : this should all disappear
//        // and the missing bits should be added
//        // to AuralScan
//
//        // val numCh = scanIn.numChannels
//
//        @inline def ensureChannels(n: Int): Unit =
//          if (n != numCh)
//            throw new IllegalStateException(s"Scan input changed number of channels (expected $numCh but found $n)")
//
//        val inCtlName = graph.ScanIn.controlName(key)
//        // var inBus     = Option.empty[AudioBusNodeSetter]
//
//        def mkInBus(): AudioBusNodeSetter = {
//          val bus    = _data.getScanBus(key) getOrElse sys.error(s"Scan bus $key not provided")
//          // val b      = Bus.audio(server, numCh)
//          logA(s"addInputBus($key, $b) (${hashCode.toHexString})")
//          val res    =
//          // if (scanIn.fixed)
//          //   BusNodeSetter.reader(inCtlName, b, synth)
//          // else
//            BusNodeSetter.mapper(inCtlName, bus, b.synth)
//          b.users ::= res
//          b.inputBuses += key -> bus
//          res
//        }
//
//        // note: if not found, stick with default
//
//        val time = b.timeRef.offsetOrZero
//
//        // XXX TODO: combination fixed + grapheme source doesn't work -- as soon as there's a bus mapper
//        //           we cannot use ControlSet any more, but need other mechanism
//// SCAN
////        b.obj.inputs.get(key).foreach { scan =>
////          val src = scan.iterator
////          if (src.isEmpty) {
////            // if (scanIn.fixed) lazyInBus  // make sure a fixed channels scan in exists as a bus
////            mkInBus()
////
////          } else {
////            src.foreach {
////              case Link.Grapheme(peer) =>
////                val segmOpt = peer.segment(time)
////                segmOpt.foreach {
////                  // again if not found... stick with default
////                  case const: Segment.Const =>
////                    ensureChannels(const.numChannels) // ... or could just adjust to the fact that they changed
////                    //                        setMap :+= ((key -> const.numChannels) : ControlSet)
////                    b.setMap += (if (const.numChannels == 1) {
////                      ControlSet.Value (inCtlName, const.values.head .toFloat )
////                    } else {
////                      ControlSet.Vector(inCtlName, const.values.map(_.toFloat))
////                    })
////
////                  case segm: Segment.Curve =>
////                    ensureChannels(segm.numChannels) // ... or could just adjust to the fact that they changed
////                    // println(s"segment : ${segm.span}")
////                    val bm          = mkInBus()
////                    val w           = SegmentWriter(bm.bus, segm, time, SampleRate)
////                    b.dependencies  ::= w
////
////                  case audio: Segment.Audio =>
////                    ensureChannels(audio.numChannels)
////                    val bm          = mkInBus()
////                    val w           = AudioArtifactWriter(bm.bus, audio, time)
////                    b.dependencies  ::= w
////                }
////
////              case Link.Scan(peer) => mkInBus()
////            }
////          }
////        }
//
//      case _ => throw new IllegalStateException(s"Unsupported input scan request $value")
//    }

    /** Sub-classes may override this if invoking the super-method. */
    protected def buildAsyncAttrInput(b: AsyncProcBuilder[S], key: String, value: UGB.Value)
                                     (implicit tx: S#Tx): Unit = value match {
      case UGB.Input.Buffer.Value(numFr, numCh, true) =>   // ----------------------- random access buffer
        b.obj.attr.get(key).fold[Unit] {
          sys.error(s"Missing attribute $key for buffer content")
        } {
          case a: Grapheme.Expr.Audio[S] =>
            val audioVal  = a.value
            val spec      = audioVal.spec
            val f         = audioVal.artifact
            val offset    = audioVal.offset
            // XXX TODO - for now, gain is ignored.
            // one might add an auxiliary control proxy e.g. Buffer(...).gain
            // val _gain     = audioElem.gain    .value
            if (spec.numFrames > 0x3FFFFFFF)
              sys.error(s"File too large for in-memory buffer: $f (${spec.numFrames} frames)")
            val bufSize   = spec.numFrames.toInt
            val buf       = Buffer(server)(numFrames = bufSize, numChannels = spec.numChannels)
            val cfg       = BufferPrepare.Config(f = f, spec = spec, offset = offset, buf = buf, key = key)
            b.resources ::= BufferPrepare[S](cfg)

          case a => sys.error(s"Cannot use attribute $a as a buffer content")
        }

      case _ =>
        throw new IllegalStateException(s"Unsupported input attribute request $value")
    }

    // ---- asynchronous preparation ----
    private def prepareAndLaunch(ugen: UGB.Complete[S], timeRef: TimeRef)
                       (implicit tx: S#Tx): Unit = {
      val p = _data.procCached()
      logA(s"begin prepare $p (${hashCode.toHexString})")

      val b = new AsyncProcBuilder(p)
      ugen.acceptedInputs.foreach { case (key, (_, value)) =>
        if (value.async) buildAsyncInput(b, key, value)
      }
      val res   = b.resources
      val done  = res.isEmpty
      if (done) {
        freePlayingRef()
        prepared(ugen)
      } else {
        val prep = setPlayingPrepare(res)
        tx.afterCommit {
          import SoundProcesses.executionContext
          val reduced = Future.reduce(res)((_, _) => ())
          reduced.foreach { _ =>
            cursor.step { implicit tx =>
              if (playingRef.get(tx.peer) == prep) {
                prepared(ugen)
              }
            }
          }
        }
      }
    }

    private def prepared(ugen: UGB.Complete[S])(implicit tx: S#Tx): Unit = {
      targetStateRef.get(tx.peer) match {
        case tp: TargetPlaying =>
          launch(ugen, tp.shiftTo(context.scheduler.time)) // XXX TODO - yes or no, shift time?
        case _ =>
          state = Prepared
      }
    }

    // ---- synchronous preparation ----
    private def launch(ugen: UGB.Complete[S], timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      val p = _data.procCached()
      logA(s"begin launch  $p (${hashCode.toHexString})")

      val ug            = ugen.result
      implicit val itx  = tx.peer

      val nameHint      = p.attr.$[StringObj](ObjKeys.attrName).map(_.value)
      val synth         = Synth.expanded(server, ug, nameHint = nameHint)

      val builder       = new SynthBuilder(p, synth, timeRef)

      // "consume" prepared state
      playingRef.swap(PlayingNone) match {
        case prep: PlayingPrepare => prep.resources.foreach { resource =>
          resource.install(builder)
        }
        case _ =>
      }

      // XXX TODO - it would be nicer if these were added optionally
      if (timeRef.frame        != 0) builder.setMap += graph.Time    .key -> (timeRef.frame        / SampleRate)
      if (timeRef.offsetOrZero != 0) builder.setMap += graph.Offset  .key -> (timeRef.offsetOrZero / SampleRate)
      timeRef.span match {
        case Span(start, stop) =>
          builder.setMap += graph.Duration.key -> ((stop - start) / SampleRate)
        case _ => // Double.PositiveInfinity
      }

      ugen.acceptedInputs.foreach { case (key, (_, value)) =>
        if (!value.async) buildSyncInput(builder, key, value)
      }

      // ---- handle output buses, and establish missing links to sinks ----
      ugen.outputs.foreach { case (key, numCh) =>
        val bus    = _data.getOutputBus(key) getOrElse sys.error(s"Scan bus $key not provided")
        logA(s"addOutputBus($key, $bus) (${hashCode.toHexString})")
        val res    = BusNodeSetter.writer(graph.ScanOut.controlName(key), bus, synth)
        builder.users ::= res
        builder.outputBuses += key -> bus
        // res
      }

      val node = builder.finish1() // .finish()
      val old = playingRef.swap(new PlayingNode(node))(tx.peer)
      old.dispose()
      _data.addInstanceNode(node)
      builder.finish2()
      logA(s"launched $p -> $node (${hashCode.toHexString})")
      state = Playing
      // setPlayingNode(node)
    }

//    private def setPlayingNode(node: AuralNode)(implicit tx: S#Tx): Unit = {
//      val old = playingRef.swap(new PlayingNode(node))(tx.peer)
//      old.dispose()
//      _data.addInstanceNode(node)
//      state = Playing
//    }

    private def setPlayingPrepare(resources: List[AsyncResource[S]])(implicit tx: S#Tx): PlayingPrepare = {
      val res = new PlayingPrepare(resources)
      val old = playingRef.swap(res)(tx.peer)
      old.dispose()
      state = Preparing
      res
    }

    private def freePlayingRef()(implicit tx: S#Tx): Unit = {
      val old = playingRef.swap(PlayingNone)(tx.peer)
      old.dispose()
    }
  }
}