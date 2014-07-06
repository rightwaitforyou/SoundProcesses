/*
 *  AuralProcImpl.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.synth.{AudioBus, AudioBusNodeSetter, AuralNode, BusNodeSetter, Bus, Buffer, Synth, DynamicUser, Resource, Sys}
import de.sciss.numbers
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.Curve.parametric
import de.sciss.synth.proc.AuralObj.ProcData
import de.sciss.synth.proc.Scan.Link
import de.sciss.synth.{addToHead, ControlSet}
import de.sciss.synth.proc.{logAural => logA}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm._

object AuralProcImpl {
  // type E[S <: evt.Sys[S]] = Proc.Elem[S]

  // def typeID = Proc.typeID

  def apply[S <: Sys[S]](proc: Proc.Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Proc[S] = {
    val data  = AuralProcDataImpl(proc)
    val res   = new Impl(data)
    // data.addView(res)
    res
  }

  private final class OutputBuilder(val bus: AudioBus) {
    var sinks = List.empty[(String, AuralNode)]
  }

  private final class AuralProcBuilder[S <: Sys[S]](val ugen: UGenGraphBuilder[S] /*, val name: String */) {
    var outputs = Map.empty[String, OutputBuilder]
  }

  private type ObjSource[S <: Sys[S]] = stm.Source[S#Tx, Obj.T[S, Proc.Elem]]

  // ---------------------------------------------------------------------

  private final class Impl[S <: Sys[S]](private val data: ProcData[S])(implicit context: AuralContext[S])
    extends AuralObj.Proc[S] with ObservableImpl[S, AuralObj.State] {

    import context.server

    def obj: stm.Source[S#Tx, Proc.Obj[S]] = data.obj

    def typeID: Int = Proc.typeID

    // def latencyEstimate(implicit tx: S#Tx): Long = ...

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

    private val currentStateRef = Ref[AuralObj.State](AuralObj.Stopped)
    private val targetStateRef  = Ref[AuralObj.State](AuralObj.Stopped)

    def state(implicit tx: S#Tx): AuralObj.State = currentStateRef.get(tx.peer)

    private def state_=(value: AuralObj.State)(implicit tx: S#Tx): Unit = {
      val old = currentStateRef.swap(value)(tx.peer)
      if (value != old) fire(value)
    }

    def play(time: SpanLike)(implicit tx: S#Tx): Unit = {
      val oldTarget = targetStateRef.swap(AuralObj.Playing)(tx.peer)
      val curr      = state
      data.state match {
        case s: UGenGraphBuilder.Complete[S] =>
          launchProc(s, time)
        case _ =>
      }
    }

    def stop(time: Long)(implicit tx: S#Tx): Unit = {
      freeNode()
      state = AuralObj.Stopped
    }

    // def prepare()(implicit tx: S#Tx): Unit = ...

    def dispose()(implicit tx: S#Tx): Unit = {
      freeNode()
      // data.removeView(this)
      context.release(data.procCached())
    }

    private def launchProc(ugen: UGenGraphBuilder.Complete[S], span: SpanLike)(implicit tx: S#Tx): Unit = {
      val p             = data.procCached()
      logA(s"begin launch $p (${hashCode.toHexString})")
      val ug            = ugen.result
      implicit val itx  = tx.peer

      val nameHint      = p.attr.expr[String](ProcKeys.attrName).map(_.value)
      val synth         = Synth.expanded(server, ug, nameHint = nameHint)
      // users are elements which must be added after the aural proc synth is started, and removed when it stops
      var users         = List.empty[DynamicUser]
      // resources are dependencies in terms of synth bundle spawning, and will be disposed by the aural proc
      var dependencies  = List.empty[Resource]

      // ---- handle input buses ----
      // val span          = timed.span.value
//      var setMap        = Vec[ControlSet](
//        graph.Time    .key -> time / sampleRate,
//        graph.Offset  .key -> (span match {
//          case Span.HasStart(start) => (time - start) / sampleRate
//          case _ => 0.0
//        }),
//        graph.Duration.key -> (span match {
//          case Span(start, stop)  => (stop - start) / sampleRate
//          case _ => Double.PositiveInfinity
//        })
//      )
      var setMap = Vector.empty[ControlSet]

      import Timeline.{SampleRate => sampleRate}

      // ---- attributes ----
      val attrNames     = ugen.attributeIns
      if (attrNames.nonEmpty) attrNames.foreach { n =>
        val ctlName = graph.attribute.controlName(n)
        p.attr.getElem(n).foreach {
          case a: IntElem     [S] => setMap :+= (ctlName -> a.peer.value.toFloat: ControlSet)
          case a: DoubleElem  [S] => setMap :+= (ctlName -> a.peer.value.toFloat: ControlSet)
          case a: BooleanElem [S] => setMap :+= (ctlName -> (if (a.peer.value) 1f else 0f): ControlSet)
          case a: FadeSpec.Elem[S] =>
            val spec = a.peer.value
            // dur, shape-id, shape-curvature, floor
            val values = Vec(
              (spec.numFrames / sampleRate).toFloat, spec.curve.id.toFloat, spec.curve match {
                case parametric(c)  => c
                case _              => 0f
              }, spec.floor
            )
            setMap :+= (ctlName -> values: ControlSet)
          case a: DoubleVecElem[S] =>
            val values = a.peer.value.map(_.toFloat)
            setMap :+= (ctlName -> values: ControlSet)
          case a: AudioGraphemeElem[S] =>
            val audioElem = a.peer
            val spec      = audioElem.spec
            //              require(spec.numChannels == 1 || spec.numFrames == 1,
            //                s"Audio grapheme ${a.peer} must have either 1 channel or 1 frame to be used as scalar attribute")
            require(spec.numFrames == 1, s"Audio grapheme ${a.peer} must have exactly 1 frame to be used as scalar attribute")
            //              val numChL = if (spec.numChannels == 1) spec.numFrames else spec.numChannels
            //              require(numChL <= 4096, s"Audio grapheme size ($numChL) must be <= 4096 to be used as scalar attribute")
            val numCh  = spec.numChannels // numChL.toInt
            require(numCh <= 4096, s"Audio grapheme size ($numCh) must be <= 4096 to be used as scalar attribute")
            val b      = Bus.control(server, numCh)
            val res    = BusNodeSetter.mapper(ctlName, b, synth)
            users ::= res
            val w      = AudioArtifactScalarWriter(b, audioElem.value)
            dependencies     ::= w
          // users ::= w

          case a => sys.error(s"Cannot cast attribute $a to a scalar value")
        }
      }

      // ---- streams ----
      val streamNames = ugen.streamIns
      if (streamNames.nonEmpty) streamNames.foreach { case (n, infoSeq0) =>
        val infoSeq = if (infoSeq0.isEmpty) UGenGraphBuilder.StreamIn.empty :: Nil else infoSeq0

        infoSeq.zipWithIndex.foreach { case (info, idx) =>
          val ctlName     = graph.stream.controlName(n, idx)
          val bufSize     = if (info.isEmpty) server.config.blockSize else {
            val maxSpeed  = if (info.maxSpeed <= 0.0) 1.0 else info.maxSpeed
            val bufDur    = 1.5 * maxSpeed
            val minSz     = (2 * server.config.blockSize * math.max(1.0, maxSpeed)).toInt
            val bestSz    = math.max(minSz, (bufDur * sampleRate).toInt)
            import numbers.Implicits._
            val bestSzHi  = bestSz.nextPowerOfTwo
            val bestSzLo  = bestSzHi >> 1
            if (bestSzHi.toDouble/bestSz < bestSz.toDouble/bestSzLo) bestSzHi else bestSzLo
          }
          val (rb, gain) = p.attr.getElem(n).fold[(Buffer, Float)] {
            // DiskIn and VDiskIn are fine with an empty non-streaming buffer, as far as I can tell...
            // So instead of aborting when the attribute is not set, fall back to zero
            val _buf = Buffer(server)(numFrames = bufSize, numChannels = 1)
            (_buf, 0f)
          } {
            case a: AudioGraphemeElem[S] =>
              val audioElem = a.peer
              val spec      = audioElem.spec
              val path      = audioElem.artifact.value.getAbsolutePath
              val offset    = audioElem.offset  .value
              val _gain     = audioElem.gain    .value
              val _buf      = if (info.isNative) {
                Buffer.diskIn(server)(
                  path          = path,
                  startFrame    = offset,
                  numFrames     = bufSize,
                  numChannels   = spec.numChannels
                )
              } else {
                val __buf = Buffer(server)(numFrames = bufSize, numChannels = spec.numChannels)
                val trig = new StreamBuffer(key = n, idx = idx, synth = synth, buf = __buf, path = path,
                  fileFrames = spec.numFrames, interp = info.interp)
                trig.install()
                __buf
              }
              (_buf, _gain.toFloat)

            case a => sys.error(s"Cannot use attribute $a as an audio stream")
          }
          setMap       :+= (ctlName -> Seq[Float](rb.id, gain): ControlSet)
          dependencies ::= rb
        }
      }

      import Grapheme.Segment
      // val outBuses      = builder.outputs
      // val aural         = AuralNode(synth, outBuses.mapValues(_.bus))
      val node = AuralNode(synth, Map.empty)

      // ---- scans ----
      // XXX TODO : this should all disappear
      // and the missing bits should be added
      // to AuralScan
      ugen.scanIns.foreach {
        case (key, scanIn) =>
          val numCh = scanIn.numChannels

          @inline def ensureChannels(n: Int): Unit =
            require(n == numCh, s"Scan input changed number of channels (expected $numCh but found $n)")

          val inCtlName = graph.scan.inControlName(key)
          // var inBus     = Option.empty[AudioBusNodeSetter]

          def mkInBus(): AudioBusNodeSetter = {
            val b      = data.getScanInBus(key) getOrElse Bus.audio(server, numCh)
            // val b      = Bus.audio(server, numCh)
            val res    = if (scanIn.fixed)
              BusNodeSetter.reader(inCtlName, b, synth)
            else
              BusNodeSetter.mapper(inCtlName, b, synth)
            users ::= res
            node.addInputBus(key, res.bus)
            res
          }

          // note: if not found, stick with default

          // XXX TODO:
          val time = span match {
            case hs: Span.HasStart => hs.start
            case _ => 0L
          }

          // XXX TODO: combination fixed + grapheme source doesn't work -- as soon as there's a bus mapper
          //           we cannot use ControlSet any more, but need other mechanism
          p.elem.peer.scans.get(key).foreach { scan =>
            val src = scan.sources
            // if (src.isEmpty) {
            // if (scanIn.fixed) lazyInBus  // make sure a fixed channels scan in exists as a bus
            // } else {
            src.foreach {
              case Link.Grapheme(peer) =>
                val segmOpt = peer.segment(time)
                segmOpt.foreach {
                  // again if not found... stick with default
                  case const: Segment.Const =>
                    ensureChannels(const.numChannels) // ... or could just adjust to the fact that they changed
                    //                        setMap :+= ((key -> const.numChannels) : ControlSet)
                    setMap :+= (if (const.numChannels == 1) {
                      ControlSet.Value (inCtlName, const.values.head .toFloat )
                    } else {
                      ControlSet.Vector(inCtlName, const.values.map(_.toFloat))
                    })

                  case segm: Segment.Curve =>
                    ensureChannels(segm.numChannels) // ... or could just adjust to the fact that they changed
                  // println(s"segment : ${segm.span}")
                  val bm     = mkInBus()
                    val w      = SegmentWriter(bm.bus, segm, time, sampleRate)
                    dependencies     ::= w
                  // users ::= w

                  case audio: Segment.Audio =>
                    ensureChannels(audio.numChannels)
                    val bm     = mkInBus()
                    val w      = AudioArtifactWriter(bm.bus, audio, time, sampleRate)
                    dependencies     ::= w
                  // users    ::= w
                }

              case Link.Scan(peer) =>
                // handles by AuralScan now
                mkInBus()

                //                scanView(peer).foreach {
                //                  case (sourceKey, sourceView) =>
                //                    val bIn         = lazyInBus
                //
                //                    // if the source isn't found (because it's probably in the ongoing build),
                //                    // we ignore that here; there is a symmetric counter part, looking for the
                //                    // builder.outputs that will handle these cases.
                //                    sourceView.getScanOutBus(sourceKey).foreach { bOut =>
                //                      ensureChannels(bOut.numChannels)
                //                      val edge        = NodeGraph.Edge(srcAural, sourceKey, node, key)
                //                      val link        = AudioLinkOLD(edge, sourceBus = bOut, sinkBus = bIn.bus)
                //                      dependencies  ::= link
                //                      users         ::= link
                //                    }
                //                }
            }
            // }
          }
      }

      // ---- handle output buses, and establish missing links to sinks ----
//      builder.outputs.foreach {
//        case (key, out) =>
//          val bw     = BusNodeSetter.writer(scan.outControlName(key), out.bus, synth)
//          val bOut   = bw.bus
//          users :+= bw
//
//          p.elem.peer.scans.get(key).foreach { scan =>
//            scan.sinks.foreach {
//              case Link.Scan(peer) =>
//                scanMap.get(peer.id).foreach {
//                  case (sinkKey, idH) =>
//                    val sinkTimedID = idH()
//                    viewMap.get(sinkTimedID).foreach { sinkAural =>
//                      val bIn = sinkAural.getInputBus(sinkKey).getOrElse {
//                        sys.error(s"Sink bus disappeared $sinkTimedID -> $sinkKey")
//                      }
//                      require(bIn.numChannels == bOut.numChannels,
//                        s"Scan input changed number of channels (expected ${bOut.numChannels} but found ${bIn.numChannels})")
//                      val edge    = NodeGraph.Edge(aural, key, sinkAural, sinkKey)
//                      val link    = AudioLink(edge, sourceBus = bOut, sinkBus = bIn)
//                      dependencies      ::= link
//                      users     ::= link
//                    }
//                }
//
//              case _  =>
//            }
//          }
//      }

      // busUsers.foreach(_.add())

      // XXX TODO
      val group = server.defaultGroup

      node.init(users, dependencies)
      // wrap as AuralProc and save it in the identifier map for later lookup
      synth.play(target = group, addAction = addToHead, args = setMap, dependencies = dependencies)
      if (users.nonEmpty) users.foreach(_.add())

      // if (setMap.nonEmpty) synth.set(audible = true, setMap: _*)
      logA(s"launched $p -> $node (${hashCode.toHexString})")
      setNode(node)
      state = AuralObj.Playing
    }

    //    private def scanView(scan: Scan[S])(implicit tx: S#Tx): Option[(String, ProcData[S])] =
    //      context.getAux[(String, ProcData[S])](scan.id)

    private def setNode(node: AuralNode)(implicit tx: S#Tx): Unit = {
      playingRef.swap(Some(node))(tx.peer).foreach(_.stop())
    }

    private def freeNode()(implicit tx: S#Tx): Unit = {
      playingRef.swap(None)(tx.peer).foreach(_.stop())
    }

    private val playingRef = Ref(Option.empty[AuralNode])
  }
}