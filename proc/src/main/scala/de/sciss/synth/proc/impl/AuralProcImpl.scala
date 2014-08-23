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
import de.sciss.span.Span
import de.sciss.synth.proc.AuralObj.ProcData
import de.sciss.synth.proc.Grapheme.Segment
import de.sciss.synth.proc.Scan.Link
import de.sciss.synth.{proc, addToHead, ControlSet}
import de.sciss.synth.proc.{logAural => logA}
import proc.{UGenGraphBuilder => UGB}

import scala.concurrent.stm.Ref

object AuralProcImpl {
  def apply[S <: Sys[S]](proc: Proc.Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Proc[S] = {
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

  private sealed trait TargetState {
    def toAuralState: AuralObj.State
  }
  private case object TargetStop extends TargetState {
    def toAuralState = AuralObj.Stopped
  }
  private final class TargetPlaying(val wallClock: Long, val timeRef: TimeRef) extends TargetState {
    def toAuralState = AuralObj.Playing

    override def toString = s"TargetPlaying(wallClock = $wallClock, timeRef = $timeRef)"

    def shiftTo(newWallClock: Long): TimeRef = {
      val delta = newWallClock - wallClock
      timeRef.shift(delta)
    }
  }
  
  final class SynthBuilder[S <: Sys[S]](val obj: Proc.Obj[S], val synth: Synth, val timeRef: TimeRef) {
    var setMap        = Vector.newBuilder[ControlSet]

    /** Users are elements which must be added after the
      * aural proc synth is started, and removed when it stops.
      */
    var users         = List.empty[DynamicUser]
    /** resources are dependencies in terms of synth bundle spawning,
      * and will be disposed by the aural proc.
      */
    var dependencies  = List.empty[Resource]

    var outputBuses   = Map.empty[String, AudioBus]
    var inputBuses    = Map.empty[String, AudioBus]
  }

  class Impl[S <: Sys[S]](implicit context: AuralContext[S])
    extends AuralObj.Proc[S] with ObservableImpl[S, AuralObj.State] {

    import context.server

    private var _data: ProcData[S]  = _
    private val currentStateRef     = Ref[AuralObj.State](AuralObj.Stopped)
    private val targetStateRef      = Ref[TargetState](TargetStop)
    private val playingRef          = Ref(Option.empty[AuralNode])

    final def obj: stm.Source[S#Tx, Proc.Obj[S]] = _data.obj

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
    final def targetState(implicit tx: S#Tx): AuralObj.State = targetStateRef .get(tx.peer).toAuralState

    private def state_=(value: AuralObj.State)(implicit tx: S#Tx): Unit = {
      val old = currentStateRef.swap(value)(tx.peer)
      if (value != old) fire(value)
    }

    final def play(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      val ts = new TargetPlaying(context.scheduler.time, timeRef)
      targetStateRef.set(ts)(tx.peer)
      if (state != AuralObj.Stopped) return
      _data.state match {
        case s: UGB.Complete[S] => launchProc(s, timeRef)
        case _ =>
      }
    }

    // same as `play` but reusing previous `timeRef`
    final def playAfterRebuild()(implicit tx: S#Tx): Unit = {
      if (state != AuralObj.Stopped) return

      (_data.state, targetStateRef.get(tx.peer)) match {
        case (s: UGB.Complete[S], tp: TargetPlaying) =>
          launchProc(s, tp.shiftTo(context.scheduler.time))
        case _ =>
      }
    }

    final def stop(/* time: Long */)(implicit tx: S#Tx): Unit = {
      targetStateRef.set(TargetStop)(tx.peer)
      stopForRebuild()
    }

    // same as `stop` but not touching target state
    final def stopForRebuild()(implicit tx: S#Tx): Unit = {
      freeNode()
      state = AuralObj.Stopped
    }

    // def prepare()(implicit tx: S#Tx): Unit = ...

    /** Sub-classes may override this if invoking the super-method. */
    def dispose()(implicit tx: S#Tx): Unit = {
      freeNode()
      _data.removeInstanceView(this)
      context.release(_data.procCached())
    }

    /** Sub-classes may override this if invoking the super-method. */
    protected def buildInput(b: SynthBuilder[S], keyW: UGB.Key, value: UGB.Value)(implicit tx: S#Tx): Unit = keyW match {
      case UGB.AttributeKey(key)  => buildAttrInput(b, key, value)
      case UGB.ScanKey     (key)  => buildScanInput(b, key, value)
      case _                      => throw new IllegalStateException(s"Unsupported input request $keyW")
    }

    /** Sub-classes may override this if invoking the super-method. */
    protected def buildScanInput(b: SynthBuilder[S], key: String, value: UGB.Value)(implicit tx: S#Tx): Unit = value match {
      case UGB.NumChannels(numCh) =>
        // ---- scans ----
        // XXX TODO : this should all disappear
        // and the missing bits should be added
        // to AuralScan

        // val numCh = scanIn.numChannels

        @inline def ensureChannels(n: Int): Unit =
          require(n == numCh, s"Scan input changed number of channels (expected $numCh but found $n)")

        val inCtlName = graph.scan.inControlName(key)
        // var inBus     = Option.empty[AudioBusNodeSetter]

        def mkInBus(): AudioBusNodeSetter = {
          val bus    = _data.getScanBus(key) getOrElse sys.error(s"Scan bus $key not provided")
          // val b      = Bus.audio(server, numCh)
          logA(s"addInputBus($key, $b) (${hashCode.toHexString})")
          val res    =
          // if (scanIn.fixed)
          //   BusNodeSetter.reader(inCtlName, b, synth)
          // else
            BusNodeSetter.mapper(inCtlName, bus, b.synth)
          b.users ::= res
          b.inputBuses += key -> bus
          res
        }

        // note: if not found, stick with default

        val time = b.timeRef.offsetOrZero

        // XXX TODO: combination fixed + grapheme source doesn't work -- as soon as there's a bus mapper
        //           we cannot use ControlSet any more, but need other mechanism
        b.obj.elem.peer.scans.get(key).foreach { scan =>
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
                  b.setMap += (if (const.numChannels == 1) {
                    ControlSet.Value (inCtlName, const.values.head .toFloat )
                  } else {
                    ControlSet.Vector(inCtlName, const.values.map(_.toFloat))
                  })

                case segm: Segment.Curve =>
                  ensureChannels(segm.numChannels) // ... or could just adjust to the fact that they changed
                // println(s"segment : ${segm.span}")
                val bm          = mkInBus()
                  val w           = SegmentWriter(bm.bus, segm, time, Timeline.SampleRate)
                  b.dependencies  ::= w
                // users ::= w

                case audio: Segment.Audio =>
                  ensureChannels(audio.numChannels)
                  val bm          = mkInBus()
                  val w           = AudioArtifactWriter(bm.bus, audio, time)
                  b.dependencies  ::= w
                // users    ::= w
              }

            case Link.Scan(peer) => mkInBus()
          }
        }

      case _ => throw new IllegalStateException(s"Unsupported input scan request $value")
    }

    /** Sub-classes may override this if invoking the super-method. */
    protected def buildAttrInput(b: SynthBuilder[S], key: String, value: UGB.Value)(implicit tx: S#Tx): Unit = value match {
      case UGB.NumChannels(numChannels) =>
        // XXX TODO - numChannels is not tested
        b.obj.attr.getElem(key).foreach {
          case a: AudioGraphemeElem[S] =>
            val ctlName   = graph.attribute.controlName(key)
            val audioElem = a.peer
            val spec      = audioElem.spec
            //              require(spec.numChannels == 1 || spec.numFrames == 1,
            //                s"Audio grapheme ${a.peer} must have either 1 channel or 1 frame to be used as scalar attribute")
            require(spec.numFrames == 1, s"Audio grapheme ${a.peer} must have exactly 1 frame to be used as scalar attribute")
            //              val numChL = if (spec.numChannels == 1) spec.numFrames else spec.numChannels
            //              require(numChL <= 4096, s"Audio grapheme size ($numChL) must be <= 4096 to be used as scalar attribute")
            val numCh = spec.numChannels // numChL.toInt
            require(numCh <= 4096, s"Audio grapheme size ($numCh) must be <= 4096 to be used as scalar attribute")
            val bus = Bus.control(server, numCh)
            val res = BusNodeSetter.mapper(ctlName, bus, b.synth)
            b.users ::= res
            val w = AudioArtifactScalarWriter(bus, audioElem.value)
            b.dependencies ::= w
          // users ::= w

          case a => b.setMap += _data.attrControlSet(key, a)
        }

      case UGB.Input.Stream.Value(numChannels, specs) =>
        val infoSeq = if (specs.isEmpty) UGB.Input.Stream.EmptySpec :: Nil else specs

        infoSeq.zipWithIndex.foreach { case (info, idx) =>
          val ctlName     = graph.stream.controlName(key, idx)
          val bufSize     = if (info.isEmpty) server.config.blockSize else {
            val maxSpeed  = if (info.maxSpeed <= 0.0) 1.0 else info.maxSpeed
            val bufDur    = 1.5 * maxSpeed
            val minSz     = (2 * server.config.blockSize * math.max(1.0, maxSpeed)).toInt
            val bestSz    = math.max(minSz, (bufDur * server.sampleRate).toInt)
            import numbers.Implicits._
            val bestSzHi  = bestSz.nextPowerOfTwo
            val bestSzLo  = bestSzHi >> 1
            if (bestSzHi.toDouble/bestSz < bestSz.toDouble/bestSzLo) bestSzHi else bestSzLo
          }
          val (rb, gain) = b.obj.attr.getElem(key).fold[(Buffer, Float)] {
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
                val trig = new StreamBuffer(key = key, idx = idx, synth = b.synth, buf = __buf, path = path,
                  fileFrames = spec.numFrames, interp = info.interp)
                trig.install()
                __buf
              }
              (_buf, _gain.toFloat)

            case a => sys.error(s"Cannot use attribute $a as an audio stream")
          }
          b.setMap      += (ctlName -> Seq[Float](rb.id, gain): ControlSet)
          b.dependencies ::= rb
        }

      case _ =>
        throw new IllegalStateException(s"Unsupported input attribute request $value")
    }
    
    private def launchProc(ugen: UGB.Complete[S], timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      val p             = _data.procCached()
      logA(s"begin launch $p (${hashCode.toHexString})")
      val ug            = ugen.result
      implicit val itx  = tx.peer

      val nameHint      = p.attr.expr[String](ObjKeys.attrName).map(_.value)
      val synth         = Synth.expanded(server, ug, nameHint = nameHint)

      val builder       = new SynthBuilder(p, synth, timeRef)

      // XXX TODO - it would be nicer if these were added optionally
      builder.setMap += graph.Time    .key -> (timeRef.frame        / Timeline.SampleRate)
      builder.setMap += graph.Offset  .key -> (timeRef.offsetOrZero / Timeline.SampleRate)
      builder.setMap += graph.Duration.key -> (timeRef.span match {
        case Span(start, stop)  => (stop - start) / Timeline.SampleRate
        case _ => Double.PositiveInfinity
      })

      ugen.acceptedInputs.foreach { tup =>
        buildInput(builder, tup._1, tup._2)
      }

      // ---- handle output buses, and establish missing links to sinks ----
      ugen.scanOuts.foreach { case (key, numCh) =>
        val bus    = _data.getScanBus(key) getOrElse sys.error(s"Scan bus $key not provided")
        logA(s"addOutputBus($key, $bus) (${hashCode.toHexString})")
        val res    = BusNodeSetter.writer(graph.scan.outControlName(key), bus, synth)
        builder.users ::= res
        builder.outputBuses += key -> bus
        // res
      }

      // XXX TODO
      val group = server.defaultGroup

      val node = AuralNode(synth, inputBuses = builder.inputBuses, outputBuses = builder.outputBuses,
        users = builder.users, resources = builder.dependencies)

      // wrap as AuralProc and save it in the identifier map for later lookup
      synth.play(target = group, addAction = addToHead, args = builder.setMap.result(), 
        dependencies = builder.dependencies)
      if (builder.users.nonEmpty) builder.users.foreach(_.add())

      // if (setMap.nonEmpty) synth.set(audible = true, setMap: _*)
      logA(s"launched $p -> $node (${hashCode.toHexString})")
      setNode(node)
      state = AuralObj.Playing
    }

    private def setNode(node: AuralNode)(implicit tx: S#Tx): Unit = {
      playingRef.swap(Some(node))(tx.peer).foreach(freeNode1)
      _data.addInstanceNode(node)
    }

    private def freeNode()(implicit tx: S#Tx): Unit = {
      playingRef.swap(None)(tx.peer).foreach(freeNode1)
    }

    private def freeNode1(n: AuralNode)(implicit tx: S#Tx): Unit = {
      _data.removeInstanceNode(n)
      n.stop()
    }
  }
}