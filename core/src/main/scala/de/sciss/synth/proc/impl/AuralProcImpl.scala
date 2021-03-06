/*
 *  AuralProcImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.file._
import de.sciss.lucre.artifact.Artifact
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.expr.{DoubleVector, StringObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj, TxnLike}
import de.sciss.lucre.synth.{AudioBus, Buffer, Bus, BusNodeSetter, NodeRef, Server, Synth, Sys}
import de.sciss.numbers
import de.sciss.span.Span
import de.sciss.synth.ControlSet
import de.sciss.synth.io.AudioFileType
import de.sciss.synth.proc.AuralObj.{TargetPlaying, TargetPrepared, TargetState, TargetStop}
import de.sciss.synth.proc.AuralView.{Playing, Prepared, Preparing, Stopped}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.TimeRef.SampleRate
import de.sciss.synth.proc.UGenGraphBuilder.{Complete, Incomplete, MissingIn}
import de.sciss.synth.proc.graph.impl.ActionResponder
import de.sciss.synth.proc.{UGenGraphBuilder => UGB, logAural => logA}

import scala.concurrent.Future
import scala.concurrent.stm.{Ref, TMap, TxnLocal}

object AuralProcImpl {
  def apply[S <: Sys[S]](proc: Proc[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Proc[S] = {
    val res   = new Impl[S]
    res.init(proc)
  }

  class Impl[S <: Sys[S]](implicit val context: AuralContext[S])
    extends AuralObj.Proc[S]
    with UGB.Context[S]
    with AuralAttribute.Observer[S]
    with ObservableImpl[S, AuralView.State] {

    import TxnLike.peer
    import context.{scheduler => sched}
    import sched.cursor

    private[this] val buildStateRef = Ref.make[UGB.State[S]]()

    // running attribute inputs
    private[this] val attrMap       = TMap.empty[String, AuralAttribute[S]]
    private[this] val outputBuses   = TMap.empty[String, AudioBus]
    private[this] val auralOutputs  = TMap.empty[String, AuralOutput.Owned[S]]

    private[this] val procLoc       = TxnLocal[Proc[S]]() // cache-only purpose

    private[this] var observers     = List.empty[Disposable[S#Tx]]

    private[this] var _obj: stm.Source[S#Tx, Proc[S]] = _

    final def obj: stm.Source[S#Tx, Proc[S]] = _obj

    override def toString = s"AuralObj.Proc@${hashCode().toHexString}"

    final def server: Server = context.server

    object ports extends ObservableImpl[S, AuralObj.Proc.Update[S]] {
      def apply(update: AuralObj.Proc.Update[S])(implicit tx: S#Tx): Unit = fire(update)
    }

    def getAttr  (key: String)(implicit tx: S#Tx): Option[AuralAttribute[S]] = attrMap     .get(key)
    def getOutput(key: String)(implicit tx: S#Tx): Option[AuralOutput   [S]] = auralOutputs.get(key)

    /* The ongoing build aural node build process, as stored in `playingRef`. */
    private sealed trait PlayingRef extends Disposable[S#Tx] {
      def nodeOption: Option[AuralNode[S]]
    }

    private[this] object PlayingNone extends PlayingRef {
      def dispose()(implicit tx: S#Tx) = ()
      def nodeOption = None
    }
    private final class PlayingNode(val node: AuralNode[S]) extends PlayingRef {
      def dispose()(implicit tx: S#Tx): Unit = {
        auralOutputs.foreach { case (_, view) =>
          view.stop()
        }
        attrMap.foreach { case (_, view) =>
          view.stop()
        }
        node.dispose()
      }

      def nodeOption = Some(node)
    }
    private final class PlayingPrepare(val resources: List[AsyncResource[S]]) extends PlayingRef {
      def dispose()(implicit tx: S#Tx): Unit = resources.foreach(_.dispose())
      def nodeOption = None
    }

    // XXX TODO - perhaps `currentStateRef` and `playingRef` could be one thing?
    private[this] val currentStateRef     = Ref[AuralView.State](Stopped    )
    private[this] val targetStateRef      = Ref[TargetState    ](TargetStop )
    private[this] val playingRef          = Ref[PlayingRef     ](PlayingNone)

    final def typeID: Int = Proc.typeID

    final def state      (implicit tx: S#Tx): AuralView.State = currentStateRef()
    final def targetState(implicit tx: S#Tx): AuralView.State = targetStateRef ().completed

    private def state_=(value: AuralView.State)(implicit tx: S#Tx): Unit = {
      val old = currentStateRef.swap(value)
      if (value != old) {
        fire(value)
      }
    }

    /** Sub-classes may override this if invoking the super-method. */
    def init(proc: Proc[S])(implicit tx: S#Tx): this.type = {
      _obj            = tx.newHandle(proc)
      val ugenInit    = UGB.init(proc)
      buildStateRef() = ugenInit

      observers ::= proc.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case Proc.GraphChange(_)        => newSynthGraph()
          case Proc.OutputAdded  (output) => outputAdded(output)
          case Proc.OutputRemoved(output) => outputRemoved(output)
        }
      }
      val attr = proc.attr
      observers ::= attr.changed.react { implicit tx => upd => upd.changes.foreach {
        case Obj.AttrAdded   (key, value) => attrAdded  (key, value)
        case Obj.AttrRemoved (key, value) => attrRemoved(key, value)
        case Obj.AttrReplaced(key, before, now) =>
          attrRemoved(key, before)
          attrAdded  (key, now   )
      }}

      tryBuild()
      this
    }

    final def nodeOption(implicit tx: TxnLike): Option[NodeRef] = playingRef().nodeOption

    @inline
    private[this] def playOutputs(n: NodeRef)(implicit tx: S#Tx): Unit = {
      logA(s"playOutputs ${procCached()}")
      auralOutputs.foreach { case (_, view) =>
        view.play(n)
      }
    }

    private def newSynthGraph()(implicit tx: S#Tx): Unit = {
      logA(s"newSynthGraph ${procCached()}")

      if (state == Playing) stopForRebuild()

      disposeBuild()

      // then try to rebuild the stuff
      val ugenInit    = UGB.init(procCached())
      buildStateRef() = ugenInit
      tryBuild() // this will re-start the temporarily stopped views if possible
    }

    // ---- scan events ----

    private def outputAdded(output: Output[S])(implicit tx: S#Tx): Unit = {
      logA(s"outputAdded  to   ${procCached()} (${output.key})")
      val key = output.key
      outputBuses.get(key).foreach { bus =>
        val view = mkAuralOutput(output, bus)
        nodeOption.foreach(view.play)
      }
    }

    private def outputRemoved(output: Output[S])(implicit tx: S#Tx): Unit = {
      logA(s"outputRemoved from ${procCached()} (${output.key})")
      context.getAux[AuralOutput[S]](output.id).foreach(disposeAuralOutput)
//      val key = output.key
//      state.outputs.get(key).foreach { numCh =>
//        ... // XXX TODO - what was I thinking to do here?
//      }
    }

    @inline
    private[this] def disposeAuralOutput(view: AuralOutput[S])(implicit tx: S#Tx): Unit = {
      view.dispose() // this will call `context.removeAux`
      val exists = auralOutputs.remove(view.key)
      if (exists.isEmpty) throw new IllegalStateException(s"AuralOutput ${view.key} was not in map")
      ports(AuralObj.Proc.OutputRemoved(this, view))
    }

    // ---- attr events ----

    private def attrAdded(key: String, value: Obj[S])(implicit tx: S#Tx): Unit = {
      val st          = buildState
      val aKey        = UGB.AttributeKey(key)
      val rejected    = st.rejectedInputs.contains(aKey)
      val acceptedOpt = st.acceptedInputs.get(aKey)
      val used        = rejected || acceptedOpt.isDefined
      logA(s"AttrAdded   to   ${procCached()} ($key) - used? $used")
      if (!used) return

      val view = mkAuralAttribute(key, value)
      st match {
        case st0: Complete[S] =>
          acceptedOpt match {
            case Some((_, UGB.Input.Attribute.Value(numChannels))) =>
              playingRef() match {
                case p: PlayingNode =>
                  val nr      = p.node
                  val target  = AuralAttribute.Target(nodeRef = nr, key, Bus.audio(server, numChannels = numChannels))
                  val trNew   = nr.shiftTo(sched.time)
                  view.play(timeRef = trNew, target = target)
                case _ =>
              }

            case _ =>
          }

        case st0: Incomplete[S] =>
          acceptedOpt.fold[Unit] {  // rejected
            // give it another incremental try
            tryBuild()
          } { case (input, valueBefore) =>
            // if the request value changes or the
            // new request is rejected, we have to
            // rebuild the whole thing
            try {
              val valueNow = requestInput[input.Value](input, st0)
              if (valueNow != valueBefore) newSynthGraph()
            } catch {
              case MissingIn(_) => newSynthGraph()
            }
          }

        case _ =>
      }
    }

    private def attrRemoved(key: String, value: Obj[S])(implicit tx: S#Tx): Unit = {
      logA(s"AttrRemoved from ${procCached()} ($key)")
      attrMap.remove(key).foreach { view =>
        ports(AuralObj.Proc.AttrRemoved(this, view))
        view.dispose()
      }
    }

    // ----

    // creates an `AuralOutput` and registers it with the aural context.
    private def addUsedOutput(key: String, numChannels: Int)(implicit tx: S#Tx): Unit = {
      val outputs = procCached().outputs
      val bus     = Bus.audio(server, numChannels = numChannels) // mkBus(key, numChannels)
      outputBuses.put(key, bus).foreach(_ => throw new IllegalStateException(s"Output bus for $key already defined"))
      outputs.get(key).foreach { output =>
        mkAuralOutput(output, bus)
      }
    }

    /** Sub-classes may override this if invoking the super-method. */
    def dispose()(implicit tx: S#Tx): Unit = {
      freePlayingRef()
      observers.foreach(_.dispose())
      disposeBuild()
    }

    // does _not_ dispose playingRef
    private def disposeBuild()(implicit tx: S#Tx): Unit = {
      auralOutputs.foreach { case (_, view) =>
        ports(AuralObj.Proc.OutputRemoved(this, view))
        view.dispose()
      }
      auralOutputs.clear()
      outputBuses .clear()

      attrMap .foreach { case (_, view) =>
        ports(AuralObj.Proc.AttrRemoved(this, view))
        view.dispose()
      }
      attrMap .clear()
    }

    private def buildState(implicit tx: S#Tx): UGB.State[S] = buildStateRef()

    /* If the ugen graph is incomplete, tries to (incrementally)
     * build it. Calls `buildAdvanced` with the old and new
     * state then.
     */
    final def tryBuild()(implicit tx: S#Tx): Unit = {
      buildState match {
        case s0: Incomplete[S] =>
          logA(s"try build ${procCached()} - ${procCached().name}")
          val s1          = s0.retry(this)
          buildStateRef() = s1
          buildAdvanced(before = s0, now = s1)

        case s0: Complete[S] => // nada
      }
    }

    /* Called after invoking `retry` on the ugen graph builder.
     * The methods looks for new scan-ins and scan-outs used by
     * the ugen graph, and creates aural-scans for them, or
     * at least the bus-proxies if no matching entries exist
     * in the proc's `scans` dictionary.
     *
     * If the now-state indicates that the ugen-graph is complete,
     * it calls `play` on the proc-views whose target-state is to play.
     */
    private def buildAdvanced(before: UGB.State[S], now: UGB.State[S])(implicit tx: S#Tx): Unit = {

      // handle newly rejected inputs
      if (now.rejectedInputs.isEmpty) {
        logA(s"buildAdvanced ${procCached()}; complete? ${now.isComplete}")
      } else {
        logA(s"buildAdvanced ${procCached()}; rejectedInputs = ${now.rejectedInputs.mkString(",")}")
      }

      // handle newly visible outputs
      if (before.outputs ne now.outputs) {
        // detect which new outputs have been determined in the last iteration
        // (newOuts is a map from `name: String` to `numChannels Int`)
        val newOuts = now.outputs.filterNot {
          case (key, _) => before.outputs.contains(key)
        }
        logA(s"...newOuts = ${newOuts.mkString(",")}")

        newOuts.foreach { case (key, numCh) =>
          addUsedOutput(key, numCh)
        }
      }

      if (now.isComplete && targetState == Playing) playAfterRebuild()
    }

    /* Creates a new aural output */
    private def mkAuralOutput(output: Output[S], bus: AudioBus)(implicit tx: S#Tx): AuralOutput.Owned[S] = {
      val view  = AuralOutput(view = this, output = output, bus = bus)
      // this is done by the `AuralOutput` constructor:
      // context.putAux[AuralOutput[S]](output.id, view)
      val old = auralOutputs.put(output.key, view)
      if (old.isDefined) throw new IllegalStateException(s"AuralOutput already exists for ${output.key}")
      ports(AuralObj.Proc.OutputAdded(this, view))
      view
    }

    private def mkAuralAttribute(key: String, value: Obj[S])(implicit tx: S#Tx): AuralAttribute[S] =
      attrMap.get(key).getOrElse {
        val view = AuralAttribute(key, value, this)
        attrMap.put(key, view)
        ports(AuralObj.Proc.AttrAdded(this, view))
        view
      }

    // AuralAttribute.Observer
    final def attrNumChannelsChanged(attr: AuralAttribute[S])(implicit tx: S#Tx): Unit = {
      val aKey = UGB.AttributeKey(attr.key)
      if (buildState.rejectedInputs.contains(aKey)) tryBuild()
    }

    /** Sub-classes may override this if invoking the super-method. */
    def requestInput[Res](in: UGB.Input { type Value = Res }, st: Incomplete[S])
                         (implicit tx: S#Tx): Res = in match {
      case i: UGB.Input.Attribute =>
        val procObj   = procCached()
        val valueOpt  = procObj.attr.get(i.name)
        val found     = valueOpt.fold(-1) { value =>
          val view = mkAuralAttribute(i.name, value)
          view.preferredNumChannels
        }

        import i.{defaultNumChannels => defNum, requiredNumChannels => reqNum}
        if ((found < 0 && i.defaultNumChannels < 0) || (found >= 0 && reqNum >= 0 && found != reqNum)) {
          // throw new IllegalStateException(s"Attribute ${i.name} requires $reqNum channels (found $found)")
          throw MissingIn(i.key)
        }
        val res = if (found >= 0) found else if (reqNum >= 0) reqNum else defNum
        UGB.Input.Attribute.Value(res)

      case i: UGB.Input.Stream =>
        val value0  = requestAttrStreamValue(i.name)
        val value1  = if (value0.numChannels < 0) value0.copy(numChannels = 1) else value0 // simply default to 1
        val newSpecs0 = st.acceptedInputs.get(i.key) match {
          case Some((_, v: UGB.Input.Stream.Value))   => v.specs
          case _                                      => Nil
        }
        val newSpecs = if (i.spec.isEmpty) newSpecs0 else {
          i.spec :: newSpecs0
        }
        val value2 = if (newSpecs.isEmpty) value1 else value1.copy(specs = newSpecs)
        value2

      case i: UGB.Input.Buffer =>
        val procObj = procCached()
        val (numFr, numCh) = procObj.attr.get(i.name).fold((-1L, -1)) {
          case a: DoubleVector[S] =>
            val v = a.value   // XXX TODO: would be better to write a.peer.size.value
            (v.size.toLong, 1)
          case a: AudioCue.Obj[S] =>
            // val spec = a.spec
            val spec = a.value.spec
            (spec.numFrames, spec.numChannels)

          case _ => (-1L, -1)
        }
        if (numCh < 0) throw MissingIn(i.key)
        // larger files are asynchronously prepared, smaller ones read on the fly
        val async = (numCh * numFr) > UGB.Input.Buffer.AsyncThreshold   // XXX TODO - that threshold should be configurable
        UGB.Input.Buffer.Value(numFrames = numFr, numChannels = numCh, async = async)

      case i: UGB.Input.Action  => UGB.Input.Action .Value
      case i: UGB.Input.DiskOut => UGB.Input.DiskOut.Value(i.numChannels)

      case _ => throw new IllegalStateException(s"Unsupported input request $in")
    }

    private def getOutputBus(key: String)(implicit tx: S#Tx): Option[AudioBus] =
      outputBuses.get(key)

    final protected def procCached()(implicit tx: S#Tx): Proc[S] = {
      if (procLoc.isInitialized) procLoc.get
      else {
        val proc = obj()
        procLoc.set(proc)
        proc
      }
    }

    @inline
    private[this] def getAuralOutput(output: Output[S])(implicit tx: S#Tx): Option[AuralOutput[S]] =
      context.getAux[AuralOutput[S]](output.id)

    @inline
    private[this] def requestAttrStreamValue(key: String)(implicit tx: S#Tx): UGB.Input.Stream.Value = {
      val procObj   = procCached()
      val valueOpt  = procObj.attr.get(key)

      def simple(numCh: Int) =
        UGB.Input.Stream.Value(numChannels = numCh, sampleRate = server.sampleRate, specs = Nil)

      valueOpt.fold(simple(-1)) {
        case a: DoubleVector[S] =>
          simple(a.value.size) // XXX TODO: would be better to write a.peer.size.value
        case a: AudioCue.Obj[S] =>
          val spec = a.value.spec
          UGB.Input.Stream.Value(numChannels = spec.numChannels, sampleRate = spec.sampleRate, specs = Nil)
        case _: FadeSpec.Obj[S] => simple(4)
        case a: Output[S] =>
          simple(getAuralOutput(a).fold(-1)(_.bus.numChannels))
        case _ => simple(-1)
      }
    }

    /** Sub-classes may override this if invoking the super-method.
      * If the value is incompatible with the assigned `value` and rebuilding the
      * synth-graph would alleviate that problem, a `MissingIn` should be thrown.
      * If the problem does not change in terms of the re-evaluation of the
      * synth-graph, a different generic exception must be thrown to avoid
      * an infinite loop.
      */
    def buildAttrInput(nr: NodeRef.Full[S], timeRef: TimeRef, key: String, value: UGB.Value)
                      (implicit tx: S#Tx): Unit = {
      value match {
        case UGB.Input.Attribute.Value(numChannels) =>  // --------------------- scalar
          attrMap.get(key).foreach { a =>
            val target = AuralAttribute.Target(nr, key, Bus.audio(server, numChannels))
            a.play(timeRef = timeRef, target = target)
          }

        case UGB.Input.Stream.Value(numChannels, _, specs) =>  // ------------------ streaming
          val infoSeq = if (specs.isEmpty) UGB.Input.Stream.EmptySpec :: Nil else specs

          infoSeq.zipWithIndex.foreach { case (info, idx) =>
            val ctlName     = graph.impl.Stream.controlName(key, idx)
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
            val (rb, gain) = procCached().attr.get(key).fold[(Buffer, Float)] {
              // DiskIn and VDiskIn are fine with an empty non-streaming buffer, as far as I can tell...
              // So instead of aborting when the attribute is not set, fall back to zero
              val _buf = Buffer(server)(numFrames = bufSize, numChannels = 1)
              (_buf, 0f)
            } {
              case a: AudioCue.Obj[S] =>
                val audioVal  = a.value
                val spec      = audioVal.spec
                val path      = audioVal.artifact.getAbsolutePath
                val _gain     = audioVal.gain
                val offsetT   = ((audioVal.offset + timeRef.offset) * spec.sampleRate / SampleRate + 0.5).toLong
                val _buf      = if (info.isNative) {
                  // XXX DIRTY HACK
                  val offset1 = if (key.contains("!rnd")) {
                    val fOffset = audioVal.fileOffset
                    fOffset + (math.random * (spec.numFrames - fOffset)).toLong
                  } else {
                    offsetT
                  }
                  // println(s"OFFSET = $offset1")
                  Buffer.diskIn(server)(
                    path          = path,
                    startFrame    = offset1,
                    numFrames     = bufSize,
                    numChannels   = spec.numChannels
                  )
                } else {
                  val __buf = Buffer(server)(numFrames = bufSize, numChannels = spec.numChannels)
                  val trig = new StreamBuffer(key = key, idx = idx, synth = nr.node, buf = __buf, path = path,
                    fileFrames = spec.numFrames, interp = info.interp, startFrame = offsetT, loop = false,
                    resetFrame = offsetT)
                  nr.addUser(trig)
                  __buf
                }
                (_buf, _gain.toFloat)

              case a => sys.error(s"Cannot use attribute $a as an audio stream")
            }
            nr.addControl(ctlName -> Seq[Float](rb.id, gain): ControlSet)
            val late = Buffer.disposeWithNode(rb, nr)
            nr.addResource(late)
          }

        case UGB.Input.Buffer.Value(numFr, numCh, false) =>   // ----------------------- random access buffer
          val rb = procCached().attr.get(key).fold[Buffer] {
            sys.error(s"Missing attribute $key for buffer content")
          } {
            case a: AudioCue.Obj[S] =>
              val audioVal  = a.value
              val spec      = audioVal.spec
              val path      = audioVal.artifact.getAbsolutePath
              val offset    = audioVal.fileOffset
              // XXX TODO - for now, gain is ignored.
              // one might add an auxiliary control proxy e.g. Buffer(...).gain
              // val _gain     = audioElem.gain    .value
              if (spec.numFrames > 0x3FFFFFFF)
                sys.error(s"File too large for in-memory buffer: $path (${spec.numFrames} frames)")
              val bufSize   = spec.numFrames.toInt
              val _buf      = Buffer(server)(numFrames = bufSize, numChannels = spec.numChannels)
              _buf.read(path = path, fileStartFrame = offset)
              _buf

            case a => sys.error(s"Cannot use attribute $a as a buffer content")
          }
          val ctlName    = graph.Buffer.controlName(key)
          nr.addControl(ctlName -> rb.id)
          val late = Buffer.disposeWithNode(rb, nr)
          nr.addResource(late)

        case UGB.Input.Action.Value =>   // ----------------------- action
          val resp = new ActionResponder(objH = obj /* tx.newHandle(nr.obj) */, key = key, synth = nr.node)
          nr.addUser(resp)

        case UGB.Input.DiskOut.Value(numCh) =>
          val rb = procCached().attr.get(key).fold[Buffer] {
            sys.error(s"Missing attribute $key for disk-out artifact")
          } {
            case a: Artifact[S] =>
              val artifact  = a
              val f         = artifact.value.absolute
              val ext       = f.ext.toLowerCase
              val tpe       = AudioFileType.writable.find(_.extensions.contains(ext)).getOrElse(AudioFileType.AIFF)
              val _buf      = Buffer.diskOut(server)(path = f.path, fileType = tpe, numChannels = numCh)
              _buf

            case a => sys.error(s"Cannot use attribute $a as an artifact")
          }
          val ctlName    = graph.DiskOut.controlName(key)
          nr.addControl(ctlName -> rb.id)
          val late = Buffer.disposeWithNode(rb, nr)
          nr.addResource(late)

        case _ =>
          throw new IllegalStateException(s"Unsupported input attribute request $value")
      }
    }

    final def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit = {
      targetStateRef() = TargetPrepared
      // XXX TODO
    }

    final def play(timeRef: TimeRef.Option, unit: Unit)(implicit tx: S#Tx): Unit = {
      val tr  = timeRef.force
      val ts  = TargetPlaying(sched.time, tr)
      targetStateRef() = ts
      buildState match {
        case s: UGB.Complete[S] =>
          state match {
            case Stopped   => prepareAndLaunch(s, tr)
            case Prepared  => launch          (s, tr)
            case _ =>
          }

        case _ =>
      }
    }

    // same as `play` but reusing previous `timeRef`
    private def playAfterRebuild()(implicit tx: S#Tx): Unit = {
      if (state != Stopped) return

      (buildState, targetStateRef()) match {
        case (s: UGB.Complete[S], tp: TargetPlaying) =>
          prepareAndLaunch(s, tp.shiftTo(sched.time))
        case _ =>
      }
    }

    final def stop(/* time: Long */)(implicit tx: S#Tx): Unit = {
      targetStateRef() = TargetStop
      stopForRebuild()
    }

    // same as `stop` but not touching target state
    private def stopForRebuild()(implicit tx: S#Tx): Unit = {
      freePlayingRef()
      state = Stopped
    }

    /** Sub-classes may override this if falling back to the super-method. */
    protected def buildAsyncInput(b: AsyncProcBuilder[S], keyW: UGB.Key, value: UGB.Value)
                                 (implicit tx: S#Tx): Unit = keyW match {
      case UGB.AttributeKey(key) => buildAsyncAttrInput(b, key, value)
      case _                     => throw new IllegalStateException(s"Unsupported async input request $keyW")
    }

    /** Sub-classes may override this if falling back to the super-method. */
    protected def buildSyncInput(nr: NodeRef.Full[S], timeRef: TimeRef, keyW: UGB.Key, value: UGB.Value)
                                (implicit tx: S#Tx): Unit = keyW match {
      case UGB.AttributeKey(key) =>
        buildAttrInput(nr, timeRef, key, value)

      case _ =>
        throw new IllegalStateException(s"Unsupported input request $keyW")
    }

    /** Sub-classes may override this if invoking the super-method. */
    protected def buildAsyncAttrInput(b: AsyncProcBuilder[S], key: String, value: UGB.Value)
                                     (implicit tx: S#Tx): Unit = value match {
      case UGB.Input.Buffer.Value(numFr, numCh, true) =>   // ----------------------- random access buffer
        b.obj.attr.get(key).fold[Unit] {
          sys.error(s"Missing attribute $key for buffer content")
        } {
          case a: AudioCue.Obj[S] =>
            val audioVal  = a.value
            val spec      = audioVal.spec
            val f         = audioVal.artifact
            val offset    = audioVal.fileOffset
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
    private def prepareAndLaunch(ugen: UGB.Complete[S], timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      val p = procCached()
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
              if (playingRef() == prep) {
                prepared(ugen)
              }
            }
          }
        }
      }
    }

    private def prepared(ugen: UGB.Complete[S])(implicit tx: S#Tx): Unit =
      targetStateRef() match {
        case tp: TargetPlaying =>
          launch(ugen, tp.shiftTo(sched.time)) // XXX TODO - yes or no, shift time?
        case _ =>
          state = Prepared
      }

    // ---- synchronous preparation ----
    protected def launch(ugen: UGB.Complete[S], timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      val p = procCached()
      logA(s"begin launch  $p (${hashCode.toHexString})")

      val ug            = ugen.result
      val nameHint      = p.attr.$[StringObj](ObjKeys.attrName).map(_.value)
      val synth         = Synth.expanded(server, ug, nameHint = nameHint)

      val builder       = AuralNode[S](timeRef, sched.time, synth)

      // "consume" prepared state
      playingRef.swap(PlayingNone) match {
        case prep: PlayingPrepare =>
          prep.resources.foreach { resource =>
            resource.install(builder)
          }
        case _ =>
      }

      // XXX TODO - it would be nicer if these were added optionally
      if (timeRef.frame  != 0) builder.addControl(graph.Time    .key -> (timeRef.frame  / SampleRate))
      if (timeRef.offset != 0) builder.addControl(graph.Offset  .key -> (timeRef.offset / SampleRate))
      timeRef.span match {
        case Span(start, stop) =>
          builder.addControl(graph.Duration.key -> ((stop - start) / SampleRate))
        case _ => // Double.PositiveInfinity
      }

      ugen.acceptedInputs.foreach { case (key, (_, value)) =>
        if (!value.async) buildSyncInput(builder, timeRef, key, value)
      }

      // ---- handle output buses, and establish missing links to sinks ----
      ugen.outputs.foreach { case (key, numCh) =>
        val bus    = getOutputBus(key) getOrElse sys.error(s"Scan bus $key not provided")
        logA(s"addOutputBus($key, $bus) (${hashCode.toHexString})")
        val res    = BusNodeSetter.writer(graph.ScanOut.controlName(key), bus, synth)
        builder.addUser(res)
      }

      val old       = playingRef.swap(new PlayingNode(builder))
      old.dispose()
      builder.play()
      playOutputs(builder)

      logA(s"launched $p -> $builder (${hashCode.toHexString})")
      state = Playing
    }

    private def setPlayingPrepare(resources: List[AsyncResource[S]])(implicit tx: S#Tx): PlayingPrepare = {
      val res = new PlayingPrepare(resources)
      val old = playingRef.swap(res)
      old.dispose()
      state = Preparing
      res
    }

    private def freePlayingRef()(implicit tx: S#Tx): Unit = {
      val old = playingRef.swap(PlayingNone)
      old.dispose()
    }
  }
}
