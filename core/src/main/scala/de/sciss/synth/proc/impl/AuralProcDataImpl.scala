/*
 *  AuralProcDataImpl.scala
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

import de.sciss.file._
import de.sciss.lucre.artifact.Artifact
import de.sciss.lucre.expr.DoubleVector
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj, TxnLike}
import de.sciss.lucre.synth.{AudioBus, Buffer, Bus, NodeRef, Sys}
import de.sciss.numbers
import de.sciss.synth.ControlSet
import de.sciss.synth.io.AudioFileType
import de.sciss.synth.proc.AuralObj.{Playing, ProcData}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.UGenGraphBuilder.{Complete, Incomplete, MissingIn}
import de.sciss.synth.proc.graph.impl.ActionResponder
import de.sciss.synth.proc.{UGenGraphBuilder => UGB, logAural => logA}

import scala.concurrent.stm.{Ref, TMap, TSet, TxnLocal}

object AuralProcDataImpl {
  def apply[S <: Sys[S]](proc: Proc[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralObj.ProcData[S] =
    context.acquire[AuralObj.ProcData[S]](proc)(new Impl[S].init(proc))

  class Impl[S <: Sys[S]](implicit val context: AuralContext[S])
    extends ProcData[S] with UGB.Context[S] {

    import TxnLike.peer

    import context.{scheduler => sched, server}

    private[this] val stateRef      = Ref.make[UGB.State[S]]()

    // running main synths
    private[this] val nodeRef       = Ref(Option.empty[NodeGroupRef])

    // running attribute inputs
    private[this] val attrMap       = TMap.empty[String, AuralAttribute[S]]
    private[this] val outputBuses   = TMap.empty[String, AudioBus]
    private[this] val procViews     = TSet.empty[AuralObj.Proc[S]]

    private[this] val procLoc       = TxnLocal[Proc[S]]() // cache-only purpose

    private[this] var observers     = List.empty[Disposable[S#Tx]]

    private[this] var _obj: stm.Source[S#Tx, Proc[S]] = _

    final def obj: stm.Source[S#Tx, Proc[S]] = _obj

    override def toString = s"AuralObj.ProcData@${hashCode().toHexString}"

    /** Sub-classes may override this if invoking the super-method. */
    def init(proc: Proc[S])(implicit tx: S#Tx): this.type = {
      _obj          = tx.newHandle(proc)
      val ugenInit  = UGB.init(proc)
      stateRef()    = ugenInit

      observers ::= proc.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case Proc.GraphChange(_)        => newSynthGraph()
          case Proc.OutputAdded  (output) => outputAdded(output)
          case Proc.OutputRemoved(output) => outputRemoved(output)
        }
      }
      val attr = proc.attr
      observers ::= attr.changed.react { implicit tx => upd => upd.changes.foreach {
        case Obj.AttrAdded  (key, value) => attrAdded  (key, value)
        case Obj.AttrRemoved(key, value) => attrRemoved(key, value)
      }}

      tryBuild()
      this
    }

    final def nodeOption(implicit tx: TxnLike): Option[NodeRef] = nodeRef()

    private def playScans(n: NodeRef)(implicit tx: S#Tx): Unit = {
      logA(s"playScans ${procCached()}")
    }

    private def newSynthGraph()(implicit tx: S#Tx): Unit = {
      logA(s"newSynthGraph ${procCached()}")

      // stop and dispose all
      procViews.foreach { view =>
        if (view.state == Playing) view.stopForRebuild()
      }
      disposeBuild()

      // then try to rebuild the stuff
      val ugenInit = UGB.init(procCached())
      stateRef() = ugenInit
      tryBuild() // this will re-start the temporarily stopped views if possible
    }

    // ---- scan events ----

    private def outputAdded(output: Output[S])(implicit tx: S#Tx): Unit = {
      logA(s"outputAdded  to   ${procCached()} (${output.key})")
      val key = output.key
      outputBuses.get(key).foreach { bus =>
        mkAuralOutput(output, bus)
      }
    }

    private def outputRemoved(output: Output[S])(implicit tx: S#Tx): Unit = {
      logA(s"outputRemoved from ${procCached()} (${output.key})")
      context.getAux[AuralOutput[S]](output.id).foreach(disposeAuralOutput)
      val key = output.key
      state.outputs.get(key).foreach { numCh =>
      }
    }

    @inline
    private def disposeAuralOutput(view: AuralOutput[S])(implicit tx: S#Tx): Unit =
      view.dispose()  // this will call `context.removeAux`

    // ---- attr events ----

    private def attrAdded(key: String, value: Obj[S])(implicit tx: S#Tx): Unit = {
      val st          = state
      val aKey        = UGB.AttributeKey(key)
      val rejected    = st.rejectedInputs.contains(aKey)
      val acceptedOpt = st.acceptedInputs.get(aKey)
      val used        = rejected || acceptedOpt.isDefined
      logA(s"AttrAdded   to   ${procCached()} ($key) - used? $used")
      if (!used) return

      // mkAttrObserver1(key, value)
      val view    = attrMap.getOrElseUpdate(key, AuralAttribute(value))
      st match {
        case st0: Complete[S] =>
          acceptedOpt match {
            case Some((_, UGB.Input.Attribute.Value(numChannels))) =>
              nodeRef().foreach { group =>
                group.instanceNodes.foreach { nr =>
                  val target  = AuralAttribute.Target(nodeRef = nr, key, Bus.audio(server, numChannels = numChannels))
                  val trNew   = nr.shiftTo(sched.time)
                  view.play(timeRef = trNew, target = target)
                }
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
      attrMap.remove(key).foreach(_.dispose())
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

    final def addInstanceNode(n: AuralNode)(implicit tx: S#Tx): Unit = {
      logA(s"addInstanceNode ${procCached()} : $n")
      nodeRef().fold {
        val groupImpl = NodeGroupRef(name = s"Group-NodeRef ${procCached()}", in0 = n)
        nodeRef() = Some(groupImpl)
        playScans(groupImpl)

      } { groupImpl =>
        groupImpl.addInstanceNode(n)
        nodeRef() = None
      }
    }

    final def removeInstanceNode(n: AuralNode)(implicit tx: S#Tx): Unit = {
      logA(s"removeInstanceNode ${procCached()} : $n")
      val groupImpl = nodeRef().getOrElse(sys.error(s"Removing unregistered AuralProc node instance $n"))
      if (groupImpl.removeInstanceNode(n)) {
        disposeAttrMap()
      }
    }

    final def addInstanceView   (view: AuralObj.Proc[S])(implicit tx: S#Tx): Unit = procViews.add   (view)
    final def removeInstanceView(view: AuralObj.Proc[S])(implicit tx: S#Tx): Unit = procViews.remove(view)

    /** Sub-classes may override this if invoking the super-method. */
    def dispose()(implicit tx: S#Tx): Unit = {
      observers.foreach(_.dispose())
      disposeBuild()
    }

    private def disposeBuild()(implicit tx: S#Tx): Unit = {
      disposeNodeRefAndScans()
      disposeAttrMap()
    }

    private def disposeAttrMap()(implicit tx: S#Tx): Unit = {
      attrMap .foreach(_._2.dispose())
      attrMap .clear()
    }

    private def disposeNodeRefAndScans()(implicit tx: S#Tx): Unit = {
      nodeRef  .swap(None).foreach(_.dispose())
      outputBuses.clear()
    }

    final def state(implicit tx: S#Tx): UGB.State[S] = stateRef()

    /* If the ugen graph is incomplete, tries to (incrementally)
     * build it. Calls `buildAdvanced` with the old and new
     * state then.
     */
    final def tryBuild()(implicit tx: S#Tx): Unit = {
      state match {
        case s0: Incomplete[S] =>
          logA(s"try build ${procCached()} - ${procCached().name}")
          val s1      = s0.retry(this)
          stateRef()  = s1
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

      if (now.isComplete) {
        procViews.foreach { view =>
          if (view.targetState == Playing) {
            // ugen graph became ready and view wishes to play.
            view.playAfterRebuild()
          }
        }
      }
    }

    /* Creates a new aural output */
    private def mkAuralOutput(output: Output[S], bus: AudioBus)(implicit tx: S#Tx): AuralOutput[S] = {
      // val key   = output.key
      // val views = scanOutViews
      val view  = AuralOutput(data = this, output = output, bus = bus)
      // this is done by the `AuralOutput` constructor:
      // context.putAux[AuralOutput[S]](output.id, view)
      view
    }

    /** Sub-classes may override this if invoking the super-method. */
    def requestInput[Res](in: UGB.Input { type Value = Res }, st: Incomplete[S])
                         (implicit tx: S#Tx): Res = in match {
      case i: UGB.Input.Attribute =>
        val procObj   = procCached()
        val valueOpt  = procObj.attr.get(i.name)
        val found     = valueOpt.fold(-1) { value =>
          val view = attrMap.getOrElseUpdate(i.name, AuralAttribute(value))
          view.preferredNumChannels
        }

        // val found = requestAttrNumChannels(i.name)
        import i.{defaultNumChannels => defNum, requiredNumChannels => reqNum}
        if ((found < 0 && i.defaultNumChannels < 0) || (found >= 0 && reqNum >= 0 && found != reqNum)) {
          // throw new IllegalStateException(s"Attribute ${i.name} requires $reqNum channels (found $found)")
          throw MissingIn(i.key)
        }
        val res = if (found >= 0) found else if (reqNum >= 0) reqNum else defNum
        UGB.Input.Attribute.Value(res)

      case i: UGB.Input.Stream =>
        val numCh0  = requestAttrNumChannels(i.name)
        val numCh   = if (numCh0 < 0) 1 else numCh0     // simply default to 1
        val newSpecs0 = st.acceptedInputs.get(i.key) match {
          case Some((_, v: UGB.Input.Stream.Value))   => v.specs
          case _                                      => Nil
        }
        val newSpecs = if (i.spec.isEmpty) newSpecs0 else {
          i.spec :: newSpecs0
        }
        UGB.Input.Stream.Value(numChannels = numCh, specs = newSpecs)

      case i: UGB.Input.Buffer =>
        val procObj = procCached()
        val (numFr, numCh) = procObj.attr.get(i.name).fold((-1L, -1)) {
          case a: DoubleVector[S] =>
            val v = a.value   // XXX TODO: would be better to write a.peer.size.value
            (v.size.toLong, 1)
          case a: Grapheme.Expr.Audio[S] =>
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

    final def getOutputBus(key: String)(implicit tx: S#Tx): Option[AudioBus] =
      outputBuses.get(key)

    final def procCached()(implicit tx: S#Tx): Proc[S] = {
      if (procLoc.isInitialized) procLoc.get
      else {
        val proc = obj()
        procLoc.set(proc)
        proc
      }
    }

    @inline
    private def getAuralOutput(output: Output[S])(implicit tx: S#Tx): Option[AuralOutput[S]] =
      context.getAux[AuralOutput[S]](output.id)

    private def requestAttrNumChannels(key: String)(implicit tx: S#Tx): Int = {
      val procObj   = procCached()
      val valueOpt  = procObj.attr.get(key)
      valueOpt.fold(-1) {
        case a: DoubleVector[S] => a.value.size // XXX TODO: would be better to write a.peer.size.value
        case a: Grapheme.Expr.Audio [S] =>
          a.value.spec.numChannels
        case _: FadeSpec.Obj[S] => 4
        case a: Output[S] =>
          getAuralOutput(a).fold(-1)(_.bus.numChannels)
        case _ => -1
      }
    }

    /** Sub-classes may override this if invoking the super-method.
      * If the value is incompatible with the assigned `value` and rebuilding the
      * synth-graph would alleviate that problem, a `MissingIn` should be thrown.
      * If the problem does not change in terms of the re-evaluation of the
      * synth-graph, a different generic exception must be thrown to avoid
      * an infinite loop.
      */
    def buildAttrInput(nr: NodeRef.Full, timeRef: TimeRef, key: String, value: UGB.Value)
                      (implicit tx: S#Tx): Unit = {
      value match {
        case UGB.Input.Attribute.Value(numChannels) =>  // --------------------- scalar
          attrMap.get(key).foreach { a =>
            val target = AuralAttribute.Target(nr, key, Bus.audio(server, numChannels))
            a.play(timeRef = timeRef, target = target)
          }

        case UGB.Input.Stream.Value(numChannels, specs) =>  // ------------------ streaming
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
              case a: Grapheme.Expr.Audio[S] =>
                val audioVal  = a.value
                val spec      = audioVal.spec
                val path      = audioVal.artifact.getAbsolutePath
                val offset    = audioVal.offset
                val _gain     = audioVal.gain
                val _buf      = if (info.isNative) {
                  // XXX DIRTY HACK
                  val offset1 = if (key.contains("!rnd")) {
                    offset + (math.random * (spec.numFrames - offset)).toLong
                  } else {
                    offset
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
                    fileFrames = spec.numFrames, interp = info.interp, startFrame = offset, loop = false,
                    resetFrame = offset)
                  nr.addUser(trig)
                  __buf
                }
                (_buf, _gain.toFloat)

              case a => sys.error(s"Cannot use attribute $a as an audio stream")
            }
            nr.addControl(ctlName -> Seq[Float](rb.id, gain): ControlSet)
            nr.addResource(rb)
          }

        case UGB.Input.Buffer.Value(numFr, numCh, false) =>   // ----------------------- random access buffer
          val rb = procCached().attr.get(key).fold[Buffer] {
            sys.error(s"Missing attribute $key for buffer content")
          } {
            case a: Grapheme.Expr.Audio[S] =>
              val audioVal  = a.value
              val spec      = audioVal.spec
              val path      = audioVal.artifact.getAbsolutePath
              val offset    = audioVal.offset
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
          nr.addResource(rb)

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
          nr.addResource(rb)

        case _ =>
          throw new IllegalStateException(s"Unsupported input attribute request $value")
      }
    }
  }
}