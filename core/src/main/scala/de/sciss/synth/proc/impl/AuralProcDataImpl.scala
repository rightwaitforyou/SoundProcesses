/*
 *  AuralProcDataImpl.scala
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

import de.sciss.file._
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.{Buffer, BusNodeSetter, AudioBus, Bus, NodeRef, Sys}
import de.sciss.model.Change
import de.sciss.numbers
import de.sciss.synth.Curve.parametric
import de.sciss.synth.io.AudioFileType
import de.sciss.synth.proc.AuralObj.ProcData
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.Scan.Link
import de.sciss.synth.proc.UGenGraphBuilder.{Complete, Incomplete, MissingIn}
import de.sciss.synth.proc.graph.impl.ActionResponder
import de.sciss.synth.proc.{UGenGraphBuilder => UGB, logAural => logA}
import de.sciss.synth.{ControlSet, SynthGraph}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{Ref, TMap, TSet, TxnLocal}

object AuralProcDataImpl {
  def apply[S <: Sys[S]](proc: Obj.T[S, Proc.Elem])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralObj.ProcData[S] =
    context.acquire[AuralObj.ProcData[S]](proc)(new Impl[S].init(proc))

  class Impl[S <: Sys[S]](implicit val context: AuralContext[S])
    extends ProcData[S] with UGB.Context[S] {

    private val stateRef  = Ref.make[UGB.State[S]]()

    // running main synths
    private val nodeRef   = Ref(Option.empty[NodeRef.Group])

    // running attribute inputs
    private val attrMap   = TMap.empty[String, Disposable[S#Tx]]

    private val scanBuses = TMap.empty[String, AudioBus]
    private val scanViews = TMap.empty[String, AuralScan.Owned[S]]
    private val procViews = TSet.empty[AuralObj.Proc[S]]

    private val procLoc   = TxnLocal[Proc.Obj[S]]() // cache-only purpose

    private var procObserver: Disposable[S#Tx] = _

    private var _obj: stm.Source[S#Tx, Proc.Obj[S]] = _

    final def obj = _obj

    override def toString = s"AuralObj.ProcData@${hashCode().toHexString}"

    /** Sub-classes may override this if invoking the super-method. */
    def init(proc: Proc.Obj[S])(implicit tx: S#Tx): this.type = {
      _obj = tx.newHandle(proc)
      val ugenInit = UGB.init(proc)
      stateRef.set(ugenInit)(tx.peer)

      procObserver = proc.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case Obj.ElemChange(Proc.Update(_, pCh)) =>
            pCh.foreach {
              case Proc.GraphChange(Change(_, newGraph)) => newSynthGraph(newGraph)
              case Proc.ScanAdded  (key, scan)      => scanAdded  (key, scan)
              case Proc.ScanRemoved(key, scan)      => scanRemoved(key, scan)
              case Proc.ScanChange (key, scan, sCh) => scanChange (key, scan, sCh)
            }

          case Obj.AttrAdded  (key, value)          => attrAdded  (key, value)
          case Obj.AttrRemoved(key, value)          => attrRemoved(key, value)
          case Obj.AttrChange (key, value, aCh)     => attrChange (key, value, aCh)
        }
      }

      tryBuild()
      this
    }

    final def nodeOption(implicit tx: S#Tx): Option[NodeRef] = nodeRef.get(tx.peer)

    final def getScan(key: String)(implicit tx: S#Tx): Option[Either[AudioBus, AuralScan[S]]] = {
      implicit val itx = tx.peer
      scanViews.get(key).fold[Option[Either[AudioBus, AuralScan[S]]]] {
        scanBuses.get(key).map(Left(_))
      } { v =>
        Some(Right(v))
      }
    }

    private def playScans(n: NodeRef)(implicit tx: S#Tx): Unit = {
      logA(s"playScans ${procCached()}")
      scanViews.foreach { case (_, view) =>
        view.play(n)
      }(tx.peer)
    }

    private def newSynthGraph(g: SynthGraph)(implicit tx: S#Tx): Unit = {
      logA(s"newSynthGraph ${procCached()}")
      implicit val itx = tx.peer

      // stop and dispose all
      procViews.foreach { view =>
        if (view.state == AuralObj.Playing) view.stopForRebuild()
      }
      disposeNodeRefAndScans()

      // then try to rebuild the stuff
      val ugenInit = UGB.init(procCached())
      stateRef() = ugenInit
      tryBuild() // this will re-start the temporarily stopped views if possible
    }

    // ---- scan events ----

    private def scanAdded(key: String, scan: Scan[S])(implicit tx: S#Tx): Unit = {
      logA(s"ScanAdded  to   ${procCached()} ($key)")
      testInScan (key, scan)
      testOutScan(key, scan)
    }

    private def scanRemoved(key: String, scan: Scan[S])(implicit tx: S#Tx): Unit = {
      logA(s"ScanRemoved from ${procCached()} ($key)")
    }

    private def scanChange(key: String, scan: Scan[S], changes: Vec[Scan.Change[S]])(implicit tx: S#Tx): Unit = {
      logA(s"ScanChange in   ${procCached()} ($key)")
      changes.foreach {
        case Scan.SourceAdded(_) => testInScan(key, scan)
        case _ =>
      }
    }

    // ---- attr events ----

    private def attrAdded(key: String, value: Obj[S])(implicit tx: S#Tx): Unit = {
      logA(s"AttrAdded   to   ${procCached()} ($key)")
      attrNodeSet(key, value)
    }

    private def attrNodeSet(key: String, value: Obj[S])(implicit tx: S#Tx): Unit =
      nodeRef.get(tx.peer).foreach { n =>
        state.acceptedInputs.get(UGB.AttributeKey(key)).foreach(v => attrNodeSet(n, key, v, value))
      }

    protected def attrNodeSet(n: NodeRef, key: String, assigned: UGB.Value, value: Obj[S])(implicit tx: S#Tx): Unit =
      assigned match {
        case UGB.Input.Attribute.Value(numCh) =>
          // XXX TODO -- we have to verify the number of channels
          val set = attrControlSet(key, value.elem)
          n.node.set(set)

        case b: UGB.Input.Buffer.Value =>
          Console.err.println(s"WARNING: Changing buffer contents ($key) while playing not yet supported")

        case UGB.Input.Action.Value => // not relevant

        case other =>
          throw new IllegalStateException(s"Unsupported input request $other")
      }

    private def attrRemoved(key: String, value: Obj[S])(implicit tx: S#Tx): Unit = {
      logA(s"AttrRemoved from ${procCached()} ($key)")
      // currently this is simply ignored
    }

    private def attrChange(key: String, value: Obj[S], changes: Vec[Obj.Change[S, Any]])(implicit tx: S#Tx): Unit = {
      logA(s"AttrChange in   ${procCached()} ($key)")
      // currently, instead of processing the changes
      // for individual types, we'll just re-evaluate
      // the value and set it that way
      val isElem = changes.exists {
        case Obj.ElemChange(_) => true
        case _ => false
      }
      if (isElem) attrNodeSet(key, value)
    }

    // ----

    // if a scan was added or a source was added to an existing scan,
    // check if the scan is used as currently missing input. if so,
    // try to build the ugen graph again.
    private def testInScan(key: String, scan: Scan[S])(implicit tx: S#Tx): Unit = {
      if (state.rejectedInputs.contains(UGB.ScanKey(key))) {
        val numCh = scanInNumChannels(scan)
        // println(s"testInScan($key) -> numCh = $numCh")
        if (numCh >= 0) {
          // the scan is ready to be used and was missing before
          tryBuild()
        } else {
          addIncompleteScanIn(key, scan)
        }
      }
    }

    // incomplete scan ins are scan ins which do exist and are used by
    // the ugen graph, but their source is not yet available. in order
    // for them to be detected when a sink is added, the sink must
    // be able to find a reference to them via the context's auxiliary
    // map. we thus store the special proxy sub-type `Incomplete` in
    // there; when the sink finds it, it will invoke `sinkAdded`,
    // in turn causing the data object to try to rebuild the ugen graph.
    private def addIncompleteScanIn(key: String, scan: Scan[S])(implicit tx: S#Tx): Unit =
      context.putAux[AuralScan.Proxy[S]](scan.id, new AuralScan.Incomplete(this, key))

    // called from scan-view if source is not materialized yet
    final def sinkAdded(key: String, view: AuralScan[S])(implicit tx: S#Tx): Unit =
      if (state.rejectedInputs.contains(UGenGraphBuilder.ScanKey(key))) tryBuild()

    private def testOutScan(key: String, scan: Scan[S])(implicit tx: S#Tx): Unit = {
      state.scanOuts.get(key).foreach { numCh =>
        scanViews.get(key)(tx.peer).fold[Unit] {
          mkAuralScan(key, scan, numCh)
        } { view =>
          checkScanNumChannels(view, numCh)
        }
      }
    }

    final def addInstanceNode(n: NodeRef)(implicit tx: S#Tx): Unit = {
      logA(s"addInstanceNode ${procCached()} : $n")
      implicit val itx = tx.peer
      nodeRef().fold {
        val groupImpl = NodeRef.Group(name = s"Group-NodeRef ${procCached()}", in0 = n)
        nodeRef() = Some(groupImpl)
        playScans(groupImpl)

      } { groupImpl =>
        groupImpl.addInstanceNode(n)
      }
    }

    final def removeInstanceNode(n: NodeRef)(implicit tx: S#Tx): Unit = {
      logA(s"removeInstanceNode ${procCached()} : $n")
      implicit val itx = tx.peer
      val groupImpl = nodeRef().getOrElse(sys.error(s"Removing unregistered AuralProc node instance $n"))
      if (groupImpl.removeInstanceNode(n)) {
        nodeRef() = None
        scanViews.foreach(_._2.stop   ())
        attrMap  .foreach(_._2.dispose())
        attrMap  .clear()
      }
    }

    final def addInstanceView   (view: AuralObj.Proc[S])(implicit tx: S#Tx): Unit = procViews.add   (view)(tx.peer)
    final def removeInstanceView(view: AuralObj.Proc[S])(implicit tx: S#Tx): Unit = procViews.remove(view)(tx.peer)

    /** Sub-classes may override this if invoking the super-method. */
    def dispose()(implicit tx: S#Tx): Unit = {
      procObserver.dispose()
      disposeNodeRefAndScans()
    }

    private def disposeNodeRefAndScans()(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      nodeRef  .swap(None).foreach(_.dispose())
      scanViews.foreach(_._2.dispose())
      scanViews.clear()
      attrMap  .foreach(_._2.dispose())
      attrMap  .clear()
      scanBuses.clear()
      val rj = stateRef().rejectedInputs
      if (rj.nonEmpty) {
        val scans = procCached().elem.peer.scans
        rj.foreach {
          case UGB.ScanKey(key) =>
            scans.get(key).foreach { scan =>
              context.removeAux(scan.id)
            }
          case _ =>
        }
      }
    }

    final def state(implicit tx: S#Tx): UGB.State[S] = stateRef.get(tx.peer)

    /* If the ugen graph is incomplete, tries to (incrementally)
     * build it. Calls `buildAdvanced` with the old and new
     * state then.
     */
    final def tryBuild()(implicit tx: S#Tx): Unit = {
      state match {
        case s0: Incomplete[S] =>
          logA(s"try build ${procCached()} - ${procCached().name}")
          val s1 = s0.retry(this)
          stateRef.set(s1)(tx.peer)
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
      implicit val itx = tx.peer

      if (now.rejectedInputs.isEmpty) {
        logA(s"buildAdvanced ${procCached()}; complete? ${now.isComplete}")
      } else {
        logA(s"buildAdvanced ${procCached()}; rejectedInputs = ${now.rejectedInputs.mkString(",")}")

        // store proxies so future sinks can detect this incomplete proc
        val scans = procCached().elem.peer.scans
        now.rejectedInputs.foreach {
          case UGB.ScanKey(key) =>
            scans.get(key).foreach { scan =>
              addIncompleteScanIn(key, scan)
            }

          case _ =>
        }
      }

      // handle newly visible outputs
      if (before.scanOuts ne now.scanOuts) {
        // detect which new scan outputs have been determined in the last iteration
        // (newOuts is a map from `name: String` to `numChannels Int`)
        val newOuts = now.scanOuts.filterNot {
          case (key, _) => before.scanOuts.contains(key)
        }
        logA(s"...newOuts = ${newOuts.mkString(",")}")

        newOuts.foreach { case (key, numCh) =>
          activateAuralScan(key, numCh)
        }
      }

      // handle newly visible inputs
      if (before.acceptedInputs ne now.acceptedInputs) {
        val newIns = now.acceptedInputs.filterNot {
          case (key, _) => before.acceptedInputs.contains(key)
        }
        logA(s"...newIns  = ${newIns.mkString(",")}")

        newIns.foreach {
          case (UGB.ScanKey(key), UGB.Input.Scan.Value(numCh)) =>
            // val numCh = meta.numChannels
            activateAuralScan(key, numCh)
          case _ =>
        }
      }

      now match {
        case c: Complete[S] =>
          procViews.foreach { view =>
            if (view.targetState == AuralObj.Playing) {
              // ugen graph became ready and view wishes to play.
              view.playAfterRebuild()
            }
          }
        case _ =>
      }
    }

    /** Sub-classes may override this if invoking the super-method. */
    def attrControlSet(key: String, value: Elem[S])(implicit tx: S#Tx): ControlSet = {
      val ctlName = graph.Attribute.controlName(key)
      value match {
        case a: IntElem     [S] => ctlName -> a.peer.value.toFloat: ControlSet
        case a: DoubleElem  [S] => ctlName -> a.peer.value.toFloat: ControlSet
        case a: BooleanElem [S] => ctlName -> (if (a.peer.value) 1f else 0f): ControlSet
        case a: FadeSpec.Elem[S] =>
          val spec = a.peer.value
          // dur, shape-id, shape-curvature, floor
          val values = Vec(
            (spec.numFrames / Timeline.SampleRate).toFloat, spec.curve.id.toFloat, spec.curve match {
              case parametric(c)  => c
              case _              => 0f
            }, spec.floor
          )
          ctlName -> values: ControlSet
        case a: DoubleVecElem[S] =>
          val values = a.peer.value.map(_.toFloat)
          ctlName -> values: ControlSet

        case _ =>
          sys.error(s"Cannot cast attribute $value to a scalar value")
      }
    }

    /* Ensures that an aural-scan for a given key exists. If it exists,
     * checks that the number of channels is correct. Otherwise, checks
     * if a scan for the key exists. If yes, instantiates the aural-scan,
     * if no, creates an audio-bus for later use.
     */
    private def activateAuralScan(key: String, numChannels: Int)(implicit tx: S#Tx): Unit = {
      scanViews.get(key)(tx.peer).fold {
        val scans = procCached().elem.peer.scans
        scans.get(key).fold[Unit] {
          mkBus(key, numChannels)
        } { scan =>
          mkAuralScan(key, scan, numChannels)
        }
      } { view =>
        checkScanNumChannels(view, numChannels)
      }
    }

    /* Creates a bus for the given scan, unless it already exists.
     * Existing buses are checked for consistency with the given
     * number-of-channels (throws an exception upon discrepancy).
     */
    private def mkBus(key: String, numChannels: Int)(implicit tx: S#Tx): AudioBus = {
      implicit val itx = tx.peer
      val bus = scanBuses.get(key).getOrElse {
        val res = Bus.audio(context.server, numChannels = numChannels)
        scanBuses.put(key, res)
        res
      }
      if (bus.numChannels != numChannels)
        sys.error(s"Scan bus channels changed from ${bus.numChannels} to $numChannels")

      bus
    }

    /* Creates a new aural scan */
    private def mkAuralScan(key: String, scan: Scan[S], numChannels: Int)(implicit tx: S#Tx): AuralScan[S] = {
      val bus   = mkBus(key, numChannels)
      val view  = AuralScan(data = this, key = key, scan = scan, bus = bus)
      scanViews.put(key, view)(tx.peer)
      // note: the view will iterate over the
      //       sources and sinks itself upon initialization,
      //       and establish the playing links if found
      //
      //      nodeOption.foreach { n =>
      //        view.play(n)
      //      }
      view
    }

    private def checkScanNumChannels(view: AuralScan[S], numCh: Int): Unit = {
      val numCh1 = view.bus.numChannels
      if (numCh1 != numCh) sys.error(s"Trying to access scan with competing numChannels ($numCh1, $numCh)")
    }

    //    def scanInBusChanged(sinkKey: String, bus: AudioBus)(implicit tx: S#Tx): Unit = {
    //      if (state.missingIns.contains(sinkKey)) tryBuild()
    //    }

    // def getScanBus(key: String)(implicit tx: S#Tx): Option[AudioBus] = scanViews.get(key)(tx.peer).map(_.bus)

    /** Sub-classes may override this if invoking the super-method. */
    def requestInput[Res](in: UGB.Input { type Value = Res }, st: Incomplete[S])
                         (implicit tx: S#Tx): Res = in match {
      case i: UGB.Input.Attribute =>
        val found  = requestAttrNumChannels(i.name)
        val reqNum = i.numChannels
        if (found >= 0 && reqNum >= 0 && found != reqNum)
          throw new IllegalStateException(s"Attribute ${i.name} requires $reqNum channels (found $found)")
        val res = if (found >= 0) found else if (reqNum >= 0) reqNum else 1
        UGB.Input.Attribute.Value(res)

      case i: UGB.Input.Scan =>
        UGB.Input.Scan.Value(requestScanInNumChannels(i))

      case i: UGB.Input.Stream =>
        val numCh0  = requestAttrNumChannels(i.name)
        val numCh   = if (numCh0 < 0) 1 else numCh0     // simply default to 1
        val newSpecs0 = st.acceptedInputs.get(i.key) match {
          case Some(v: UGB.Input.Stream.Value)  => v.specs
          case _                                => Nil
        }
        val newSpecs = if (i.spec.isEmpty) newSpecs0 else {
          i.spec :: newSpecs0
        }
        UGB.Input.Stream.Value(numChannels = numCh, specs = newSpecs)

      case i: UGB.Input.Buffer =>
        val procObj = procCached()
        val (numFr, numCh) = procObj.attr.getElem(i.name).fold((-1L, -1)) {
          case a: DoubleVecElem[S] =>
            val v = a.peer.value   // XXX TODO: would be better to write a.peer.size.value
            (v.size.toLong, 1)
          case a: AudioGraphemeElem[S] =>
            val spec = a.peer.spec
            (spec.numFrames, spec.numChannels)

          case _ => (-1L, -1)
        }
        if (numCh < 0) throw MissingIn(i)
        // larger files are asynchronously prepared, smaller ones read on the fly
        val async = (numCh * numFr) > UGB.Input.Buffer.AsyncThreshold   // XXX TODO - that threshold should be configurable
        UGB.Input.Buffer.Value(numFrames = numFr, numChannels = numCh, async = async)

      case i: UGB.Input.Action  => UGB.Input.Action .Value
      case i: UGB.Input.DiskOut => UGB.Input.DiskOut.Value(i.numChannels)

      case _ => throw new IllegalStateException(s"Unsupported input request $in")
    }

    final def getScanBus(key: String)(implicit tx: S#Tx): Option[AudioBus] = scanBuses.get(key)(tx.peer)

    final def procCached()(implicit tx: S#Tx): Proc.Obj[S] = {
      implicit val itx = tx.peer
      if (procLoc.isInitialized) procLoc.get
      else {
        val proc = obj()
        procLoc.set(proc)
        proc
      }
    }

    private def scanView(scan: Scan[S])(implicit tx: S#Tx): Option[AuralScan[S]] =
      context.getAux[AuralScan.Proxy[S]](scan.id) match {
        case Some(view: AuralScan[S]) => Some(view)
        case _                        => None
      }

    private def requestAttrNumChannels(key: String)(implicit tx: S#Tx): Int = {
      val procObj = procCached()
      procObj.attr.getElem(key).fold(-1) {
        case a: DoubleVecElem    [S] => a.peer.value.size // XXX TODO: would be better to write a.peer.size.value
        case a: AudioGraphemeElem[S] => a.peer.spec.numChannels
        case _: FadeSpec.Elem    [S] => 4
        case _ => -1
      }
    }

    private def requestScanInNumChannels(req: UGenGraphBuilder.Input.Scan)(implicit tx: S#Tx): Int = {
      val procObj = procCached()    /** Sub-classes may override this if invoking the super-method. */

      val proc    = procObj.elem.peer
      val numCh0  = proc.scans.get(req.name).fold(-1)(scanInNumChannels)
      val numCh   = if (numCh0 == -1) req.fixed else numCh0
      if (numCh == -1) throw MissingIn(req) else numCh
    }

    private def scanInNumChannels(scan: Scan[S])(implicit tx: S#Tx): Int = {
      val chans = scan.sources.toList.map {
        case Link.Grapheme(peer) =>
          // val chansOpt = peer.valueAt(time).map(_.numChannels)
          // chansOpt.getOrElse(numChannels)
          peer.numChannels

        case Link.Scan(peer) =>
          val sourceOpt = scanView(peer)
          sourceOpt.fold(-1) { sourceView =>
            // val sourceObj = sourceObjH()
            // getOutputBus(sourceObj, sourceKey)
            sourceView.bus.numChannels // data.state.scanOuts.get(sourceView.key)
          }
      }
      if (chans.isEmpty) -1 else chans.max
    }

    //////////////////////////////////////////////////////////////////////////////////////////////

    /** Sub-classes may override this if invoking the super-method. */
    protected def buildSyncAttrInput(b: NodeDependencyBuilder[S], key: String, value: UGB.Value)
                                    (implicit tx: S#Tx): Unit = {
      import context.server
      import context.scheduler.cursor
      value match {
        case UGB.Input.Attribute.Value(numChannels) =>  // --------------------- scalar
          // XXX TODO - numChannels is not tested
          b.obj.attr.getElem(key).foreach {
            case a: AudioGraphemeElem[S] =>
              val ctlName   = graph.Attribute.controlName(key)
              val audioElem = a.peer
              val spec      = audioElem.spec
              if (spec.numFrames != 1)
                sys.error(s"Audio grapheme ${a.peer} must have exactly 1 frame to be used as scalar attribute")
              val numCh = spec.numChannels // numChL.toInt
              if (numCh > 4096) sys.error(s"Audio grapheme size ($numCh) must be <= 4096 to be used as scalar attribute")
              val bus = Bus.control(server, numCh)
              val res = BusNodeSetter.mapper(ctlName, bus, b.node)
              // b.users ::= res
              b.addUser(res)
              val w = AudioArtifactScalarWriter(bus, audioElem.value)
              // b.dependencies ::= w
              b.addResource(w)

            case a =>
              // b.setMap += /* _data.*/ attrControlSet(key, a)
              b.addControl(attrControlSet(key, a))
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
                  val trig = new StreamBuffer(key = key, idx = idx, synth = b.node, buf = __buf, path = path,
                    fileFrames = spec.numFrames, interp = info.interp, startFrame = offset, loop = false,
                    resetFrame = offset)
                  trig.install()
                  __buf
                }
                (_buf, _gain.toFloat)

              case a => sys.error(s"Cannot use attribute $a as an audio stream")
            }
            // b.setMap      += (ctlName -> Seq[Float](rb.id, gain): ControlSet)
            b.addControl(ctlName -> Seq[Float](rb.id, gain): ControlSet)
            // b.dependencies ::= rb
            b.addResource(rb)
          }

        case UGB.Input.Buffer.Value(numFr, numCh, false) =>   // ----------------------- random access buffer
          val rb = b.obj.attr.getElem(key).fold[Buffer] {
            sys.error(s"Missing attribute $key for buffer content")
          } {
            case a: AudioGraphemeElem[S] =>
              val audioElem = a.peer
              val spec      = audioElem.spec
              val path      = audioElem.artifact.value.getAbsolutePath
              val offset    = audioElem.offset  .value
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
          // b.setMap      += ctlName -> rb.id
          b.addControl(ctlName -> rb.id)
          // b.dependencies ::= rb
          b.addResource(rb)

        case UGB.Input.Action.Value =>   // ----------------------- action
          ActionResponder.install(obj = b.obj, key = key, synth = b.node)

        case UGB.Input.DiskOut.Value(numCh) =>
          val rb = b.obj.attr.getElem(key).fold[Buffer] {
            sys.error(s"Missing attribute $key for disk-out artifact")
          } {
            case a: ArtifactElem[S] =>
              val artifact  = a.peer
              val f         = artifact.value.absolute
              val ext       = f.ext.toLowerCase
              val tpe       = AudioFileType.writable.find(_.extensions.contains(ext)).getOrElse(AudioFileType.AIFF)
              val _buf      = Buffer.diskOut(server)(path = f.path, fileType = tpe, numChannels = numCh)
              _buf

            case a => sys.error(s"Cannot use attribute $a as an artifact")
          }
          val ctlName    = graph.DiskOut.controlName(key)
          // b.setMap      += ctlName -> rb.id
          b.addControl(ctlName -> rb.id)
          // b.dependencies ::= rb
          b.addResource(rb)

        case _ =>
          throw new IllegalStateException(s"Unsupported input attribute request $value")
      }
    }
  }
}
