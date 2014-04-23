/*
 *  AuralPresentationImpl.scala
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
import stm.IdentifierMap
import collection.breakOut
import collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{Ref, TxnLocal}
import de.sciss.synth.proc.{logAural => logA}
import UGenGraphBuilder.MissingIn
import graph.scan
import de.sciss.span.Span
import de.sciss.synth.Curve.parametric
import de.sciss.synth.proc.Scan.Link
import scala.util.control.NonFatal
import de.sciss.lucre.synth.{DynamicUser, Buffer, Bus, AudioBus, NodeGraph, AuralNode, BusNodeSetter, AudioBusNodeSetter, Sys, Synth, Group, Server, Resource, Txn}
import scala.concurrent.stm.{Txn => ScalaTxn}
import de.sciss.synth.{addToHead, ControlSet}
import de.sciss.numbers

object AuralPresentationImpl {
  //  def run[S <: Sys[S]](transport: ProcTransport[S], aural: AuralSystem): AuralPresentation[S] = {
  //    val c = new Client[S](transport, aural)
  //    aural.addClient(c)
  //    atomic { implicit itx =>
  //      implicit val ptx = Txn.wrap(itx)
  //      aural.serverOption.foreach { s =>
  //        ScalaTxn.afterCommit(_ => c.started(s))
  //      }
  //    }
  //    c
  //  }

  def run[S <: Sys[S]](transport: ProcTransport[S], aural: AuralSystem)(implicit tx: S#Tx): AuralPresentation[S] = {
    val c = new Client[S](transport, aural)
    aural.addClient(c)
    aural.serverOption.foreach(c.startedTx)
    c
  }

  private final class Client[S <: Sys[S]](transport: ProcTransport[S], aural: AuralSystem)
    extends AuralPresentation[S] with AuralSystem.Client {

    override def toString = "AuralPresentation@" + hashCode.toHexString

    private val running   = Ref(Option.empty[RunningImpl[S]])
    private val groupRef  = Ref(Option.empty[Group])

    def dispose()(implicit tx: S#Tx) = {
      aural.removeClient(this)
      val rOpt = running.swap(None)(tx.peer)
      rOpt.foreach(_.dispose())
    }

    def stopped()(implicit tx: Txn): Unit = {
      // aural.removeClient(this)
      val rOpt = running.swap(None)(tx.peer)
      rOpt.foreach { r =>
        ScalaTxn.afterCommit { _ =>
          transport.cursor.step { implicit tx =>
            r.dispose()
          }
        } (tx.peer)
      }
    }

    def group(implicit tx: S#Tx): Option[Group] = groupRef.get(tx.peer)

    def started(server: Server)(implicit tx: Txn): Unit =
      ScalaTxn.afterCommit { _ =>
        transport.cursor.step { implicit tx =>
          startedTx(server)
        }
      } (tx.peer)

    def stopAll(implicit tx: S#Tx): Unit =
      group.foreach(_.freeAll())

    def startedTx(server: Server)(implicit tx: S#Tx): Unit = {
      // implicit val itx: I#Tx = tx
      // println("startedTx")

      val viewMap: IdentifierMap[S#ID, S#Tx, AuralNode]                         = tx.newInMemoryIDMap
      val scanMap: IdentifierMap[S#ID, S#Tx, (String, stm.Source[S#Tx, S#ID])]  = tx.newInMemoryIDMap

      val group = Group(server)
      //         group.play( target = server.defaultGroup ) // ( ProcTxn()( tx ))
      groupRef.set(Some(group))(tx.peer)

      val booted = new RunningImpl(server, group, viewMap, scanMap, transport.sampleRate /*, artifactStore */)
      logA(s"started (${booted.hashCode.toHexString})")
      NodeGraph.addServer(server) // ( ProcTxn()( tx ))

      def t_play(time: Long)(implicit tx: S#Tx): Unit =
        transport.iterator.foreach {
          case (_, timed) => booted.procAdded(time, timed)
        }

      def t_stop(time: Long)(implicit tx: S#Tx): Unit =
        transport.iterator.foreach {
          case (_, timed) => booted.procRemoved(timed)
        }

      if (transport.isPlaying) t_play(transport.time)

      val tObs = transport.react { implicit tx => {
        // only when playing
        case Transport.Advance(tr, time, isSeek, true, added, removed, changes) =>
          logA(s"at $time added ${added.mkString("[", ", ", "]")}; removed ${removed.mkString("[", ", ", "]")}; " +
            s"changes? ${changes.nonEmpty} (${booted.hashCode.toHexString})")
          removed.foreach {
            timed => booted.procRemoved(timed)
          }
          changes.foreach {
            case (timed, m) => booted.procUpdated(timed, m)
          }
          added.foreach { timed =>
            val mute = timed.value.attributes[Elem.Boolean]("mute").exists(_.value)
            if (!mute) booted.procAdded(time, timed)
          }

        case Transport.Play(tr, time) => t_play(time)
        case Transport.Stop(tr, time) => t_stop(time)

        case _ =>
      }}

      implicit val itx = tx.peer
      booted.transportObserver = tObs
      running() = Some(booted)
    }
  }

  private final class OutputBuilder(val bus: AudioBus) {
    var sinks = List.empty[(String, AuralNode)]
  }

  private final class AuralProcBuilder[S <: Sys[S]](val ugen: UGenGraphBuilder[S] /*, val name: String */) {
    var outputs = Map.empty[String, OutputBuilder]
  }

   /*
    * @param missingMap maps each missing input to a set of builders who requested that input
    * @param idMap      map's timed-ids to aural proc builders
    * @param seq        sequence of builders in the current transaction
    */
  private final class OngoingBuild[S <: Sys[S]](var missingMap: Map[MissingIn[S], Set[AuralProcBuilder[S]]] =
                                                  Map.empty[MissingIn[S], Set[AuralProcBuilder[S]]],
                                                var idMap: Option[IdentifierMap[S#ID, S#Tx, AuralProcBuilder[S]]] =
                                                  None,
                                                var seq: Vec[AuralProcBuilder[S]] = Vec.empty) {
    override def toString = "OngoingBuild(missingMap = " + missingMap + ", idMap = " + idMap + ", seq = " + seq + ")"
  }

  private final class RunningImpl[S <: Sys[S]](server: Server, group: Group,
                                               viewMap: IdentifierMap[S#ID, S#Tx, AuralNode],
                                               scanMap: IdentifierMap[S#ID, S#Tx, (String, stm.Source[S#Tx, S#ID])],
                                               sampleRate: Double)
  //                                               artifactStore: stm.Source[S#Tx, ArtifactStore[S]]
    extends AuralPresentation.Running[S] {

    override def toString = s"AuralPresentation.Running@${hashCode.toHexString}"

    // for simplicity. must be set by within the transaction that creates the RunningImpl
    var transportObserver: stm.Disposable[S#Tx] = _

    // ongoingBuild is a transaction local field storing a mutable object. This is
    // totally fine since a transaction is not shared across threads. the idea is
    // that a fresh object is retrieved for each new transaction. If it is touched,
    // a beforeCommit hook is invoked before the transaction successfully completes,
    // building the unfinished aural procs if possible.
    //      private val ongoingBuild: TxnLocal[ OngoingBuild[ S ]] =
    //         TxnLocal( init = OngoingBuild(), beforeCommit = beforeCommit )

    private val ongoingBuild = TxnLocal(new OngoingBuild[S]())

    //      private def getNumChannels( timed: TimedProc[ S ], key: String )( implicit tx: S#Tx ) : Int = {
    //         viewMap.get( timed.id ).flatMap({ aural =>
    //            implicit val ptx = ProcTxn()( tx.peer )
    //            aural.getBus( key ).map( _.numChannels )
    //         }).getOrElse( throw MissingIn( timed, key ))
    //      }

    private def launchProc(builder: AuralProcBuilder[S]): Unit = {
      val ugen          = builder.ugen
      val timed         = ugen.timed

      logA(s"begin launch $timed (${hashCode.toHexString})")

      val ug            = ugen.finish   // finalise the ugen graph
      implicit val tx   = ugen.tx
      implicit val itx  = tx.peer

      val time          = ugen.time
      val p             = timed.value

      val nameHint      = p.attributes[Elem.String](ProcKeys.attrName).map(_.value)
      val synth         = Synth.expanded(server, ug, nameHint = nameHint)
      // users are elements which must be added after the aural proc synth is started, and removed when it stops
      var users         = List.empty[DynamicUser]
      // resources are dependencies in terms of synth bundle spawning, and will be disposed by the aural proc
      var dependencies  = List.empty[Resource]

      // ---- handle input buses ----
      val span          = timed.span.value
      var setMap        = Vec[ControlSet](
        graph.Time    .key -> time / sampleRate,
        graph.Offset  .key -> (span match {
          case Span.HasStart(start) => (time - start) / sampleRate
          case _ => 0.0
        }),
        graph.Duration.key -> (span match {
          case Span(start, stop)  => (stop - start) / sampleRate
          case _ => Double.PositiveInfinity
        })
      )

      // ---- attributes ----
      val attrNames     = ugen.attributeIns
      if (attrNames.nonEmpty) attrNames.foreach { n =>
        val ctlName = graph.attribute.controlName(n)
        p.attributes.get(n).foreach {
          case a: Elem.Int     [S] => setMap :+= (ctlName -> a.peer.value.toFloat: ControlSet)
          case a: Elem.Double  [S] => setMap :+= (ctlName -> a.peer.value.toFloat: ControlSet)
          case a: Elem.Boolean [S] => setMap :+= (ctlName -> (if (a.peer.value) 1f else 0f): ControlSet)
          case a: Elem.FadeSpec[S] =>
            val spec = a.peer.value
            // dur, shape-id, shape-curvature, floor
            val values = Vec(
              (spec.numFrames / sampleRate).toFloat, spec.curve.id.toFloat, spec.curve match {
                case parametric(c)  => c
                case _              => 0f
              }, spec.floor
            )
            setMap :+= (ctlName -> values: ControlSet)
          case a: Elem.DoubleVec[S] =>
            val values = a.peer.value.map(_.toFloat)
            setMap :+= (ctlName -> values: ControlSet)
          case a: Elem.AudioGrapheme[S] =>
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
          val (rb, gain) = p.attributes.get(n).fold[(Buffer, Float)] {
            // DiskIn and VDiskIn are fine with an empty non-streaming buffer, as far as I can tell...
            // So instead of aborting when the attribute is not set, fall back to zero
            val _buf = Buffer(server)(numFrames = bufSize, numChannels = 1)
            (_buf, 0f)
          } {
            case a: Elem.AudioGrapheme[S] =>
              val audioElem = a.peer
              val spec      = audioElem.spec
              val path      = audioElem.artifact.value.getAbsolutePath
              val offset    = audioElem.offset  .value
              val _gain     = audioElem.gain    .value
              val interp    = info.interp
              val _buf      = if (interp == 4) {
                Buffer.diskIn(server)(
                  path          = path,
                  startFrame    = offset,
                  numFrames     = bufSize,
                  numChannels   = spec.numChannels
                )
              } else {
                val __buf = Buffer(server)(numFrames = bufSize, numChannels = spec.numChannels)
                val trig = new StreamBuffer(key = n, idx = idx, synth = synth, buf = __buf, path = path,
                                            fileFrames = spec.numFrames, interp = interp)
                trig.install()
                __buf
              }
              (_buf, _gain.toFloat)

            case a => sys.error(s"Cannot use attribute $a as an audio stream")
          }
          setMap      :+= (ctlName -> Seq[Float](rb.id, gain): ControlSet)
          dependencies        ::= rb
        }
      }

      import Grapheme.Segment
      val outBuses      = builder.outputs
      val aural         = AuralNode(synth, outBuses.mapValues(_.bus))

      // ---- scans ----
      ugen.scanIns.foreach {
        case (key, scanIn) =>
          val numCh = scanIn.numChannels

          @inline def ensureChannels(n: Int): Unit =
            require(n == numCh, s"Scan input changed number of channels (expected $numCh but found $n)")

          val inCtlName = scan.inControlName(key)
          // var inBus     = Option.empty[AudioBusNodeSetter]

          lazy val lazyInBus: AudioBusNodeSetter = {
            val b      = Bus.audio(server, numCh)
            val res    = if (scanIn.fixed)
              BusNodeSetter.reader(inCtlName, b, synth)
            else
              BusNodeSetter.mapper(inCtlName, b, synth)
            users ::= res
            aural.addInputBus(key, res.bus)
            res
          }

          // note: if not found, stick with default

          // XXX TODO: combination fixed + grapheme source doesn't work -- as soon as there's a bus mapper
          //           we cannot use ControlSet any more, but need other mechanism
          p.scans.get(key).foreach { scan =>
            val src = scan.sources
            // if (src.isEmpty) {
              if (scanIn.fixed) lazyInBus  // make sure a fixed channels scan in exists as a bus
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
                      val bm     = lazyInBus
                      val w      = SegmentWriter(bm.bus, segm, time, sampleRate)
                      dependencies     ::= w
                      // users ::= w

                    case audio: Segment.Audio =>
                      ensureChannels(audio.numChannels)
                      val bm     = lazyInBus
                      val w      = AudioArtifactWriter(bm.bus, audio, time, sampleRate)
                      dependencies     ::= w
                      // users    ::= w
                  }

                case Link.Scan(peer) =>
                  scanMap.get(peer.id).foreach {
                    case (srcKey, idH) =>
                      val srcTimedID  = idH()
                      val bIn         = lazyInBus

                      // if the source isn't found (because it's probably in the ongoing build),
                      // we ignore that here; there is a symmetric counter part, looking for the
                      // builder.outputs that will handle these cases.
                      viewMap.get(srcTimedID).foreach { srcAural =>
                        val bOut    = srcAural.getOutputBus(srcKey).getOrElse {
                          sys.error(s"Source bus disappeared $srcTimedID -> $srcKey")
                        }
                        ensureChannels(bOut.numChannels)
                        val edge    = NodeGraph.Edge(srcAural, srcKey, aural, key)
                        val link    = AudioLink(edge, sourceBus = bOut, sinkBus = bIn.bus)
                        dependencies      ::= link
                        users     ::= link
                      }
                  }
              }
            // }
          }
      }

      // ---- handle output buses, and establish missing links to sinks ----
      builder.outputs.foreach {
        case (key, out) =>
          val bw     = BusNodeSetter.writer(scan.outControlName(key), out.bus, synth)
          val bOut   = bw.bus
          users :+= bw

          p.scans.get(key).foreach { scan =>
            scan.sinks.foreach {
              case Link.Scan(peer) =>
                scanMap.get(peer.id).foreach {
                  case (sinkKey, idH) =>
                    val sinkTimedID = idH()
                    viewMap.get(sinkTimedID).foreach { sinkAural =>
                      val bIn = sinkAural.getInputBus(sinkKey).getOrElse {
                        sys.error(s"Sink bus disappeared $sinkTimedID -> $sinkKey")
                      }
                      require(bIn.numChannels == bOut.numChannels,
                        s"Scan input changed number of channels (expected ${bOut.numChannels} but found ${bIn.numChannels})")
                      val edge    = NodeGraph.Edge(aural, key, sinkAural, sinkKey)
                      val link    = AudioLink(edge, sourceBus = bOut, sinkBus = bIn)
                      dependencies      ::= link
                      users     ::= link
                    }
                }

              case _  =>
            }
          }
      }

      // busUsers.foreach(_.add())

      aural.init(users, dependencies)
      // wrap as AuralProc and save it in the identifier map for later lookup
      synth.play(target = group, addAction = addToHead, args = setMap, dependencies = dependencies)
      if (users.nonEmpty) users.foreach(_.add())

      // if (setMap.nonEmpty) synth.set(audible = true, setMap: _*)
      logA(s"launched $timed -> $aural (${hashCode.toHexString})")
      viewMap.put(timed.id, aural)
    }

    // called before the transaction successfully completes.
    // this is the place where we launch completely built procs.
    private def flush()(ptx: Txn): Unit = {
      val itx = ptx.peer
      ongoingBuild.get(itx).seq.foreach { builder =>
        val ugen = builder.ugen
        if (ugen.isComplete) {
          try {
            launchProc(builder)
          } catch {
            case NonFatal(e) =>
              e.printStackTrace()
              throw e
          }

        } else {
          // XXX TODO: do we need to free buses associated with ugen.scanOuts ?
          println("Warning: Incomplete aural proc build for " + ugen.timed.value)
        }
      }
    }

    private def getOutputBus(timedID: S#ID, key: String)(implicit tx: S#Tx): Option[AudioBus] =
      viewMap.get(timedID) match {
        case Some(aural) =>
          aural.getOutputBus(key)
        case _ =>
          assert(ongoingBuild.isInitialized(tx.peer))
          val ob = ongoingBuild.get(tx.peer)
          for {
            map <- ob.idMap
            pb  <- map.get(timedID)
            out <- pb.outputs.get(key)
          } yield out.bus
      }

    // called by UGenGraphBuilderImpl
    def attrNumChannels(timed: TimedProc[S], key: String)(implicit tx: S#Tx): Int =
      timed.value.attributes.get(key).fold(1) {
        case a: Elem.DoubleVec[S]      => a.peer.value.size // XXX TODO: would be better to write a.peer.size.value
        case a: Elem.AudioGrapheme[S]  => a.peer.spec.numChannels
        case _ => 1
      }

    // called by UGenGraphBuilderImpl
    def scanInNumChannels(timed: TimedProc[S], time: Long, key: String, numChannels: Int)(implicit tx: S#Tx): Int = {
      val numCh = timed.value.scans.get(key).fold(0) { scan =>
        val chans = scan.sources.toList.map {
          case Link.Grapheme(peer) =>
            val chansOpt = peer.valueAt(time).map(_.numChannels)
            chansOpt.getOrElse(numChannels)

          case Link.Scan(peer) =>
            val sourceOpt = scanMap.get(peer.id)
            val busOpt    = sourceOpt.flatMap {
              case (sourceKey, idH) =>
                val sourceTimedID = idH()
                getOutputBus(sourceTimedID, sourceKey)
            }
            busOpt.map(_.numChannels).getOrElse {
              if (numChannels < 0) throw MissingIn(peer)
              numChannels
            }
        }
        if (chans.isEmpty) 0 else chans.max
      }
      math.max(1, numCh)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      viewMap.dispose()
      transportObserver.dispose()
    }

    //      private def addFlush()( implicit ptx: Txn ) {
    //         ptx.beforeCommit( flush()( _ ))
    //      }

    private def addFlush()(implicit tx: S#Tx): Unit = {
      logA(s"addFlush (${hashCode.toHexString})")
      tx.beforeCommit(flush()(_))
      // concurrent.stm.Txn.afterRollback(status => logA(s"rollback $status !!"))(tx.peer)
    }

    def procAdded(time: Long, timed: TimedProc[S])(implicit tx: S#Tx): Unit = {
      // val name = timed.value.name.value
      logA(s"added $timed (${hashCode.toHexString})")

      val timedID = timed.id
      val ugen    = UGenGraphBuilder(this, timed, time)
      val builder = new AuralProcBuilder(ugen /*, name */)
      val newTxn  = !ongoingBuild.isInitialized(tx.peer)
      if (newTxn) addFlush() // ( ProcTxn() )   // the next line (`ongoingBuild.get`) will initialise then
      val ongoing = ongoingBuild.get(tx.peer)
      ongoing.seq :+= builder
      // assert(ongoingBuild.isInitialized(tx.peer))

      // initialise the id-to-builder map if necessary
      val builderMap = ongoing.idMap.getOrElse {
        val m = tx.newInMemoryIDMap[AuralProcBuilder[S]]
        ongoing.idMap = Some(m)
        m
      }
      // add the builder to it.
      builderMap.put(timedID, builder)

      // store the look up information for the scans
      // (this is only needed because Scan.Link.Scan reveals
      // only the Scan which in turn doesn't currently carry
      // key and proc information, so it can't be recovered
      // otherwise; in the future this may change)
      val scans = timed.value.scans
      scans.iterator.foreach {
        case (key, scan) =>
          import de.sciss.lucre.synth.expr.IdentifierSerializer
          scanMap.put(scan.id, key -> tx.newHandle(timedID))
      }

      incrementalBuild(ongoing, builder)
    }

    // note: builder.outBuses will be updated by this method
    private def incrementalBuild(ongoing: OngoingBuild[S], builder: AuralProcBuilder[S])(implicit tx: S#Tx): Unit = {
      // simpler algorithm (does not allow for circular relationships):
      // - just store with the missing keys, and wait for other finished ugs to show up within the txn

      // more inquisitive algorithm:
      // - register scanIns
      // - register scanOuts
      // - register missingScanIns
      // - look up (one, arbitrary) missingScanIn for any of the scanOuts
      // - if found, continue building

      val ugen        = builder.ugen
      val isComplete  = ugen.tryBuild()

      logA(s"incremental ${ugen.timed}; completed? $isComplete (${hashCode.toHexString})")

      // detect which new scan outputs have been determined in the last iteration
      // (newOuts is a map from `name: String` to `numChannels Int`)
      val newOuts = ugen.scanOuts.filterNot {
        case (key, _) => builder.outputs.contains(key)
      }
      // if there were any, create rich audio buses for them, and store them in the builder's bus map.
      //    note that these buses initially do not have any real resources allocated, so it's safe to
      //    forget about them and have them garbage-collected if the process does not complete by the end of the txn.
      if (newOuts.nonEmpty) {
        val newBuses = newOuts.mapValues(numCh => new OutputBuilder(Bus.audio(server, numCh)))
        builder.outputs ++= newBuses
      }

      // if the last iteration did not complete the build process, store the missing in keys
      // (since missingMap is a map and the values are sets, it is safe to re-add existing entries)
      if (!isComplete) {
        var newMissing = ongoing.missingMap
        ugen.missingIns.foreach { miss =>
          newMissing += miss -> (newMissing.getOrElse(miss, Set.empty) + builder)
        }
        ongoing.missingMap = newMissing
      }

      // if new scan outputs have been determined, find out whether other incomplete
      // processes depend on them, so that their building might be advanced
      val retry = if (newOuts.nonEmpty) {
        // the retried entries are those whose missing scan ins contain
        // any of the newly determined scan outs
        val scans = ugen.timed.value.scans
        val keys: Set[MissingIn[S]] = newOuts.flatMap({
          case (key, _) =>
            val scanOpt = scans.get(key)
            scanOpt.map {
              scan => MissingIn(scan)
            }
        })(breakOut)
        // divide missing map according to these keys
        val (retE, keep) = ongoing.missingMap.partition {
          case (key, _) => keys.contains(key)
        }
        // merge all the found builder sets together
        val ret: Set[AuralProcBuilder[S]] = retE.flatMap(_._2)(breakOut)
        // ...and update the ongoing information by removing the builders to retry from the missing map
        // (they will add themselves again in the recursive `afterIncr` call)
        ongoing.missingMap = keep
        ret

      } else {
        Set.empty[AuralProcBuilder[S]]
      }

      // advance the ugen graph builder for all processes which have been selected for retry
      retry.foreach(r => incrementalBuild(ongoing, r))
    }

    def procRemoved(timed: TimedProc[S])(implicit tx: S#Tx): Unit = {
      val timedID = timed.id
      viewMap.get(timedID) match {
        case Some(aural) =>
          viewMap.remove(timedID)
          //               implicit val ptx = ProcTxn()( tx )
          logA(s"removed $timed (${hashCode.toHexString})") // + " -- playing? " + aural.playing )
          aural.stop()

        case _ =>
          def warn(): Unit = {
            val mute = timed.value.attributes[Elem.Boolean]("mute").exists(_.value)
            if (!mute) println("WARNING: could not find aural view for " + timed)
          }

          val newTxn  = !ongoingBuild.isInitialized(tx.peer)
          if (newTxn) addFlush()
          val ongoing = ongoingBuild.get(tx.peer)
          ongoing.idMap match {
            case Some(idMap) => idMap.get(timedID) match {
              case Some(builder) =>
                idMap.remove(timedID)
                ongoing.seq = ongoing.seq.filterNot(_ == builder)

              case _ => warn()
            }
            case _ => warn()
          }
      }

      // remove auxiliary scan map (see procAdded)
      val scans = timed.value.scans
      scans.iterator.foreach {
        case (key, scan) =>
          scanMap.remove(scan.id)
      }
    }

    def procUpdated(timed: TimedProc[S], change: Transport.Proc.Update[S])(implicit tx: S#Tx): Unit = {
      // XXX TODO !
    }
  }
}