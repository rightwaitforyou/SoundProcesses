/*
 *  AuralPresentationImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package proc
package impl

import de.sciss.lucre.stm
import stm.IdentifierMap
import collection.breakOut
import collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{TxnExecutor, Ref, TxnLocal, Txn => ScalaTxn}
import proc.{logAural => log}
import UGenGraphBuilder.MissingIn
import graph.scan
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import TxnExecutor.{defaultAtomic => atomic}
import de.sciss.span.Span
import de.sciss.synth.Curve.parametric
import de.sciss.synth.proc.Scan.Link
import scala.util.control.NonFatal

object AuralPresentationImpl {
  def run[S <: Sys[S]](transport: ProcTransport[S], aural: AuralSystem): AuralPresentation[S] = {
    val c = new Client[S](transport, aural)
    aural.addClient(c)
    atomic { implicit itx =>
      implicit val ptx = Txn.applyPlain(itx)
      aural.serverOption.foreach { s =>
        ScalaTxn.afterCommit(_ => c.started(s))
      }
    }
    c
  }

  def runTx[S <: Sys[S]](transport: ProcTransport[S], aural: AuralSystem)(implicit tx: S#Tx): AuralPresentation[S] = {
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

    def dispose()(implicit tx: S#Tx) = () // XXX TODO dispose running

    def stopped(): Unit = {
      // implicit val itx: I#Tx = tx
      aural.removeClient(this)
      running.single() = None // XXX TODO dispose
    }

    def group(implicit tx: S#Tx): Option[Group] = groupRef.get(tx.peer)

    def started(server: Server): Unit =
      transport.cursor.step { implicit tx =>
        startedTx(server)
      }

    def stopAll(implicit tx: S#Tx): Unit =
      group.foreach(_.freeAll())

    def startedTx(server: Server)(implicit tx: S#Tx): Unit = {
      // implicit val itx: I#Tx = tx
      // println("startedTx")

      val viewMap: IdentifierMap[S#ID, S#Tx, AuralProc]                         = tx.newInMemoryIDMap
      val scanMap: IdentifierMap[S#ID, S#Tx, (String, stm.Source[S#Tx, S#ID])]  = tx.newInMemoryIDMap

      val group = Group(server)
      //         group.play( target = server.defaultGroup ) // ( ProcTxn()( tx ))
      groupRef.set(Some(group))(tx.peer)

      val booted = new RunningImpl(server, group, viewMap, scanMap, transport.sampleRate /*, artifactStore */)
      log(s"started (${booted.hashCode.toHexString})")
      ProcDemiurg.addServer(server) // ( ProcTxn()( tx ))

      def t_play(time: Long)(implicit tx: S#Tx): Unit =
        transport.iterator.foreach {
          case (_, timed) => booted.procAdded(time, timed)
        }

      def t_stop(time: Long)(implicit tx: S#Tx): Unit =
        transport.iterator.foreach {
          case (_, timed) => booted.procRemoved(timed)
        }

      if (transport.isPlaying) t_play(transport.time)

      transport.react { implicit tx => {
        // only when playing
        case Transport.Advance(tr, time, isSeek, true, added, removed, changes) =>
          log(s"at $time added ${added.mkString("[", ", ", "]")}; removed ${removed.mkString("[", ", ", "]")}; " +
            s"changes? ${changes.nonEmpty} (${booted.hashCode.toHexString})")
          removed.foreach {
            timed => booted.procRemoved(timed)
          }
          changes.foreach {
            case (timed, m) => booted.procUpdated(timed, m)
          }
          added.foreach { timed =>
            val mute = timed.value.attributes[Attribute.Boolean]("mute").exists(_.value)
            if (!mute) booted.procAdded(time, timed)
          }

        case Transport.Play(tr, time) => t_play(time)
        case Transport.Stop(tr, time) => t_stop(time)

        case _ =>
      }}

      implicit val itx = tx.peer
      running() = Some(booted)
    }
  }

  private final class OutputBuilder(val bus: RichAudioBus) {
    var sinks = List.empty[(String, AuralProc)]
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
                                               viewMap: IdentifierMap[S#ID, S#Tx, AuralProc],
                                               scanMap: IdentifierMap[S#ID, S#Tx, (String, stm.Source[S#Tx, S#ID])],
                                               sampleRate: Double)
  //                                               artifactStore: stm.Source[S#Tx, ArtifactStore[S]]
    extends AuralPresentation.Running[S] {

    override def toString = "AuralPresentation.Running@" + hashCode.toHexString

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

      log(s"begin launch $timed (${hashCode.toHexString})")

      val ug            = ugen.finish   // finalise the ugen graph
      implicit val tx   = ugen.tx
      implicit val itx  = tx.peer

      val time          = ugen.time
      val p             = timed.value

      val nameHint      = p.attributes[Attribute.String](ProcKeys.attrName).map(_.value)
      val synth         = Synth.expanded(server, ug, nameHint = nameHint)

      // ---- handle input buses ----
      var busUsers      = List.empty[DynamicBusUser]
      val span          = timed.span.value
      var setMap        = Vec[ControlSetMap](
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
      var deps          = List.empty[Resource.Source]

      val attrNames     = ugen.attributeIns
      if (attrNames.nonEmpty) {
        // println(s"Attributes used: ${attrNames.mkString(", ")}")
        attrNames.foreach { n =>
          val ctlName = graph.attribute.controlName(n)
          val csm: Option[ControlSetMap] = p.attributes.get(n).map {
            case a: Attribute.Int     [S] => ctlName -> a.peer.value.toFloat
            case a: Attribute.Double  [S] => ctlName -> a.peer.value.toFloat
            case a: Attribute.Boolean [S] => ctlName -> (if (a.peer.value) 1f else 0f)
            case a: Attribute.FadeSpec[S] =>
              val spec = a.peer.value
              // dur, shape-id, shape-curvature, floor
              ctlName -> Vec(
                (spec.numFrames / sampleRate).toFloat, spec.curve.id.toFloat, spec.curve match {
                  case parametric(c)  => c
                  case _              => 0f
                }, spec.floor
              )
            case a => sys.error(s"Cannot cast attribute $a to a scalar value")
          }
          csm.foreach(setMap :+= _)
        }
      }

      import Grapheme.Segment

      val outBuses  = builder.outputs
      val aural     = AuralProc(synth, outBuses.mapValues(_.bus))

      ugen.scanIns.foreach {
        case (key, numCh) =>

          @inline def ensureChannels(n: Int): Unit =
            require(n == numCh, s"Scan input changed number of channels (expected $numCh but found $n)")

          val inCtlName = scan.inControlName(key)
          // var inBus     = Option.empty[AudioBusNodeSetter]

          lazy val lazyInBus: AudioBusNodeSetter = {
            val b      = RichBus.audio(server, numCh)
            val res    = BusNodeSetter.mapper(inCtlName, b, synth)
            busUsers ::= res
            // inBus      = Some(res)
            aural.addInputBus(key, res.bus)
            res
          }

          // note: if not found, stick with default
          p.scans.get(key).foreach { scan =>
            scan.sources.foreach {
              case Link.Grapheme(peer) =>
                val segmOpt = peer.segment(time)
                segmOpt.foreach {
                  // again if not found... stick with default
                  case const: Segment.Const =>
                    ensureChannels(const.numChannels) // ... or could just adjust to the fact that they changed
                    //                        setMap :+= ((key -> const.numChannels) : ControlSetMap)
                    setMap :+= (if (const.numChannels == 1) {
                      ControlSetMap.Single(inCtlName, const.values.head .toFloat )
                    } else {
                      ControlSetMap.Multi (inCtlName, const.values.map(_.toFloat))
                    })

                  case segm: Segment.Curve =>
                    ensureChannels(segm.numChannels) // ... or could just adjust to the fact that they changed
                    // println(s"segment : ${segm.span}")
                    val bm     = lazyInBus
                    val w      = SegmentWriter(bm.bus, segm, time, sampleRate)
                    deps     ::= w
                    busUsers ::= w

                  case audio: Segment.Audio =>
                    ensureChannels(audio.numChannels)
                    val bm     = lazyInBus
                    val w      = AudioArtifactWriter(bm.bus, audio, time, sampleRate)
                    deps     ::= w
                    busUsers ::= w
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
                      val edge    = ProcEdge(srcAural, srcKey, aural, key)
                      val link    = AudioLink(edge, sourceBus = bOut, sinkBus = bIn.bus)
                      deps      ::= link
                      // deps      ::= srcAural.group()
                      busUsers  ::= link
                    }
                }
            }
          }
      }

      // ---- handle output buses, and establish missing links to sinks ----
      builder.outputs.foreach {
        case (key, out) =>
          val bw     = BusNodeSetter.writer(scan.outControlName(key), out.bus, synth)
          val bOut   = bw.bus
          busUsers :+= bw

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
                      val edge    = ProcEdge(aural, key, sinkAural, sinkKey)
                      val link    = AudioLink(edge, sourceBus = bOut, sinkBus = bIn)
                      deps      ::= link
                      busUsers  ::= link
                    }
                }

              case _  =>
            }
          }
      }

      // busUsers.foreach(_.add())

      aural.setBusUsers(busUsers)
      // wrap as AuralProc and save it in the identifier map for later lookup
      val deps1 = deps.map(_.resource)
      synth.play(target = group, addAction = addToHead, args = setMap, dependencies = deps1)
      busUsers.foreach(_.add())

      // if (setMap.nonEmpty) synth.set(audible = true, setMap: _*)
      log(s"launched $timed -> $aural (${hashCode.toHexString})")
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

    private def getOutputBus(timedID: S#ID, key: String)(implicit tx: S#Tx): Option[RichAudioBus] =
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
    def scanInNumChannels(timed: TimedProc[S], time: Long, key: String)(implicit tx: S#Tx): Int = {
      val numCh = timed.value.scans.get(key).fold(0) { scan =>
        val chans = scan.sources.toList.map {
          case Link.Grapheme(peer) =>
            val chansOpt = peer.valueAt(time).map(_.numChannels)
            chansOpt.getOrElse(1) // producing a non-mapped monophonic control with default value; sounds sensible?

          case Link.Scan(peer) =>
            val sourceOpt = scanMap.get(peer.id)
            val busOpt = sourceOpt.flatMap {
              case (sourceKey, idH) =>
                val sourceTimedID = idH()
                getOutputBus(sourceTimedID, sourceKey)
            }
            val bus = busOpt.getOrElse(throw MissingIn(peer))
            bus.numChannels
        }
        if (chans.isEmpty) 0 else chans.max
      }
      math.max(1, numCh)
    }

    def dispose()(implicit tx: S#Tx): Unit =
      viewMap.dispose()

    //      private def addFlush()( implicit ptx: Txn ) {
    //         ptx.beforeCommit( flush()( _ ))
    //      }

    private def addFlush()(implicit tx: S#Tx): Unit = {
      log(s"addFlush (${hashCode.toHexString})")
      tx.beforeCommit(flush()(_))
      // concurrent.stm.Txn.afterRollback(status => log(s"rollback $status !!"))(tx.peer)
    }

    def procAdded(time: Long, timed: TimedProc[S])(implicit tx: S#Tx): Unit = {
      // val name = timed.value.name.value
      log(s"added $timed (${hashCode.toHexString})")

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
          import CommonSerializers.Identifier
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

      log(s"incremental ${ugen.timed}; completed? $isComplete (${hashCode.toHexString})")

      // detect which new scan outputs have been determined in the last iteration
      // (newOuts is a map from `name: String` to `numChannels Int`)
      val newOuts = ugen.scanOuts.filterNot {
        case (key, _) => builder.outputs.contains(key)
      }
      // if there were any, create rich audio buses for them, and store them in the builder's bus map.
      //    note that these buses initially do not have any real resources allocated, so it's safe to
      //    forget about them and have them gc'ed if the process does not complete by the end of the txn.
      if (newOuts.nonEmpty) {
        val newBuses = newOuts.mapValues(numCh => new OutputBuilder(RichBus.audio(server, numCh)))
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
          log(s"removed $timed (${hashCode.toHexString})") // + " -- playing? " + aural.playing )
          aural.stop()

        case _ =>
          def warn(): Unit = {
            val mute = timed.value.attributes[Attribute.Boolean]("mute").exists(_.value)
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

    //      def procGraphChanged( timed: TimedProc[ S ], newGraph: SynthGraph )( implicit tx: S#Tx ): Unit = {
    //         viewMap.get( timed.id ) match {
    //            case Some( aural ) =>
    //               implicit val ptx = ProcTxn()( tx.peer )
    //               logConfig( "aural graph changed " + timed.value )
    //               aural.graph_=( newGraph )
    //            case _ =>
    //               println( "WARNING: could not find aural view for proc " + timed.value )
    //         }
    //      }

    def procUpdated(timed: TimedProc[S], change: Transport.Proc.Update[S])(implicit tx: S#Tx): Unit = {
      // XXX TODO !
      //         viewMap.get( timed.id ) match {
      //            case Some( aural ) =>
      //               implicit val ptx = ProcTxn()( tx.peer )
      //               logConfig( "aural freq changed " + timed.value )
      //               aural.addParams( changes )
      //            case _ =>
      //               println( "WARNING: could not find aural view for proc " + timed.value )
      //         }
    }
  }
}