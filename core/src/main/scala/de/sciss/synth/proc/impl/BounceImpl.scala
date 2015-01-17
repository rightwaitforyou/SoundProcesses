/*
 *  BounceImpl.scala
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

import java.io.{RandomAccessFile, File}
import java.nio.ByteBuffer

import de.sciss.lucre.stm
import de.sciss.lucre.synth.{Sys, Server}
import de.sciss.osc
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.synth.{Server => SServer}
import de.sciss.synth.io.{SampleFormat, AudioFileType}

import scala.annotation.tailrec
import scala.concurrent.{Future, Promise, Await, blocking}
import scala.concurrent.duration.Duration
import scala.util.Success

final class BounceImpl[S <: Sys[S], I <: stm.Sys[I]](implicit cursor: stm.Cursor[S], bridge: S#Tx => I#Tx,
                                                     workspace: WorkspaceHandle[S])
  extends Bounce[S] {

  protected def prepare(config: Config): Prepared = {
    if (config.server.sampleRate <= 0)
      throw new IllegalArgumentException("The sample-rate of the server configuration must be explicitly specified")

    new Impl(config)
  }

  private final class Impl(config: Config) extends ProcessorImpl[Product, Processor[File]]
    with Processor[File] {

    private val needsOSCFile  = config.server.nrtCommandPath.isEmpty  // we need to generate that file
    private val needsDummyOut = config.server.outputBusChannels == 0  // scsynth doesn't allow this. must have 1 dummy channel
    private val needsOutFile  = config.server.nrtOutputPath.isEmpty && !needsDummyOut // we need to generate

    private var oscFile     : File = null
    private var outFile     : File = null
    private var aural: AuralSystem = null

    override protected def cleanUp(): Unit = {
      if (needsOSCFile  && oscFile != null) oscFile.delete()
      if (needsDummyOut && outFile != null) outFile.delete()
      if (aural != null) cursor.step { implicit tx => aural.stop() }
    }

    protected def body(): File = {
      // ---- configuration ----

      // the server config (either directly from input, or updated according to the necessary changes)
      val sCfg = if (needsOSCFile || needsDummyOut || needsOutFile) {
        val b = Server.ConfigBuilder(config.server)
        if (needsOSCFile) {
          val f = File.createTempFile("bounce", ".osc")
          b.nrtCommandPath = f.getCanonicalPath
        }
        if (needsDummyOut) {
          b.nrtHeaderFormat   = AudioFileType.AIFF
          b.nrtSampleFormat   = SampleFormat.Int16
          b.outputBusChannels = 1
        }
        if (needsDummyOut || needsOutFile) {
          outFile = File.createTempFile("bounce", s".${b.nrtHeaderFormat.extension}")
          b.nrtOutputPath = outFile.getCanonicalPath
        }
        b.build
      } else {
        config.server
      }

      // ---- run transport and gather OSC ----

      val server = Server.offline(sCfg)

      val (span, scheduler, transport, __aural) = cursor.step { implicit tx =>
        val _scheduler  = Scheduler.offline[S]
        val _span       = config.span

        // val _transp = TransportOLD.offline[S, I](group, sampleRate)
        val _aural = AuralSystem.offline(server)
        val _transport = Transport(_aural, _scheduler)
        config.group.foreach { h =>
          _transport.addObject(h())
        }
        _transport.seek(_span.start)
        _transport.play()
        (_span, _scheduler, _transport, _aural)
      }
      aural = __aural

      val srRatio = server.sampleRate / Timeline.SampleRate

      // Tricky business: While handling prepared state is not yet
      // fully solved, especially with collection objects such as
      // Timeline, we at least provide some bounce support for objects
      // that require asynchronous preparation. To do that, we gather
      // all views with state `Preparing` and wait for them to go into
      // either `Playing` or `Stopped`. We go deeply into timelines as
      // well. Finally, we bundle all these futures together and wait
      // for their completion. Then we should be fine advancing the
      // logical clock.
      //
      // This does not work with objects on a timeline that do not
      // overlap with the transport's starting position!
      val prepFutures = cursor.step { implicit tx =>
        // println(s"States = ${transport.views.map(_.state)}")

        def gather(views: Set[AuralObj[S]]): Set[Future[Unit]] = {
          //          views.foreach { obj =>
          //            if (obj.state != AuralObj.Preparing) println(s"- - - - $obj: ${obj.state}")
          //          }
          val set1 = views.collect {
            case obj if obj.state == AuralObj.Preparing =>
              val p = Promise[Unit]()
              obj.react { implicit tx => {
                case AuralObj.Playing | AuralObj.Stopped =>
                  tx.afterCommit(p.tryComplete(Success(())))
                case _ =>
              }}
              p.future
          }
          val set2 = views.flatMap {
            case atl: AuralObj.Timeline[S] =>
              val children = atl.views
              // println(s"For timeline: $children")
              gather(children)
            case _ => Set.empty[Future[Unit]]
          }

          set1 ++ set2
        }

        gather(transport.views)
      }
      if (prepFutures.nonEmpty) {
        logTransport(s"waiting for ${prepFutures.size} preparations to complete...")
        Await.result(Future.sequence(prepFutures), Duration.Inf)
        logTransport("...preparations completed")
      }

      @tailrec def loop(): Unit = {
        Await.result(server.committed(), Duration.Inf)
        val keepPlaying = blocking {
          cursor.step { implicit tx =>
            scheduler.stepTarget match {
              case Some(pos) if pos <= span.length =>
                logTransport(s"stepTarget = $pos")
                server.position = (pos * srRatio + 0.5).toLong
                scheduler.step()
                true

              case _ =>
                if (transport.position < span.stop) {
                  server.position = (span.length * srRatio + 0.5).toLong
                  server !! osc.Bundle.now() // dummy bundle to terminate the OSC file at the right position
                }
                false
            }
          }
        }
        if (keepPlaying) loop()
      }

      loop()
      Await.result(server.committed(), Duration.Inf)
      val bundles = server.bundles()

      if (showTransportLog) {
        logTransport("---- BOUNCE: bundles ----")
        bundles.foreach(b => logTransport(b.toString()))
      }

      // ---- write OSC file ----

      oscFile = new File(sCfg.nrtCommandPath)
      if (oscFile.exists()) require(oscFile.delete(), s"Could not delete existing OSC file $oscFile")

      // XXX TODO: this should be factored out, probably go into ScalaOSC or ScalaCollider
      blocking {
        val c   = Server.codec
        val sz  = 8192 // bundles.map(_.encodedSize(c)).max
        // logTransport(s"Max bundle size is $sz")
        val raf = new RandomAccessFile(oscFile, "rw")
        try {
          val bb = ByteBuffer.allocate(sz)
          val fch = raf.getChannel
          bundles.foreach { bndl =>
            bndl.encode(c, bb)
            bb.flip()
            raf.writeInt(bb.limit)
            fch.write(bb)
            bb.clear()
          }
        } finally {
          raf.close()
        }
      }

      // ---- run scsynth ----

      val dur = span.length / Timeline.SampleRate

      logTransport("---- BOUNCE: scsynth ----")

      val nrtFut = SServer.renderNRT(dur = dur, config = sCfg)
      nrtFut.start()
      val nrtRes = await(nrtFut)
      if (nrtRes != 0) throw Bounce.ServerFailed(nrtRes)

      cursor.step { implicit tx =>
        transport.dispose()
      }
      // scheduler.dispose()

      if (needsOutFile) outFile else new File(config.server.nrtOutputPath)
    }
  }
}