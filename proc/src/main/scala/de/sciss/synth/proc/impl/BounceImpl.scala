/*
 *  BounceImpl.scala
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

import java.io.{RandomAccessFile, File}
import java.nio.ByteBuffer

import de.sciss.lucre.stm
import de.sciss.lucre.synth.{Sys, Server}
import de.sciss.osc
import de.sciss.processor.{GenericProcessor, Processor}
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.synth.io.{SampleFormat, AudioFileType}

import scala.annotation.tailrec
import scala.concurrent.{Await, blocking}
import scala.concurrent.duration.Duration
import scala.sys.process.{Process, ProcessLogger}

final class BounceImpl[S <: Sys[S], I <: stm.Sys[I]](implicit cursor: stm.Cursor[S], bridge: S#Tx => I#Tx)
  extends Bounce[S] {

  protected def prepare(config: Config): Prepared = {
    require(config.server.sampleRate > 0, "The sample-rate of the server configuration must be explicitly specified")
    new Impl(config)
  }

  private final class Impl(config: Config) extends ProcessorImpl[Product, GenericProcessor[File]]
    with GenericProcessor[File] {

    // XXX TODO: due to a bug in Processor, this is currently not called:
    // - note: should be fixed in current version

    // override protected def cleanUp() {
    // }

    protected def body(): File = {
      val needsOSCFile  = config.server.nrtCommandPath.isEmpty  // we need to generate that file
      val needsDummyOut = config.server.outputBusChannels == 0  // scsynth doesn't allow this. must have 1 dummy channel
      val needsOutFile  = config.server.nrtOutputPath.isEmpty && !needsDummyOut // we need to generate

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
          val f = File.createTempFile("bounce", s".${b.nrtHeaderFormat.extension}")
          b.nrtOutputPath = f.getCanonicalPath
        }
        b.build
      } else {
        config.server
      }

      // ---- run transport and gather OSC ----

      val server = Server.offline(sCfg)

      val (span, scheduler, transport) = cursor.step { implicit tx =>
        val _scheduler  = Scheduler.offline[S]
        val _span       = config.span

        // val _transp = TransportOLD.offline[S, I](group, sampleRate)
        val aural = AuralSystem.offline(server)
        val _transport = Transport(aural, _scheduler)
        config.group.foreach { h =>
          _transport.addObject(h())
        }
        _transport.seek(_span.start)
        _transport.play()
        (_span, _scheduler, _transport)
      }

      val srRatio = server.sampleRate / Timeline.SampleRate

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

      val oscFile = new File(sCfg.nrtCommandPath)
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

      val procArgs = sCfg.toNonRealtimeArgs
      val procBuilder = Process(procArgs, Some(new File(sCfg.program).getParentFile))

      logTransport("---- BOUNCE: scsynth ----")
      logTransport(procArgs.mkString(" "))

      lazy val log: ProcessLogger = new ProcessLogger {
        def buffer[A](f: => A): A = f

        // ??
        def out(line: => String): Unit =
          if (line.startsWith("nextOSCPacket")) {
            val time = line.substring(14).toFloat
            val prog = time / dur
            progress = prog
            try {
              checkAborted()
            } catch {
              case Processor.Aborted() =>
                proc.destroy()
            }
          } else if (line != "start time 0") {
            Console.out.println(line)
          }

        def err(line: => String): Unit = Console.err.println(line)
      }
      lazy val proc: Process = procBuilder.run(log)

      val res = blocking(proc.exitValue()) // blocks
      if (needsOSCFile) oscFile.delete()
      val outputFile = new File(sCfg.nrtOutputPath)
      if (needsDummyOut) outputFile.delete()
      checkAborted()
      if (res != 0) throw new RuntimeException(s"scsynth failed with exit code $res")

      // XXX TODO: clean up

      cursor.step { implicit tx =>
        transport.dispose()
      }
      // scheduler.dispose()

      outputFile
    }
  }
}