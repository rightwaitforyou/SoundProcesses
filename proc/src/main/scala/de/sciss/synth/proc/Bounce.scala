package de.sciss.synth.proc

import de.sciss.span.{SpanLike, Span}
import language.implicitConversions
import de.sciss.processor.{Processor, ProcessorFactory}
import java.io.{RandomAccessFile, File}
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.synth.io.{SampleFormat, AudioFileType}
import de.sciss.lucre.stm
import scala.concurrent.{Await, blocking}
import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import de.sciss.osc
import java.nio.ByteBuffer
import scala.sys.process.{Process, ProcessLogger}
import de.sciss.lucre.synth.{Sys, Server}

object Bounce {
  var DEBUG = false

  def apply[S <: Sys[S], I <: stm.Sys[I]](implicit cursor: stm.Cursor[S], bridge: S#Tx => I#Tx): Bounce[S, I] =
    new Bounce[S, I]
}
final class Bounce[S <: Sys[S], I <: stm.Sys[I]] private (implicit cursor: stm.Cursor[S], bridge: S#Tx => I#Tx)
  extends ProcessorFactory {

  import Bounce.DEBUG

  type Product = File

  private type GroupH = stm.Source[S#Tx, ProcGroup[S]]

  sealed trait ConfigLike {
    /** The group to transport through the bounce.
      * This parameter is initially unspecified in the builder, and calling the getter will throw an error.
      * This parameter must be specified before generating a `Config` instance.
      */
    def group: GroupH

    /** The span of the timeline to bounce. This is either a given interval,
      * or a one-sided open interval (`Span.From`), in which case the stopping point is determined by
      * the processes found in the group, or `Span.Void`, in which case both the starting and stopping
      * points are determined by the processes found in the group. */
    def span: SpanLike

    /** Configuration of the offline server.
      * It is crucial to specify the NRT bits, i.e.
      * `nrtInputPath` (if used), `nrtOutputPath`,
      * `nrtHeaderFormat` (defaults to AIFF), `nrtSampleFormat` (defaults to Float32),
      * as well as the number of input and output channels
      * `inputBusChannels` and `outputBusChannels`, and finally
      * the sampling rate `sampleRate`.
      *
      * Typically you will not specify the `nrtCommandPath` (NRT OSC file). For debugging purposes this
      * may be set, otherwise a temporary file is automatically generated.
      */
    def server: Server.ConfigLike

    /** An arbitrary function may be provided which is called when the server is initialized (logical time zero).
      * This entry is typically used to set up extra routing synths, master volume, etc.
      */
    def init: (S#Tx, Server) => Unit
  }
  object Config {
    def apply(): ConfigBuilder = new ConfigBuilder

    implicit def build(b: ConfigBuilder): Config = b.build
  }
  sealed trait Config extends ConfigLike {
    def server: Server.Config
  }
  final class ConfigBuilder private[Bounce] () extends ConfigLike {
    private var _group: GroupH = null
    def group: GroupH = {
      if (_group == null) throw new IllegalStateException("A group has not yet been assigned")
      _group
    }
    def group_=(value: GroupH): Unit = _group = value

    var span  : SpanLike                = Span.Void
    val server: Server.ConfigBuilder    = Server.Config()
    var init  : (S#Tx, Server) => Unit  = (_, _) => ()

    // some sensible defaults
    server.blockSize          = 1
    server.inputBusChannels   = 0
    server.outputBusChannels  = 1

    def build: Config = ConfigImpl(group = group, span = span, server = server, init = init)
  }

  private final case class ConfigImpl(group: GroupH, span: SpanLike,
                                      server: Server.Config, init: (S#Tx, Server) => Unit)
    extends Config {

    override def productPrefix = "Config"
  }
  
  type Repr = Processor[Product, _]

  protected def prepare(config: Config): Prepared = {
    require(config.server.sampleRate > 0, "The sample-rate of the server configuration must be explicitly specified")
    new Impl(config)
  }

  private final class Impl(config: Config) extends ProcessorImpl[Product, Repr] {
    // XXX TODO: due to a bug in Processor, this is currently not called:
    // override protected def cleanUp() {
    // }

    protected def body(): File = {
      val needsOSCFile  = config.server.nrtCommandPath    == "" // we need to generate that file
      val needsDummyOut = config.server.outputBusChannels == 0  // scsynth doesn't allow this. must have 1 dummy channel
      val needsOutFile  = config.server.nrtOutputPath     == ""  && !needsDummyOut // we need to generate
      val sampleRate    = config.server.sampleRate.toDouble

      // ---- configuration ----

      // the server config (either directly from input, or updated according to the necessary changes)
      val sCfg  = if (needsOSCFile || needsDummyOut || needsOutFile) {
        val b = Server.ConfigBuilder(config.server)
        if (needsOSCFile) {
          val f               = File.createTempFile("bounce", ".osc")
          b.nrtCommandPath    = f.getCanonicalPath
        }
        if (needsDummyOut) {
          b.nrtHeaderFormat   = AudioFileType.AIFF
          b.nrtSampleFormat   = SampleFormat.Int16
          b.outputBusChannels = 1
        }
        if (needsDummyOut || needsOutFile) {
          val f               = File.createTempFile("bounce", "." + b.nrtHeaderFormat.extension)
          b.nrtOutputPath     = f.getCanonicalPath
        }
        b.build
      } else {
        config.server
      }

      // ---- run transport and gather OSC ----

      val (span, transp) = blocking {
        cursor.step { implicit tx =>
          val group   = config.group()

          val _span   = config.span match {
            case defined: Span    => defined
            case Span.From(start) =>
              val stop  = group.nearestEventBefore(Long.MaxValue).getOrElse(0L)
              Span(start, stop)
            case Span.Until(stop) =>
              val start = group.nearestEventAfter (Long.MinValue).getOrElse(0L)
              Span(start, stop)
            case _ =>
              val start = group.nearestEventAfter (Long.MinValue).getOrElse(0L)
              val stop  = group.nearestEventBefore(Long.MaxValue).getOrElse(0L)
              Span(start, stop)
          }

          val _transp = Transport.offline[S, I](group, sampleRate)
          (_span, _transp)
        }
      }

      val server  = Server.offline(sCfg)
      val aural   = AuralSystem.offline(server)

      val view = blocking {
        cursor.step { implicit tx =>
          val _view   = AuralPresentation.runTx[S](transp, aural)
          config.init(tx, server)
          transp.seek(span.start)
          transp.play()
          _view
        }
      }

      @tailrec def loop() {
        Await.result(server.committed(), Duration.Inf)
        val keepPlaying = blocking {
          cursor.step { implicit tx =>
            transp.stepTarget match {
              case _posO @ Some(pos) if (pos <= span.stop) =>
                logTransport(s"stepTarget = $pos")
                server.position = pos - span.start
                transp.step()
                true

              case _ =>
                if (transp.position < span.stop) {
                  server.position = span.length
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

      if (DEBUG) {
        println("---- BOUNCE: bundles ----")
        bundles.foreach(println)
      }

      // ---- write OSC file ----

      val oscFile = new File(sCfg.nrtCommandPath)
      if (oscFile.exists()) require(oscFile.delete(), s"Could not delete existing OSC file $oscFile")

      // XXX TODO: this should be factored out, probably go into ScalaOSC or ScalaCollider
      blocking {
        val c   = osc.PacketCodec().scsynth().build
        val sz  = bundles.map(_.encodedSize(c)).max
        val raf = new RandomAccessFile(oscFile, "rw")
        try {
          val bb  = ByteBuffer.allocate(sz)
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

      val dur = span.length / sampleRate

      val procArgs    = sCfg.toNonRealtimeArgs
      val procBuilder = Process(procArgs, Some(new File(sCfg.programPath).getParentFile))

      if (DEBUG) {
        println("---- BOUNCE: scsynth ----")
        println(procArgs.mkString(" "))
      }

      lazy val log: ProcessLogger = new ProcessLogger {
        def buffer[T](f: => T): T = f

        // ??
        def out(line: => String): Unit =
          if (line.startsWith("nextOSCPacket")) {
            val time = line.substring(14).toFloat
            val prog = time / dur
            progress(prog.toFloat)
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
      if (res != 0) throw new RuntimeException("scsynth failed with exit code " + res)

      // XXX TODO: clean up

      cursor.step(implicit tx => view.dispose())

      outputFile
    }
  }
}