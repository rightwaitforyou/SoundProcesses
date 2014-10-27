/*
 *  Bounce.scala
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

import java.io.File

import de.sciss.span.Span
import language.implicitConversions
import de.sciss.processor.{GenericProcessor, Processor, ProcessorFactory}
import de.sciss.lucre.stm
import de.sciss.lucre.synth.{Sys, Server}
import scala.collection.immutable.{Iterable => IIterable}
import impl.{BounceImpl => Impl}

object Bounce {
  def apply[S <: Sys[S], I <: stm.Sys[I]](implicit cursor: stm.Cursor[S], bridge: S#Tx => I#Tx,
                                          workspace: WorkspaceHandle[S]): Bounce[S] =
    new Impl[S, I]

  private type GroupH[S <: Sys[S]] = IIterable[stm.Source[S#Tx, Obj[S]]]

  sealed trait ConfigLike[S <: Sys[S]] {
    /** The group to transport through the bounce.
      * This parameter is initially unspecified in the builder, and calling the getter will throw an error.
      * This parameter must be specified before generating a `Config` instance.
      */
    def group: GroupH[S]

    /** The span of the timeline to bounce. */
    def span: Span

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
    def apply[S <: Sys[S]]: ConfigBuilder[S] = new ConfigBuilder

    implicit def build[S <: Sys[S]](b: ConfigBuilder[S]): Config[S] = b.build
  }
  sealed trait Config[S <: Sys[S]] extends ConfigLike[S] {
    def server: Server.Config
  }
  final class ConfigBuilder[S <: Sys[S]] private[Bounce] () extends ConfigLike[S] {
    private var _group: GroupH[S] = null
    def group: GroupH[S] = {
      if (_group == null) throw new IllegalStateException("A group has not yet been assigned")
      _group
    }
    def group_=(value: GroupH[S]): Unit = _group = value

    var span  : Span                    = Span(0L, Timeline.SampleRate.toLong)
    val server: Server.ConfigBuilder    = Server.Config()
    var init  : (S#Tx, Server) => Unit  = (_, _) => ()

    // some sensible defaults
    server.blockSize          = 1
    server.inputBusChannels   = 0
    server.outputBusChannels  = 1

    def build: Config[S] = ConfigImpl(group = group, span = span, server = server, init = init)
  }

  private final case class ConfigImpl[S <: Sys[S]](group: GroupH[S], span: Span,
                                      server: Server.Config, init: (S#Tx, Server) => Unit)
    extends Config[S] {

    override def productPrefix = "Config"
  }
}
trait Bounce[S <: Sys[S]] extends ProcessorFactory {
  type Product  = File
  type Config   = Bounce.Config[S]
  type Repr     = GenericProcessor[Product]
}