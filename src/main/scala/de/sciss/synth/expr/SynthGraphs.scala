package de.sciss
package synth
package expr

import serial.{DataOutput, DataInput}
import synth.proc.{ProcKeys, graph, impl}
import lucre.{event => evt}
import evt.{Targets, Sys}
import lucre.expr.Expr
import scala.annotation.switch

object SynthGraphs extends BiTypeImpl[SynthGraph] {
  final val typeID = 16

  //  private val map = mutable.Map.empty[String, SynthGraph]
  //
  //  /** Adds a predefined synth graph which is serialized through a key.
  //    * Care must be taken so that the key is unique and that the graph
  //    * is registered before any possible deserialization (which would
  //    * fail if no in-memory graph is found under the key)
  //    */
  //  def add(key: String, graph: SynthGraph): Unit =
  //    map.synchronized(map += (key, graph))

  private final val oldTapeCookie = 1
  private final val emptyCookie   = 2
  private final val tapeCookie    = 3

  def readValue(in: DataInput): SynthGraph = impl.SynthGraphSerializer.read(in)

  def writeValue(value: SynthGraph, out: DataOutput): Unit =
    impl.SynthGraphSerializer.write(value, out)

  protected def readTuple[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                      (implicit tx: S#Tx): ReprNode[S] =
    (cookie: @switch) match {
      case `oldTapeCookie` | `emptyCookie` | `tapeCookie` => new Predefined(targets, cookie)

      //      case `mapCookie`  =>
      //        val key     = in.readUTF()
      //        val graph   = map.synchronized(map(key))
      //        new MapImpl(key, graph)
    }

  // private final class MapImpl[S <: Sys[S]]

  private lazy val oldTapeSynthGraph = SynthGraph {
    import synth._
    import ugen._
    val sig   = graph.scan     (ProcKeys.graphAudio).ar(0)
    val bus   = graph.attribute(ProcKeys.attrBus   ).ir(0)
    val mute  = graph.attribute(ProcKeys.attrMute  ).ir(0)
    val env   = graph.FadeInOut(ProcKeys.attrFadeIn, ProcKeys.attrFadeOut).ar
    val amp   = env * (1 - mute)
    Out.ar(bus, sig * amp)
  }

  private lazy val tapeSynthGraph = SynthGraph {
    import synth._
    val sig   = graph.scan     (ProcKeys.graphAudio).ar(0)
    val mute  = graph.attribute(ProcKeys.attrMute  ).ir(0)
    val env   = graph.FadeInOut(ProcKeys.attrFadeIn, ProcKeys.attrFadeOut).ar
    val amp   = env * (1 - mute)
    graph.scan(ProcKeys.scanMainOut) := sig * amp
  }

  private val emptySynthGraph = SynthGraph {}

  def tape   [S <: Sys[S]](implicit tx: S#Tx): Ex[S] = apply(tapeCookie   )
  def tapeOld[S <: Sys[S]](implicit tx: S#Tx): Ex[S] = apply(oldTapeCookie)
  def empty  [S <: Sys[S]](implicit tx: S#Tx): Ex[S] = apply(emptyCookie  )

  private def apply[S <: Sys[S]](cookie: Int)(implicit tx: S#Tx): Ex[S] = {
    val targets = evt.Targets[S]
    new Predefined(targets, cookie)
  }

  // XXX TODO -- we should allow other constant values in Type. now we have a wasted evt.Targets...
  private final class Predefined[S <: Sys[S]](protected val targets: Targets[S], cookie: Int)
    extends Expr[S, SynthGraph]
    with evt.Node[S]
    with evt.impl.SingleGenerator[S, evt.Change[SynthGraph], Ex[S]] {

    protected def writeData(out: DataOutput): Unit = out.writeByte(cookie)

    protected def disposeData()(implicit tx: S#Tx) = ()

    protected def reader: evt.Reader[S, SynthGraphs.Ex[S]] = serializer

    def value(implicit tx: S#Tx): SynthGraph = (cookie: @switch) match {
      case `oldTapeCookie`  => oldTapeSynthGraph
      case `emptyCookie`    => emptySynthGraph
      case `tapeCookie`     => tapeSynthGraph
    }
  }
}
// sealed trait SynthGraphSource[S <: Sys[S]] extends Expr[S, SynthGraph]