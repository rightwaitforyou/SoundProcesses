package de.sciss
package synth
package expr

import serial.{DataOutput, DataInput}
import synth.proc.{ProcKeys, graph, impl}
import lucre.{event => evt}
import evt.{Targets, Sys}
import lucre.expr.Expr

object SynthGraphs extends BiTypeImpl[SynthGraph] {
  final val typeID = 16

  //  private val map = mutable.Map.empty[String, SynthGraph]
  //
  //  /** Adds a predefined synth graph which is serialized through a key.
  //    * Care must be taken so that the key is unique and that the graph
  //    * is registered before any possible deserialization (which would
  //    * fail if no in-memory graph is found under the key)
  //    */
  //  def add(key: String, graph: SynthGraph) {
  //    map.synchronized(map += (key, graph))
  //  }

  private final val tapeCookie  = 1
  // private final val mapCookie   = 2

  def readValue(in: DataInput): SynthGraph = impl.SynthGraphSerializer.read(in)

  def writeValue(value: SynthGraph, out: DataOutput) {
    impl.SynthGraphSerializer.write(value, out)
  }

  protected def readTuple[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                      (implicit tx: S#Tx): ReprNode[S] =
    (cookie /* : @switch */) match {
      case `tapeCookie` =>
        new TapeImpl(targets)

      //      case `mapCookie`  =>
      //        val key     = in.readUTF()
      //        val graph   = map.synchronized(map(key))
      //        new MapImpl(key, graph)
    }

  // private final class MapImpl[S <: Sys[S]]

  private val tapeSynthGraph = SynthGraph {
    import synth._
    import ugen._
    val sig   = graph.scan     (ProcKeys.graphAudio).ar(0)
    val bus   = graph.attribute(ProcKeys.attrBus   ).ir(0)
    val mute  = graph.attribute(ProcKeys.attrMute  ).ir(0)
    val env   = graph.FadeInOut(ProcKeys.attrFadeIn, ProcKeys.attrFadeOut).ar
    val amp   = env * (1 - mute)
    Out.ar(bus, sig * amp)
  }

  def tape[S <: Sys[S]](implicit tx: S#Tx): Ex[S] = {
    val targets = evt.Targets[S]
    new TapeImpl(targets)
  }

  // XXX TODO -- we should allow other constant values in Type. now we have a wasted evt.Targets...
  private final class TapeImpl[S <: Sys[S]](protected val targets: Targets[S])
    extends Expr[S, SynthGraph]
    with evt.Node[S]
    with evt.impl.SingleGenerator[S, evt.Change[SynthGraph], Ex[S]] {

    protected def writeData(out: DataOutput) {}
    protected def disposeData()(implicit tx: S#Tx) {}

    protected def reader: evt.Reader[S, SynthGraphs.Ex[S]] = serializer

    def value(implicit tx: S#Tx): SynthGraph = tapeSynthGraph
  }
}
// sealed trait SynthGraphSource[S <: Sys[S]] extends Expr[S, SynthGraph]