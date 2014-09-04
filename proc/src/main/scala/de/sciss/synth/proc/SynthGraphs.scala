/*
 *  SynthGraphs.scala
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

import de.sciss.lucre.{event => evt, expr}
import evt.{Targets, Sys}
import scala.annotation.switch
import de.sciss.model
import de.sciss.synth.{Lazy, MaybeRate, SynthGraph}
import de.sciss.serial.{ImmutableSerializer, DataOutput, DataInput}
import de.sciss.lucre.expr.Expr
import de.sciss.synth.ugen.{ControlProxyLike, Constant}
import java.util

import scala.util.control.NonFatal

object SynthGraphs extends expr.impl.ExprTypeImpl[SynthGraph] {
  final val typeID = 16

/** A serializer for synth graphs. */
object ValueSerializer extends ImmutableSerializer[SynthGraph] {
  private final val SER_VERSION = 0x5347

  // private final class RefMapOut {
  // var map   = Map.empty[Product, Int]
  // val map   = collection.mutable.Map.empty[Product, Int] // not faster than immutable map!
  // var count = 0
  // }

    // we use an identity hash map, because we do _not_
    // want to alias objects in the serialization; the input
    // is an in-memory object graph.
  private type RefMapOut = util.IdentityHashMap[Product, Integer]

  private final class RefMapIn {
    var map   = Map.empty[Int, Product]
    // val map   = collection.mutable.Map.empty[Int, Product]
    var count = 0
  }

  private def writeProduct(p: Product, out: DataOutput, ref: RefMapOut): Unit = {
    val id0Ref = ref.get(p)
    // val id0 = ref.map.getOrElse(p, -1)
    if (id0Ref != null) {
      out.writeByte('<')
      out.writeInt(id0Ref)
      return
    }
    out.writeByte('P')
    val pck     = p.getClass.getPackage.getName
    val prefix  = p.productPrefix
    val name    = if (pck == "de.sciss.synth.ugen") prefix else s"$pck.$prefix"
    out.writeUTF(name)
    out.writeShort(p.productArity)
    p.productIterator.foreach(writeElem(_, out, ref))

    val id     = ref.size() // count
    // ref.map   += ((p, id))
    // ref.count  = id + 1
    ref.put(p, id)
  }

  private def writeElemSeq(xs: Seq[Any], out: DataOutput, ref: RefMapOut): Unit = {
    out.writeByte('X')
    out.writeInt(xs.size)
    xs.foreach(writeElem(_, out, ref))
  }

  private def writeElem(e: Any, out: DataOutput, ref: RefMapOut): Unit =
    e match {
      case c: Constant =>
        out.writeByte('C')
        out.writeFloat(c.value)
      case r: MaybeRate =>
        out.writeByte('R')
        out.writeByte(r.id)
      case o: Option[_] =>
        out.writeByte('O')
        out.writeBoolean(o.isDefined)
        if (o.isDefined) writeElem(o.get, out, ref)
      case xs: Seq[_] =>  // 'X'. either indexed seq or var arg (e.g. wrapped array)
        writeElemSeq(xs, out, ref)
      case p: Product =>
        writeProduct(p, out, ref) // 'P' or '<'
      case i: Int =>
        out.writeByte('I')
        out.writeInt(i)
      case s: String =>
        out.writeByte('S')
        out.writeUTF(s)
      case b: Boolean   =>
        out.writeByte('B')
        out.writeBoolean(b)
      case f: Float =>
        out.writeByte('F')
        out.writeFloat(f)
      case d: Double =>
        out.writeByte('D')
        out.writeDouble(d)
    }

  def write(v: SynthGraph, out: DataOutput): Unit = {
    out.writeShort(SER_VERSION)
    writeNew(v, out)
  }

  private def writeNew(v: SynthGraph, out: DataOutput): Unit = {
    val ref = new RefMapOut
    writeElemSeq(v.sources, out, ref)
    val ctl = v.controlProxies
    out.writeByte('T')
    out.writeInt(ctl.size)
    ctl.foreach(writeProduct(_, out, ref))
  }

  // expects that 'X' byte has already been read
  private def readIdentifiedSeq(in: DataInput, ref: RefMapIn): Seq[Any] = {
    val num = in.readInt()
    Vector.fill(num)(readElem(in, ref))
  }

  // expects that 'P' byte has already been read
  private def readIdentifiedProduct(in: DataInput, ref: RefMapIn): Product = {
    val prefix    = in.readUTF()
    val arity     = in.readShort()
    val className = if (Character.isUpperCase(prefix.charAt(0))) s"de.sciss.synth.ugen.$prefix" else prefix

    val res = try {
      if (arity == 0 && className.charAt(className.length - 1) == '$') {
        // case object
        val companion = Class.forName(s"$className").getField("MODULE$").get(null)
        companion.asInstanceOf[Product]

      } else {

        // cf. stackoverflow #3039822
        val companion = Class.forName(s"$className$$").getField("MODULE$").get(null)
        val elems = new Array[AnyRef](arity)
        var i = 0
        while (i < arity) {
          elems(i) = readElem(in, ref).asInstanceOf[AnyRef]
          i += 1
        }
        //    val m         = companion.getClass.getMethods.find(_.getName == "apply")
        //      .getOrElse(sys.error(s"No apply method found on $companion"))
        val ms = companion.getClass.getMethods
        var m = null: java.lang.reflect.Method
        var j = 0
        while (m == null && j < ms.length) {
          val mj = ms(j)
          if (mj.getName == "apply" && mj.getParameterTypes.length == arity) m = mj
          j += 1
        }
        if (m == null) sys.error(s"No apply method found on $companion")

        m.invoke(companion, elems: _*).asInstanceOf[Product]
      }

    } catch {
      case NonFatal(e) =>
        throw new IllegalArgumentException(s"While de-serializing $prefix: ${e.getMessage}")
    }

    val id        = ref.count
    ref.map      += ((id, res))
    ref.count     = id + 1
    res
  }

  private def readElem(in: DataInput, ref: RefMapIn): Any = {
    (in.readByte(): @switch) match {
      case 'C' => Constant(in.readFloat())
      case 'R' => MaybeRate(in.readByte())
      case 'O' => if (in.readBoolean()) Some(readElem(in, ref)) else None
      case 'X' => readIdentifiedSeq(in, ref)
      case 'P' => readIdentifiedProduct(in, ref)
      case '<' =>
        val id = in.readInt()
        ref.map(id)
      case 'I' => in.readInt()
      case 'S' => in.readUTF()
      case 'B' => in.readBoolean()
      case 'F' => in.readFloat()
      case 'D' => in.readDouble()
    }
  }

  def read(in: DataInput): SynthGraph = {
    val cookie  = in.readShort()
    require(cookie == SER_VERSION, s"Unexpected cookie $cookie")
    val res2  = readNew(in)
    res2
  }

  private def readNew(in: DataInput): SynthGraph = {
    val ref     = new RefMapIn
    val b1 = in.readByte()
    require(b1 == 'X')    // expecting sequence
    val numSources  = in.readInt()
    val sources     = Vector.fill(numSources) {
      readElem(in, ref).asInstanceOf[Lazy]
    }
    val b2 = in.readByte()
    require(b2 == 'T')    // expecting set
    val numControls = in.readInt()
    val controls    = Set.newBuilder[ControlProxyLike] // stupid Set doesn't have `fill` and `tabulate` methods
    var i = 0
    while (i < numControls) {
      controls += readElem(in, ref).asInstanceOf[ControlProxyLike]
      i += 1
    }
    SynthGraph(sources, controls.result())
  }

  //  private def readOld(in: DataInput): SynthGraph = {
  //    val ois = new java.io.ObjectInputStream(in.asInputStream)
  //    val res = ois.readObject().asInstanceOf[SynthGraph]
  //    ois.close()
  //    res
  //  }
}
  //  private val map = mutable.Map.empty[String, SynthGraph]
  //
  //  /** Adds a predefined synth graph which is serialized through a key.
  //    * Care must be taken so that the key is unique and that the graph
  //    * is registered before any possible deserialization (which would
  //    * fail if no in-memory graph is found under the key)
  //    */
  //  def add(key: String, graph: SynthGraph): Unit =
  //    map.synchronized(map += (key, graph))

  // private final val oldTapeCookie = 1
  private final val emptyCookie   = 2
  private final val tapeCookie    = 3

  def readValue (                   in : DataInput ): SynthGraph  = ValueSerializer.read (       in )
  def writeValue(value: SynthGraph, out: DataOutput): Unit        = ValueSerializer.write(value, out)

  lazy val install: Unit = ()

  // XXX TODO: not cool. Should use `1` to `3` for cookies
  override protected def readNode[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                      (implicit tx: S#Tx): Ex[S] with evt.Node[S] =
    (cookie: @switch) match {
      case /* `oldTapeCookie` | */ `emptyCookie` | `tapeCookie` => new Predefined(targets, cookie)

      //      case `mapCookie`  =>
      //        val key     = in.readUTF()
      //        val graph   = map.synchronized(map(key))
      //        new MapImpl(key, graph)
    }

  // private final class MapImpl[S <: Sys[S]]

  //  private lazy val oldTapeSynthGraph: SynthGraph =
  //    SynthGraph {
  //      import de.sciss.synth._
  //      import ugen._
  //      val sig   = graph.scan.In(Proc.Obj.graphAudio)
  //      val bus   = graph.attribute(ObjKeys.attrBus   ).ir(0)
  //      val mute  = graph.attribute(ObjKeys.attrMute  ).ir(0)
  //      val env   = graph.FadeInOut(ObjKeys.attrFadeIn, ObjKeys.attrFadeOut).ar
  //      val amp   = env * (1 - mute)
  //      Out.ar(bus, sig * amp)
  //    }

  private lazy val tapeSynthGraph: SynthGraph =
    SynthGraph {
      import de.sciss.synth._
      val sig   = graph.ScanIn(Proc.Obj.graphAudio)
      val gain  = graph.Attribute.kr(ObjKeys.attrGain, 1)
      val mute  = graph.Attribute.kr(ObjKeys.attrMute, 0)
      val env   = graph.FadeInOut.ar
      val amp   = env * ((1 - mute) * gain)
      graph.ScanOut(Proc.Obj.scanMainOut, sig * amp)
    }

  private val emptySynthGraph = SynthGraph {}

  def tape   [S <: Sys[S]](implicit tx: S#Tx): Ex[S] = apply(tapeCookie   )
  // def tapeOld[S <: Sys[S]](implicit tx: S#Tx): Ex[S] = apply(oldTapeCookie)
  def empty  [S <: Sys[S]](implicit tx: S#Tx): Ex[S] = apply(emptyCookie  )

  private def apply[S <: Sys[S]](cookie: Int)(implicit tx: S#Tx): Ex[S] = {
    val targets = evt.Targets[S]
    new Predefined(targets, cookie)
  }

  // XXX TODO -- we should allow other constant values in Type. now we have a wasted evt.Targets...
  private final class Predefined[S <: Sys[S]](protected val targets: Targets[S], cookie: Int)
    extends Expr[S, SynthGraph]
    with evt.Node[S]
    with evt.impl.SingleGenerator[S, model.Change[SynthGraph], Ex[S]] {

    protected def writeData(out: DataOutput): Unit = out.writeByte(cookie)

    protected def disposeData()(implicit tx: S#Tx) = ()

    protected def reader: evt.Reader[S, SynthGraphs.Ex[S]] = serializer

    def value(implicit tx: S#Tx): SynthGraph = (cookie: @switch) match {
      // case `oldTapeCookie`  => oldTapeSynthGraph
      case `emptyCookie`    => emptySynthGraph
      case `tapeCookie`     => tapeSynthGraph
    }
  }
}
// sealed trait SynthGraphSource[S <: Sys[S]] extends Expr[S, SynthGraph]