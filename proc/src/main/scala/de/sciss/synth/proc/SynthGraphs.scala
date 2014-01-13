/*
 *  SynthGraphs.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.synth.proc

import de.sciss.lucre.{event => evt}
import evt.{Targets, Sys}
import scala.annotation.switch
import de.sciss.model
import de.sciss.synth.{Lazy, MaybeRate, SynthGraph}
import de.sciss.serial.{ImmutableSerializer, DataOutput, DataInput}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.synth.expr.BiTypeImpl
import de.sciss.synth.ugen.{ControlProxyLike, Constant}

object SynthGraphs extends BiTypeImpl[SynthGraph] {
  final val typeID = 16

/** A serializer for synth graphs. */
object ValueSerializer extends ImmutableSerializer[SynthGraph] {
  private final val SER_VERSION = 0x5347

  private final class RefMapOut {
    var map   = Map.empty[Product, Int]
    // val map   = collection.mutable.Map.empty[Product, Int] // not faster than immutable map!
    var count = 0
  }

  private final class RefMapIn {
    var map   = Map.empty[Int, Product]
    // val map   = collection.mutable.Map.empty[Int, Product]
    var count = 0
  }

  private def writeProduct(p: Product, out: DataOutput, ref: RefMapOut): Unit = {
    val id0 = ref.map.getOrElse(p, -1)
    if (id0 >= 0) {
      out.writeByte('<')
      out.writeInt(id0)
      return
    }
    out.writeByte('P')
    val pck     = p.getClass.getPackage.getName
    val prefix  = p.productPrefix
    val name    = if (pck == "de.sciss.synth.ugen") prefix else pck + "." + prefix
    out.writeUTF(name)
    out.writeShort(p.productArity)
    p.productIterator.foreach(writeElem(_, out, ref))

    val id     = ref.count
    ref.map   += ((p, id))
    ref.count  = id + 1
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
    //    val t1    = System.nanoTime()
    //    val p1    = out.position
    //    writeOld(v, out)
    //    val t2    = System.nanoTime()
    //    val p2    = out.position
    writeNew(v, out)
    //    val t3    = System.nanoTime()
    //    val p3    = out.position
    //    val timOld    = t2 - t1
    //    val timNew    = t3 - t2
    //    val spcOld    = p2 - p1
    //    val spcNew    = p3 - p2
    //    val timeRel   = timNew.toDouble/timOld.toDouble*100
    //    val spaceRel  = spcNew.toDouble/spcOld.toDouble*100
    //    println(f"<<< writ >>> time ${timOld/1000}%5d vs. ${timNew/1000}%5d ($timeRel%5.1f%); space $spcOld%5d vs. $spcNew%5d ($spaceRel%5.1f%)")
  }

  //  private def writeOld(v: SynthGraph, out: DataOutput): Unit = {
  //    val oos = new java.io.ObjectOutputStream(out.asOutputStream)
  //    oos.writeObject(v)
  //    oos.flush()
  //  }

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
    // val elems     = Vector.fill[AnyRef](arity)(readElem(in, ref).asInstanceOf[AnyRef])
    val elems     = new Array[AnyRef](arity)
    var i = 0
    while (i < arity) {
      elems(i) = readElem(in, ref).asInstanceOf[AnyRef]
      i += 1
    }
    val className = if (Character.isUpperCase(prefix.charAt(0))) "de.sciss.synth.ugen." + prefix else prefix
    // cf. stackoverflow #3039822
    val companion = Class.forName(className + "$").getField("MODULE$").get(null)
    //    val m         = companion.getClass.getMethods.find(_.getName == "apply")
    //      .getOrElse(sys.error(s"No apply method found on $companion"))
    val ms        = companion.getClass.getMethods
    var m         = null: java.lang.reflect.Method
    var j = 0
    while (m == null && j < ms.length) {
      val mj = ms(j)
      if (mj.getName == "apply") m = mj
      j += 1
    }
    if (m == null) sys.error(s"No apply method found on $companion")
    val res       = m.invoke(companion, elems: _*).asInstanceOf[Product]

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

    //    val t1    = System.nanoTime()
    //    val p1    = in.position
    //    val res1  = readOld(in)
    //    val t2    = System.nanoTime()
    //    val p2    = in.position
    val res2  = readNew(in)
    //    val t3    = System.nanoTime()
    //    val p3    = in.position
    //    val timOld    = t2 - t1
    //    val timNew    = t3 - t2
    //    val spcOld    = p2 - p1
    //    val spcNew    = p3 - p2
    //    val timeRel   = timNew.toDouble/timOld.toDouble*100
    //    val spaceRel  = spcNew.toDouble/spcOld.toDouble*100
    //    println(f"<<< read >>> time ${timOld/1000}%5d vs. ${timNew/1000}%5d ($timeRel%5.1f%); space $spcOld%5d vs. $spcNew%5d ($spaceRel%5.1f%)")

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

  private final val oldTapeCookie = 1
  private final val emptyCookie   = 2
  private final val tapeCookie    = 3

  def readValue(in: DataInput): SynthGraph = ValueSerializer.read(in)

  def writeValue(value: SynthGraph, out: DataOutput): Unit = ValueSerializer.write(value, out)

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

  private lazy val oldTapeSynthGraph: SynthGraph =
    SynthGraph {
      import de.sciss.synth._
      import ugen._
      val sig   = graph.scan.In(ProcKeys.graphAudio)
      val bus   = graph.attribute(ProcKeys.attrBus   ).ir(0)
      val mute  = graph.attribute(ProcKeys.attrMute  ).ir(0)
      val env   = graph.FadeInOut(ProcKeys.attrFadeIn, ProcKeys.attrFadeOut).ar
      val amp   = env * (1 - mute)
      Out.ar(bus, sig * amp)
    }

  private lazy val tapeSynthGraph: SynthGraph =
    SynthGraph {
      import de.sciss.synth._
      val sig   = graph.scan.In(ProcKeys.graphAudio)
      val gain  = graph.attribute(ProcKeys.attrGain  ).ir(1)
      val mute  = graph.attribute(ProcKeys.attrMute  ).ir(0)
      val env   = graph.FadeInOut(ProcKeys.attrFadeIn, ProcKeys.attrFadeOut).ar
      val amp   = env * (1 - mute) * gain
      graph.scan.Out(ProcKeys.scanMainOut, sig * amp)
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
    with evt.impl.SingleGenerator[S, model.Change[SynthGraph], Ex[S]] {

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