/*
 *  SynthGraphSerializer.scala
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

import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import scala.annotation.switch
import ugen.{ControlProxyLike, Constant}

/** A serializer for synth graphs. */
object SynthGraphSerializer extends ImmutableSerializer[SynthGraph] {
  //  private val map = mutable.Map.empty[String, SynthGraph]
  //
  //  def register(key: String, graph: SynthGraph) {
  //    map.synchronized(map += key -> graph)
  //  }

  private final val SER_VERSION = 0x5347

  private final class RefMapOut {
    var map   = Map.empty[Product, Int]
    var count = 0
  }

  private final class RefMapIn {
    var map   = Map.empty[Int, Product]
    var count = 0
  }

  private def writeProduct(p: Product, out: DataOutput, ref: RefMapOut) {
    ref.map.get(p).foreach { id =>
      out.writeByte('<')
      out.writeInt(id)
      return
    }
    out.writeByte('P')
    val pck     = p.getClass.getPackage.getName
    val prefix  = p.productPrefix
    val name    = if (pck == "de.sciss.synth.ugen") prefix else s"$pck.$prefix"
    out.writeUTF(name)
    out.writeShort(p.productArity)
    p.productIterator.foreach(writeElem(_, out, ref))

    val id     = ref.count
    ref.map   += p -> id
    ref.count  = id + 1
  }

  private def writeElemSeq(xs: Seq[Any], out: DataOutput, ref: RefMapOut) {
    out.writeByte('X')
    out.writeInt(xs.size)
    xs.foreach(writeElem(_, out, ref))
  }

  private def writeElem(e: Any, out: DataOutput, ref: RefMapOut) {
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
  }

  def write(v: SynthGraph, out: DataOutput) {
    val ref = new RefMapOut
    out.writeShort(SER_VERSION)
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
    val elems     = Vector.fill[AnyRef](arity)(readElem(in, ref).asInstanceOf[AnyRef])
    val className = if (prefix.charAt(0).isUpper) "de.sciss.synth.ugen." + prefix else prefix
    // cf. stackoverflow #3039822
    val companion = Class.forName(className + "$").getField("MODULE$").get(null)
    val m         = companion.getClass.getMethods.find(_.getName == "apply")
      .getOrElse(sys.error(s"No apply method found on $companion"))
    val res       = m.invoke(companion, elems: _*).asInstanceOf[Product]

    val id        = ref.count
    ref.map      += id -> res
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
    val ref     = new RefMapIn
    val cookie  = in.readShort()
    require(cookie == SER_VERSION, s"Unexpected cookie $cookie")
    val b1 = in.readByte()
    require(b1 == 'X')    // expecting sequence
    val numSources  = in.readInt()
    val sources     = Vector.fill(numSources) {
      readElem(in, ref) match {
        case lz: Lazy => lz
        case other    => sys.error(s"Expected Lazy but found $other")
      }
    }
    val b2 = in.readByte()
    require(b2 == 'T')    // expecting set
    val numControls = in.readInt()
    val controls    = Set.newBuilder[ControlProxyLike] // stupid Set doesn't have `fill` and `tabulate` methods
    for (_ <- 0 until numControls) {
      controls += (readElem(in, ref) match {
        case ctl: ControlProxyLike  => ctl
        case other                  => sys.error(s"Expected ControlProxyLike but found $other")
      })
    }
    SynthGraph(sources, controls.result())
  }

  //  def read(in: DataInput): SynthGraph = {
  //    val cookie = in.readShort()
  //    require(cookie == SER_VERSION, s"Unexpected cookie $cookie")
  //    val ois = new java.io.ObjectInputStream(in.asInputStream)
  //    val res = ois.readObject().asInstanceOf[SynthGraph]
  //    ois.close()
  //    res
  //  }
}