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

  private def writeProduct(p: Product, out: DataOutput) {
    out.writeByte('P')
    out.writeUTF(p.productPrefix)
    out.writeShort(p.productArity)
    p.productIterator.foreach(writeElem(_, out))
  }

  private def writeElemSeq(xs: Seq[Any], out: DataOutput) {
    out.writeByte('X')
    out.writeInt(xs.size)
    xs.foreach(writeElem(_, out))
  }

  private def writeElem(e: Any, out: DataOutput) {
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
        if (o.isDefined) writeElem(o.get, out)
      case xs: Seq[_] =>  // 'X'. either indexed seq or var arg (e.g. wrapped array)
        writeElemSeq(xs, out)
      case p: Product =>
        writeProduct(p, out) // 'P'
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
    out.writeShort(SER_VERSION)
    writeElemSeq(v.sources, out)
    val ctl = v.controlProxies
    out.writeByte('T')
    out.writeInt(ctl.size)
    ctl.foreach(writeProduct(_, out))
  }


  // expects that 'X' byte has already been read
  private def readIdentifiedSeq(in: DataInput): Seq[Any] = {
    val num = in.readInt()
    Vector.fill(num)(readElem(in))
  }

  // expects that 'P' byte has already been read
  private def readIdentifiedProduct(in: DataInput): Product = {
    val prefix    = in.readUTF()
    val arity     = in.readShort()
    val elems     = Vector.fill[AnyRef](arity)(readElem(in).asInstanceOf[AnyRef])
    // cf. stackoverflow #3039822
    val companion = Class.forName(s"de.sciss.synth.ugen.$prefix$$").getField("MODULE$").get(null)
    // val m         = companion.getClass.getMethod("apply", elems.map(_.getClass): _*)
    val m         = companion.getClass.getMethods.find(_.getName == "apply")
      .getOrElse(sys.error(s"No apply method found on $companion"))

    // try {
      m.invoke(companion, elems: _*).asInstanceOf[Product]
    //    } catch {
    //      case i: IllegalArgumentException =>
    //        println(s"IllegalArgumentException. companion = $companion, m = $m, elems = $elems")
    //        throw i
    //    }
  }

  private def readElem(in: DataInput): Any = {
    (in.readByte(): @switch) match {
      case 'C' => Constant(in.readFloat())
      case 'R' => MaybeRate(in.readByte())
      case 'O' => if (in.readBoolean()) Some(readElem(in)) else None
      case 'X' => readIdentifiedSeq(in)
      case 'P' => readIdentifiedProduct(in)
      case 'I' => in.readInt()
      case 'S' => in.readUTF()
      case 'B' => in.readBoolean()
      case 'F' => in.readFloat()
      case 'D' => in.readDouble()
    }
  }

  def read(in: DataInput): SynthGraph = {
    val cookie = in.readShort()
    require(cookie == SER_VERSION, s"Unexpected cookie $cookie")
    val b1 = in.readByte()
    require(b1 == 'X')    // expecting sequence
    val numSources  = in.readInt()
    val sources     = Vector.fill(numSources) {
      readElem(in) match {
        case lz: Lazy => lz
        case other    => sys.error(s"Expected Lazy but found $other")
      }
    }
    val b2 = in.readByte()
    require(b2 == 'T')    // expecting set
    val numControls = in.readInt()
    val controls    = Set.newBuilder[ControlProxyLike] // stupid Set doesn't have `fill` and `tabulate` methods
    for (_ <- 0 until numControls) {
      controls += (readElem(in) match {
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