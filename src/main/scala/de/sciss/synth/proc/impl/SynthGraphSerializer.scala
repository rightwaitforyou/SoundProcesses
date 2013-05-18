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

import java.io.{ObjectInputStream, ObjectOutputStream}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import collection.mutable

/**
 * A serializer using plain old java (object output) serialization
 */
object SynthGraphSerializer extends ImmutableSerializer[SynthGraph] {
  //  private val map = mutable.Map.empty[String, SynthGraph]
  //
  //  def register(key: String, graph: SynthGraph) {
  //    map.synchronized(map += key -> graph)
  //  }

  private final val SER_VERSION = 0x5347

  def write(v: SynthGraph, out: DataOutput) {
    out.writeShort(SER_VERSION)
    val oos = new ObjectOutputStream(out.asOutputStream)
    oos.writeObject(v)
    oos.close()
  }

  def read(in: DataInput): SynthGraph = {
    val cookie = in.readShort()
    require(cookie == SER_VERSION, s"Unexpected cookie $cookie")
    val ois = new ObjectInputStream(in.asInputStream)
    val res = ois.readObject().asInstanceOf[SynthGraph]
    ois.close()
    res
  }
}
