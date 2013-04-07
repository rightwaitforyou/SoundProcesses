/*
 *  ArtifactImpl.scala
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

package de.sciss
package synth
package proc
package impl

import java.io.File
import de.sciss.serial.{DataOutput, ImmutableSerializer, DataInput}

object ArtifactImpl {
  private final val SER_VERSION = 0x4172

  def apply(path: List[String], name: String): Artifact = new Impl(path, name)

  def read(in: DataInput): Artifact = {
    val cookie = in.readShort()
    require(cookie == SER_VERSION, s"Version mismatch. Expected $SER_VERSION but found $cookie")
    val pathSz  = in.readShort()
    val path    = if (pathSz == 0) Nil else List.fill(pathSz)(in.readUTF)
    val name    = in.readUTF()
    ArtifactImpl(path, name)
  }

  implicit object serializer extends ImmutableSerializer[Artifact] {
    def write(v: Artifact, out: DataOutput) {
      v.write(out)
    }
    def read(in: DataInput): Artifact = ArtifactImpl.read(in)
  }

  private final case class Impl(path: List[String], name: String) extends Artifact {
    override def toString = s"Artifact(${if (path.isEmpty) "" else path.mkString("", "/", "/")}$name)"

    def toFile(implicit store: ArtifactStoreLike): File = {
      // XXX TODO: in the future we could have a better resolution scheme
      val base   = store.baseDirectory
      val folder = if (path.isEmpty) base else (base /: path)( (res, sub) => new File(res, sub))
      new File(folder, name)
    }

    def write(out: DataOutput) {
      out.writeShort(SER_VERSION)
      if (path.isEmpty) out.writeShort(0) else {
        out.writeShort(path.size)
        path.foreach(out.writeUTF _)
      }
      out.writeUTF(name)
    }
  }
}