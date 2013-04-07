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

  def apply(path: String): Artifact = new Impl(path)

  def read(in: DataInput): Artifact = {
    val cookie = in.readShort()
    require(cookie == SER_VERSION, s"Version mismatch. Expected $SER_VERSION but found $cookie")
    val path = in.readUTF()
    ArtifactImpl(path)
  }

  implicit object serializer extends ImmutableSerializer[Artifact] {
    def write(v: Artifact, out: DataOutput) {
      v.write(out)
    }
    def read(in: DataInput): Artifact = ArtifactImpl.read(in)
  }

  private final case class Impl(path: String) extends Artifact {
    override def toString = "Artifact(" + path + ")"

    def toFile(implicit store: ArtifactStore[_]): File = {
      // XXX TODO: in the future we could have a better resolution scheme
      new File(store.baseDirectory, path)
    }

    def write(out: DataOutput) {
      out.writeShort(SER_VERSION)
      out.writeUTF(path)
    }
  }
}