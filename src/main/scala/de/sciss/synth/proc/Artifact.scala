/*
 *  Artifact.scala
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

package de.sciss.synth.proc

import java.io.File
import impl.{ArtifactImpl => Impl}
import de.sciss.serial.{Writable, ImmutableSerializer, DataInput}
import scala.annotation.tailrec

object Artifact {
  // def apply(path: List[String], name: String): Artifact = Impl(path, name)

  def read(in: DataInput): Artifact = Impl.read(in)

  //  def fromFile(file: File)(implicit store: ArtifactStoreLike): Artifact = {
  //    val can     = file.getCanonicalFile
  //    val name    = can.getName
  //    val base    = store.baseDirectory.getCanonicalFile
  //    val folder  = can.getParentFile
  //
  //    @tailrec def loop(res: List[String], left: File): List[String] = {
  //      if (left == null)
  //        throw new IllegalArgumentException(s"File $file is not inside artifact store's base directory $base")
  //
  //      if (left == base) res
  //      else {
  //        val last  = left.getName
  //        val init  = left.getParentFile
  //        loop(last :: res, init)
  //      }
  //    }
  //
  //    val path    = loop(Nil, folder)
  //    Impl(path, name)
  //  }

  implicit def serializer: ImmutableSerializer[Artifact] = Impl.serializer
}

trait Artifact extends Writable {
  def key: Int
  def name: String
  def path: List[String]
  // def toFile[S <: evt.Sys[S]](implicit store: ArtifactStoreLike): File
}