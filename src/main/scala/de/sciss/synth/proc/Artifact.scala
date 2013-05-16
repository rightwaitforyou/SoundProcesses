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

package de.sciss
package synth
package proc

import impl.{ArtifactImpl => Impl}
import serial.{Serializer, DataInput}
import lucre.{stm, event => evt, data, expr}
import stm.Mutable
import java.io.File
import evt.EventLike
import expr.Expr
import scala.annotation.tailrec

object Artifact {
  def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Artifact[S] = Impl.read(in, access)

  implicit def serializer[S <: evt.Sys[S]]: Serializer[S#Tx, S#Acc, Artifact[S]] = Impl.serializer

  def relativize(parent: File, sub: File): File = {
    val can     = sub.getCanonicalFile
    val base    = parent.getCanonicalFile

    @tailrec def loop(res: File, left: File): File = {
      if (left == null)
        throw new IllegalArgumentException(s"File $sub is not in a subdirectory of $parent")

      if (left == base) res
      else {
        val last  = left.getName
        val init  = left.getParentFile
        loop(new File(last, res.getPath), init)
      }
    }

    loop(new File(can.getName), can.getParentFile)
  }

  object Location {
    object Modifiable {
      def tmp[S <: Sys[S]]()(implicit tx: S#Tx): Location.Modifiable[S] = {
        val dir   = File.createTempFile("artifacts", "tmp")
        dir.delete()
        dir.mkdir()
        dir.deleteOnExit()
        apply(dir)
      }

      def apply[S <: Sys[S]](init: File)(implicit tx: S#Tx): Location.Modifiable[S] = Impl.newLocation(init)

      implicit def serializer[S <: evt.Sys[S]]: Serializer[S#Tx, S#Acc, Location.Modifiable[S]] =
        Impl.modLocationSerializer

      def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Location.Modifiable[S] =
        Impl.readModLocation[S](in, access)
    }
    trait Modifiable[S <: evt.Sys[S]] extends Location[S] {
      /**
       * Registers a significant artifact with the system. That is,
       * stores the artifact, which should have a real resource
       * association, as belonging to the system.
       *
       * @param file   the file to turn into a registered artifact
       */
      def add(file: File)(implicit tx: S#Tx): Artifact.Modifiable[S]
      def remove(artifact: Artifact[S])(implicit tx: S#Tx): Unit

      def directory_=(value: File)(implicit tx: S#Tx): Unit
    }

    sealed trait Update[S <: evt.Sys[S]] {
      def location: Location[S]
    }
    final case class Added[S <: evt.Sys[S]](location: Location[S], idx: Int, artifact: Artifact[S])
      extends Update[S]

    final case class Removed[S <: evt.Sys[S]](location: Location[S], idx: Int, artifact: Artifact[S])
      extends Update[S]

    final case class Moved[S <: evt.Sys[S]](location: Location[S], change: evt.Change[File]) extends Update[S]

    implicit def serializer[S <: evt.Sys[S]]: Serializer[S#Tx, S#Acc, Location[S]] = Impl.locationSerializer

    def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Location[S] =
      Impl.readLocation[S](in, access)
  }
  trait Location[S <: evt.Sys[S]] extends /* Writable with Disposable[S#Tx] */ Mutable[S#ID, S#Tx] {
    def directory(implicit tx: S#Tx): File
    def iterator (implicit tx: S#Tx): data.Iterator[S#Tx, Artifact[S]]

    def modifiableOption: Option[Location.Modifiable[S]]

    def changed: EventLike[S, Location.Update[S], Location[S]]
  }

  type Value = File

  // sealed trait Update[S <: evt.Sys[S]]
  // final case class

  object Modifiable {
    def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Modifiable[S] =
      Impl.readMod(in, access)

    def apply[S <: evt.Sys[S]](from: Artifact[S])(implicit tx: S#Tx): Modifiable[S] =
      Impl.copy(from)

    implicit def serializer[S <: evt.Sys[S]]: Serializer[S#Tx, S#Acc, Modifiable[S]] = Impl.modSerializer
  }
  trait Modifiable[S <: evt.Sys[S]] extends Artifact[S] {
    def child_=(value: File)(implicit tx: S#Tx): Unit
  }
}

trait Artifact[S <: evt.Sys[S]] extends Expr[S, Artifact.Value] /* Mutable[S#ID, S#Tx] */ {
  import Artifact._
  def location: Location[S]
  def modifiableOption: Option[Modifiable[S]]
  def child(implicit tx: S#Tx): File
}