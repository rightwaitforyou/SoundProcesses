/*
 *  Artifact.scala
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

package de.sciss
package synth
package proc

import impl.{ArtifactImpl => Impl}
import serial.{Serializer, DataInput}
import lucre.{stm, event => evt, data, expr}
import stm.Mutable
import java.io.File
import de.sciss.lucre.event.Publisher
import expr.Expr
import scala.annotation.tailrec
import de.sciss.model
import de.sciss.lucre.synth.Sys

object Artifact {
  def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Artifact[S] = Impl.read(in, access)

  implicit def serializer[S <: evt.Sys[S]]: Serializer[S#Tx, S#Acc, Artifact[S]] = Impl.serializer

  def relativize(parent: File, sub: File): Child = {
    // Note: .getCanonicalFile will resolve symbolic links.
    // In order to support artifacts being symbolic links
    // inside a parent folder, we must not resolve them!

    val can     = sub   .getAbsoluteFile // .getCanonicalFile
    val base    = parent.getAbsoluteFile // .getCanonicalFile

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

    val cf = loop(new File(can.getName), can.getParentFile)
    Child(cf.getPath)
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

      def apply[S <: Sys[S]](init: File)(implicit tx: S#Tx): Location.Modifiable[S] = Impl.newLocation[S](init)

      implicit def serializer[S <: evt.Sys[S]]: Serializer[S#Tx, S#Acc, Location.Modifiable[S]] =
        Impl.modLocationSerializer

      def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Location.Modifiable[S] =
        Impl.readModLocation[S](in, access)
    }
    trait Modifiable[S <: evt.Sys[S]] extends Location[S] {
      /** Registers a significant artifact with the system. That is,
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

    final case class Moved[S <: evt.Sys[S]](location: Location[S], change: model.Change[File]) extends Update[S]

    implicit def serializer[S <: evt.Sys[S]]: Serializer[S#Tx, S#Acc, Location[S]] = Impl.locationSerializer

    def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Location[S] =
      Impl.readLocation[S](in, access)
  }
  trait Location[S <: evt.Sys[S]] extends Mutable[S#ID, S#Tx] with Publisher[S, Location.Update[S]] {
    def directory(implicit tx: S#Tx): File
    def iterator (implicit tx: S#Tx): data.Iterator[S#Tx, Artifact[S]]

    def modifiableOption: Option[Location.Modifiable[S]]
  }

  type Value = File

  // sealed trait Update[S <: evt.Sys[S]]
  // final case class

  object Modifiable {
    def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Modifiable[S] =
      Impl.readMod(in, access)

    def copy[S <: evt.Sys[S]](from: Artifact[S])(implicit tx: S#Tx): Modifiable[S] =
      Impl.copy(from)

    implicit def serializer[S <: evt.Sys[S]]: Serializer[S#Tx, S#Acc, Modifiable[S]] = Impl.modSerializer
  }
  trait Modifiable[S <: evt.Sys[S]] extends Artifact[S] {
    def child_=(value: Child)(implicit tx: S#Tx): Unit
  }

  final case class Child(path: String)
}

trait Artifact[S <: evt.Sys[S]] extends Expr[S, Artifact.Value] /* Mutable[S#ID, S#Tx] */ {
  import Artifact._
  def location: Location[S]
  def modifiableOption: Option[Modifiable[S]]
  def child(implicit tx: S#Tx): Child
}