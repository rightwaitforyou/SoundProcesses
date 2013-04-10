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
import serial.{Writable, Serializer, DataInput}
import lucre.{stm, event => evt, data, expr}
import stm.{Disposable, Mutable}
import java.io.File
import evt.EventLike
import expr.Expr

object Artifact {
  def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Artifact[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Artifact[S]] = Impl.serializer

  object Location {
    object Modifiable {
      def tmp[S <: Sys[S]]()(implicit tx: S#Tx): Location.Modifiable[S] = {
        val dir   = File.createTempFile("artifacts", "tmp")
        dir.delete()
        dir.mkdir()
        dir.deleteOnExit()
        apply(dir)
      }
      def apply[S <: Sys[S]](init: File): Location.Modifiable[S] = ???
    }
    trait Modifiable[S <: evt.Sys[S]] extends Location[S] {
      /**
       * Registers a significant artifact with the system. That is,
       * stores the artifact, which should have a real resource
       * association, as belonging to the system.
       *
       * @param file   the file to turn into a registered artifact
       */
      def add(file: File)(implicit tx: S#Tx): Artifact[S]
      def remove(artifact: Artifact[S])(implicit tx: S#Tx): Unit

      def directory_=(value: File)(implicit tx: S#Tx): Unit
    }

    sealed trait Update[S <: evt.Sys[S]] {
      def location: Location[S]
    }
    final case class Added[S <: Sys[S]](location: Location[S], artifact: Artifact[S])
      extends Update[S]

    final case class Removed[S <: Sys[S]](location: Location[S], artifact: Artifact[S])
      extends Update[S]

    final case class Moved[S <: Sys[S]](location: Location[S], change: evt.Change[File]) extends Update[S]

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Location[S]] = ??? // Impl.locSerializer

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Location[S] =
        ??? // Impl.locRead[S](in, access)
  }
  trait Location[S <: evt.Sys[S]] extends /* Writable with Disposable[S#Tx] */ Mutable[S#ID, S#Tx] {
    def directory(implicit tx: S#Tx): File
    def iterator (implicit tx: S#Tx): data.Iterator[S#Tx, Artifact[S]]

    def modifiableOption: Option[Location.Modifiable[S]]

    def changed: EventLike[S, Location.Update[S], Location[S]]
  }

  // final case class Value(file: File)
  type Value = File
}

trait Artifact[S <: evt.Sys[S]] extends Expr[S, Artifact.Value] /* Mutable[S#ID, S#Tx] */ {
  import Artifact._
  def location: Location[S]
  // def value(implicit tx: S#Tx): Value
  // def changed: EventLike[S, Update[S], Artifact[S]]
  // def name: String
  // def path: List[String]
  // def toFile[S <: Sys[S]](implicit store: ArtifactStoreLike): File
}