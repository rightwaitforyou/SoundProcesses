/*
 *  ArtifactStore.scala
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

import lucre.{event => evt, data}
import java.io.File
import impl.{ArtifactStoreImpl => Impl}
import serial.{Writable, Serializer, DataInput}
import de.sciss.lucre.event.{EventLike, Event}
import de.sciss.lucre.stm.Disposable

object ArtifactStore {
  def tmp[S <: evt.Sys[S], D <: evt.Sys[D]]()(implicit tx: S#Tx, bridge: S#Tx => D#Tx): (ArtifactStore.Modifiable[S], Location.Modifiable[S]) = {
    val store = apply[S, D]
    val dir   = File.createTempFile("artifacts", "tmp")
    dir.delete()
    dir.mkdir()
    dir.deleteOnExit()
    val loc   = store.addLocation(dir)
    (store, loc)
  }

  def apply[S <: evt.Sys[S], D <: evt.Sys[D]](implicit tx: S#Tx, bridge: S#Tx => D#Tx): ArtifactStore.Modifiable[S] =
    Impl[S, D]

  implicit def serializer[S <: evt.Sys[S]]: Serializer[S#Tx, S#Acc, ArtifactStore[S]] = Impl.serializer[S]

  def read[S <: evt.Sys[S], D <: evt.Sys[D]](in: DataInput, access: S#Acc)
                                            (implicit tx: S#Tx, bridge: S#Tx => D#Tx): ArtifactStore[S] =
    Impl.read[S, D](in, access)

  object Location {
    trait Modifiable[S <: evt.Sys[S]] extends Location[S] {
      //      /**
      //       * Creates a new artifact. This is a side-effect and
      //       * thus should be called outside of a transaction.
      //       * The artifact is not in any way registered with the system.
      //       * Once the artifact has meaningful content, it may be
      //       * registered by calling the `register` method.
      //       *
      //       * @return  the new file.
      //       */
      //      def createFile(): File

      /**
       * Registers a significant artifact with the system. That is,
       * stores the artifact, which should have a real resource
       * association, as belonging to the system.
       *
       * @param file   the file to turn into a registered artifact
       */
      def add(file: File)(implicit tx: S#Tx): Artifact

      def directory_=(value: File)(implicit tx: S#Tx): Unit
    }
  }
  trait Location[S <: evt.Sys[S]] {
    def key: Int
    def directory(implicit tx: S#Tx): File
    def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Artifact]
  }

  trait Modifiable[S <: evt.Sys[S]] extends ArtifactStore[S] {
    def remove(artifact: Artifact)(implicit tx: S#Tx): Unit
    def addLocation(directory: File)(implicit tx: S#Tx): Location.Modifiable[S]
    def removeLocation(location: Location[S])(implicit tx: S#Tx): Unit
  }

  sealed trait Update[S <: evt.Sys[S]] {
    def store: ArtifactStore[S]
    def location: Location[S]
  }
  sealed trait ArtifactUpdate {
    def artifact: Artifact
  }
  final case class ArtifactAdded[S <: evt.Sys[S]](store: ArtifactStore[S], location: Location[S], artifact: Artifact)
    extends Update[S] with ArtifactUpdate

  final case class ArtifactRemoved[S <: evt.Sys[S]](store: ArtifactStore[S], location: Location[S], artifact: Artifact)
    extends Update[S] with ArtifactUpdate

  final case class LocationAdded  [S <: evt.Sys[S]](store: ArtifactStore[S], location: Location[S]) extends Update[S]
  final case class LocationRemoved[S <: evt.Sys[S]](store: ArtifactStore[S], location: Location[S]) extends Update[S]

  final case class LocationMoved[S <: evt.Sys[S]](store: ArtifactStore[S], location: Location[S],
                                                  change: evt.Change[File]) extends Update[S]
}

trait ArtifactStore[S <: evt.Sys[S]] extends Writable with Disposable[S#Tx] {
  def locations(implicit tx: S#Tx): data.Iterator[S#Tx, ArtifactStore.Location[S]]
  def changed: EventLike[S, ArtifactStore.Update[S], ArtifactStore[S]]
  def resolve(artifact: Artifact)(implicit tx: S#Tx): File
}
