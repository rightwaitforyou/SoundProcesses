/*
 *  ArtifactLocation.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.event.{Publisher, Sys}
import java.io.File
import de.sciss.synth.proc.impl.{ElemImpl, ArtifactImpl}
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.model
import de.sciss.synth.proc
import de.sciss.lucre.stm.Mutable
import de.sciss.lucre.data
import impl.{ArtifactImpl => Impl}

object ArtifactLocation {
  object Modifiable {
    def tmp[S <: Sys[S]]()(implicit tx: S#Tx): ArtifactLocation.Modifiable[S] = {
      val dir   = File.createTempFile("artifacts", "tmp")
      dir.delete()
      dir.mkdir()
      dir.deleteOnExit()
      apply(dir)
    }

    def apply[S <: Sys[S]](init: File)(implicit tx: S#Tx): ArtifactLocation.Modifiable[S] =
      Impl.newLocation[S](init)

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ArtifactLocation.Modifiable[S]] =
      Impl.modLocationSerializer

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ArtifactLocation.Modifiable[S] =
      Impl.readModLocation[S](in, access)
  }
  trait Modifiable[S <: Sys[S]] extends ArtifactLocation[S] {
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

  sealed trait Update[S <: Sys[S]] {
    def location: ArtifactLocation[S]
  }
  final case class Added[S <: Sys[S]](location: ArtifactLocation[S], idx: Int, artifact: Artifact[S])
    extends Update[S]

  final case class Removed[S <: Sys[S]](location: ArtifactLocation[S], idx: Int, artifact: Artifact[S])
    extends Update[S]

  final case class Moved[S <: Sys[S]](location: ArtifactLocation[S], change: model.Change[File]) extends Update[S]

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ArtifactLocation[S]] = Impl.locationSerializer

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ArtifactLocation[S] =
    Impl.readLocation[S](in, access)

  // ---- Elem ----

  object Elem {
    def apply[S <: Sys[S]](peer: ArtifactLocation[S])(implicit tx: S#Tx): ArtifactLocation.Elem[S] =
      proc.impl.ElemImpl.ArtifactLocation(peer)

    object Obj {
      def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, ArtifactLocation.Elem]] =
        if (obj.elem.isInstanceOf[Elem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, ArtifactLocation.Elem]])
        else None
    }

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ArtifactLocation.Elem[S]] =
      ElemImpl.ArtifactLocation.serializer[S]
  }
  trait Elem[S <: Sys[S]] extends proc.Elem[S] {
    type Peer       = ArtifactLocation[S]
    type PeerUpdate = ArtifactLocation.Update[S]
  }
}
trait ArtifactLocation[S <: Sys[S]] extends Mutable[S#ID, S#Tx] with Publisher[S, ArtifactLocation.Update[S]] {
  def directory(implicit tx: S#Tx): File
  def iterator (implicit tx: S#Tx): data.Iterator[S#Tx, Artifact[S]]

  def modifiableOption: Option[ArtifactLocation.Modifiable[S]]
}
