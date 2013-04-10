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

import serial.{DataOutput, Serializer, DataInput}
import lucre.{event => evt, data, expr}
import java.io.File
import scala.annotation.tailrec
import expr.{Expr, LinkedList}
import proc.Artifact.Location.Update
import evt.{Change, EventLike}

object ArtifactImpl {
  import Artifact.Location

  private final val SER_VERSION = 0x4172

  def apply[S <: evt.Sys[S]](location: Location[S], path: List[String], name: String): Artifact[S] =
    new Impl(location, path, name)

  def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Artifact[S] = {
    val cookie = in.readShort()
    require(cookie == SER_VERSION, s"Version mismatch. Expected $SER_VERSION but found $cookie")
    val location: Location[S] = ??? //  = tx.readID(in, access)
    val pathSz    = in.readShort()
    val path      = if (pathSz == 0) Nil else List.fill(pathSz)(in.readUTF)
    val name      = in.readUTF()
    ArtifactImpl(location, path, name)
  }

  def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Artifact[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[evt.InMemory]

  private final class Ser[S <: evt.Sys[S]] extends Serializer[S#Tx, S#Acc, Artifact[S]] {
    def write(v: Artifact[S], out: DataOutput) {
      v.write(out)
    }
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Artifact[S] = ArtifactImpl.read(in, access)
  }

  private final class LocationImpl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                                    _directory: S#Var[File],
                                                    artifacts: LinkedList.Modifiable[S, Artifact[S], Unit])
    extends Location.Modifiable[S] {
    loc =>

    def id = targets.id

    def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Artifact[S]] = artifacts.iterator

    def modifiableOption: Option[Location.Modifiable[S]] = Some(this)

    def changed: EventLike[S, Update[S], Location[S]] = ???

    def directory(implicit tx: S#Tx): File = _directory()
    def directory_=(value: File)(implicit tx: S#Tx) {
      val change = evt.Change(_directory(), value)
      if (change.isSignificant) {
        _directory() = value
        ??? // store.dispatch(LocationMoved(store, loc, change))
      }
    }

    // def createFile(): File = File.createTempFile("artifact", ".bin", directory())

    def remove(artifact: Artifact[S])(implicit tx: S#Tx) {
      if (!artifacts.remove(artifact)) throw new NoSuchElementException(s"Artifact $artifact was not found in the store")
      ??? // store.dispatch(ArtifactRemoved(store, loc, artifact))
    }

    def add(file: File)(implicit tx: S#Tx): Artifact[S] = {
      val can     = file.getCanonicalFile
      val name    = can.getName
      val base    = _directory().getCanonicalFile
      val folder  = can.getParentFile

      @tailrec def loop(res: List[String], left: File): List[String] = {
        if (left == null)
          throw new IllegalArgumentException(s"File $file is not inside artifact store's base directory $base")

        if (left == base) res
        else {
          val last  = left.getName
          val init  = left.getParentFile
          loop(last :: res, init)
        }
      }

      val path      = loop(Nil, folder)
      val artifact: Artifact[S] = ??? //  = ArtifactImpl(id, path, name)
      ??? // store.dispatch(ArtifactAdded(store, loc, artifact))
      artifact
    }

    def write(out: DataOutput) {
      targets.write(out)
      writeData(out)
    }

    protected def writeData(out: DataOutput) {
      _directory.write(out)
      artifacts .write(out)
    }

    def dispose()(implicit tx: S#Tx) {
      targets.dispose()
      disposeData()
    }

    protected def disposeData()(implicit tx: S#Tx) {
      _directory.dispose()
      artifacts .dispose()
    }
  }

  private final case class Impl[S <: evt.Sys[S]](location: Location[S], path: List[String], name: String)
    extends Artifact[S] {

    override def toString = s"Artifact(${if (path.isEmpty) "" else path.mkString("", "/", "/")}$name)"

    //    def toFile(implicit store: ArtifactStoreLike): File = {
    //      // XXX TODO: in the future we could have a better resolution scheme
    //      val base   = store.baseDirectory
    //      val folder = if (path.isEmpty) base else (base /: path)( (res, sub) => new File(res, sub))
    //      new File(folder, name)
    //    }

    def changed: EventLike[S, Change[Artifact.Value], Expr[S, Artifact.Value]] = ???

    def value(implicit tx: S#Tx): Artifact.Value = ???

    def dispose()(implicit tx: S#Tx) {
      location.dispose()
    }

    def write(out: DataOutput) {
      out.writeShort(SER_VERSION)
      location.write(out)
      if (path.isEmpty) out.writeShort(0) else {
        out.writeShort(path.size)
        path.foreach(out.writeUTF _)
      }
      out.writeUTF(name)
    }
  }
}