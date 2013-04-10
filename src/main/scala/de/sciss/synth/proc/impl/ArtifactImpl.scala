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
import de.sciss.lucre.event.{Pull, Change, EventLike}
import de.sciss.lucre.event.impl.MultiEventImpl

object ArtifactImpl {
  import Artifact.Location

  private final val SER_VERSION = 0x4172

  // ---- artifact ----

  def apply[S <: evt.Sys[S]](location: Location[S], child: File)(implicit tx: S#Tx): Artifact.Modifiable[S] = {
    val targets = evt.Targets[S]
    val _child  = tx.newVar(targets.id, child)
    new Impl(targets, location, _child)
  }

  def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Artifact[S] =
    serializer[S].read(in, access)

  def serializer[S <: evt.Sys[S]]: Serializer[S#Tx, S#Acc, Artifact[S]] with evt.Reader[S, Artifact[S]] =
    anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[evt.InMemory]

  private final class Ser[S <: evt.Sys[S]] extends Serializer[S#Tx, S#Acc, Artifact[S]] with evt.Reader[S, Artifact[S]] {
    def write(v: Artifact[S], out: DataOutput) {
      v.write(out)
    }
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Artifact[S] = {
      val targets = evt.Targets.read(in, access)
      read(in, access, targets)
    }

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Artifact[S] with evt.Node[S] = {
      val cookie    = in.readShort()
      require(cookie == SER_VERSION, s"Version mismatch. Expected $SER_VERSION but found $cookie")
      val location  = readLocation(in, access)
      val _child    = tx.readVar[File](targets.id, in)
      new Impl(targets, location, _child)
    }
  }

  // ---- location ----

  def newLocation[S <: evt.Sys[S]](init: File)(implicit tx: S#Tx): Location.Modifiable[S] = ???

  def readLocation[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Location[S] = ???

  def locationSerializer[S <: evt.Sys[S]]: Serializer[S#Tx, S#Acc, Location[S]] = ??? // anySer.asInstanceOf[ModSer[S]]

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

    def add(file: File)(implicit tx: S#Tx): Artifact.Modifiable[S] = {
      val base      = _directory()
      val child     = Artifact.relativize(base, file)
      val artifact  = ArtifactImpl.apply(loc, child)
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

  private final class Impl[S <: evt.Sys[S]](protected val targets: evt.Targets[S],
                                            val location: Location[S], _child: S#Var[File])
    extends Artifact.Modifiable[S] with evt.impl.StandaloneLike[S, Change[File], Artifact[S]] {

    // override def toString = s"Artifact(${if (path.isEmpty) "" else path.mkString("", "/", "/")}$name)"
    override def toString() = s"Artifact@${hashCode().toHexString}"

    def child(implicit tx: S#Tx): File = _child()

    def child_=(value: File)(implicit tx: S#Tx) {
      val old = _child()
      if (old != value) {
        _child() = value
        ??? // dispatch change
      }
    }

    def changed: EventLike[S, Change[Artifact.Value], Expr[S, Artifact.Value]] = this

    def connect()(implicit tx: S#Tx) {
      location.changed ---> this
    }

    def disconnect()(implicit tx: S#Tx) {
      location.changed -/-> this
    }

    def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[Change[File]] = {
      ???
    }

    protected def reader: evt.Reader[S, Artifact[S]] = ArtifactImpl.serializer

    def value(implicit tx: S#Tx): Artifact.Value = {
      val base   = location.directory
      val child  = _child()
      new File(base, child.getPath)
    }

    protected def disposeData()(implicit tx: S#Tx) {
      // location.dispose()
      _child.dispose()
    }

    protected def writeData(out: DataOutput) {
      out.writeShort(SER_VERSION)
      location.write(out)
      _child.write(out)
      //      if (path.isEmpty) out.writeShort(0) else {
      //        out.writeShort(path.size)
      //        path.foreach(out.writeUTF _)
      //      }
      //      out.writeUTF(name)
    }
  }
}