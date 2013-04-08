/*
 *  ArtifactStoreImpl.scala
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
import lucre.{expr, data, event => evt}
import expr.LinkedList
import de.sciss.serial.{DataOutput, Serializer, DataInput}
import de.sciss.lucre.data.SkipList
import evt.{impl => evti}
import evt.EventLike
import scala.annotation.tailrec
import de.sciss.synth.proc.ArtifactStore.{Location, Update, ArtifactAdded, ArtifactRemoved, LocationAdded, LocationRemoved, LocationMoved}

object ArtifactStoreImpl {
  private final val SER_VERSION = 0x4153

  def apply[S <: evt.Sys[S], D <: evt.Sys[D]](implicit tx: S#Tx, bridge: S#Tx => D#Tx): ArtifactStore.Modifiable[S] = {
    implicit val dtx = bridge(tx)
    val targets = evt.Targets[D]
    val _locKey = dtx.newIntVar(targets.id, 0)
    new NewImpl[S, D](targets, _locKey)
  }

  def serializer[S <: evt.Sys[S], D <: evt.Sys[D]](implicit bridge: S#Tx => D#Tx)
  : Serializer[S#Tx, S#Acc, ArtifactStore[S]] with evt.Reader[S, ArtifactStore[S]] = new Ser

  def read[S <: evt.Sys[S], D <: evt.Sys[D]](in: DataInput, access: S#Acc)
                                            (implicit tx: S#Tx, bridge: S#Tx => D#Tx): ArtifactStore[S] =
    serializer[S, D].read(in, access)

  private[ArtifactStoreImpl] def read[S <: evt.Sys[S], D <: evt.Sys[D]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                      (implicit tx: S#Tx, bridge: S#Tx => D#Tx): Impl[S, D] = {
    val dtx = bridge(tx)
    val cookie = in.readShort()
    require(cookie == SER_VERSION, s"Version mismatch. Expected $SER_VERSION but found $cookie")
    val _locKey = dtx.readIntVar(targets.id, in)
    new ReadImpl(targets, _locKey, in, access)
  }

  // private final val anySer = new Ser[evt.InMemory, evt.InMemory]

  private final class Ser[S <: evt.Sys[S], D <: evt.Sys[D]](implicit bridge: S#Tx => D#Tx)
    extends Serializer[S#Tx, S#Acc, ArtifactStore[S]] with evt.Reader[S, ArtifactStore[S]] {
    def write(v: ArtifactStore[S], out: DataOutput) {
      v.write(out)
    }

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): ArtifactStore[S] = {
      val targets = evt.Targets.read(in, access)
      read(in, access, targets)
    }

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])
            (implicit tx: S#Tx): ArtifactStore[S] with evt.Node[S] = ArtifactStoreImpl.read[S, D](in, access, targets)
  }

  private final class LocationImpl[S <: evt.Sys[S], D <: evt.Sys[D]](store: Impl[S, D], val key: Int,
                                                                     count: D#Var[Int], _directory: D#Var[File],
                                                    artifacts: LinkedList.Modifiable[D, Artifact, Unit])
    extends ArtifactStore.Location.Modifiable[S] {
    loc =>

    def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Artifact] = ??? // artifacts.iterator

    def directory(implicit tx: S#Tx): File = {
      implicit val dtx = store.bridge(tx)
      _directory()
    }
    def directory_=(value: File)(implicit tx: S#Tx) {
      implicit val dtx = store.bridge(tx)
      val change = evt.Change(_directory(), value)
      if (change.isSignificant) {
        _directory() = value
        store.dispatch(LocationMoved(store, loc, change))
      }
    }

    // def createFile(): File = File.createTempFile("artifact", ".bin", directory())

    def remove(artifact: Artifact)(implicit tx: S#Tx) {
      implicit val dtx = store.bridge(tx)
      if (!artifacts.remove(artifact)) throw new NoSuchElementException(s"Artifact $artifact was not found in the store")
      store.dispatch(ArtifactRemoved(store, loc, artifact))
    }

    def add(file: File)(implicit tx: S#Tx): Artifact = {
      implicit val dtx = store.bridge(tx)
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

      val key       = count()
      count()       = key + 1
      val path      = loop(Nil, folder)
      val artifact  = ArtifactImpl(key, path, name)
      store.dispatch(ArtifactAdded(store, loc, artifact))
      artifact
    }

    def write(out: DataOutput) {
      out.writeInt(key)
      count     .write(out)
      _directory.write(out)
      artifacts .write(out)
    }

    def dispose()(implicit tx: S#Tx) {
      implicit val dtx = store.bridge(tx)
      count     .dispose()
      _directory.dispose()
      artifacts .dispose()
    }
  }

  private final class ReadImpl[S <: evt.Sys[S], D <: evt.Sys[D]](protected val targets: evt.Targets[S],
                                                protected val _locKey: D#Var[Int], in: DataInput, access: S#Acc)
                                               (implicit tx0: S#Tx, bridge: S#Tx => D#Tx)
    extends Impl[S, D] {

    protected val _locations  = SkipList.Map.read[S, Int, LocationImpl[S, D]](in, access)
  }

  private final class NewImpl[S <: evt.Sys[S], D <: evt.Sys[D]](protected val targets: evt.Targets[D],
                                               protected val _locKey: D#Var[Int])
                                              (implicit tx0: S#Tx)
    extends Impl[S, D] {

    protected val _locations = {
      implicit val dtx = bridge(tx0)
      SkipList.Map.empty[D, Int, LocationImpl[S, D]]
    }
  }

  private sealed trait Impl[S <: evt.Sys[S], D <: evt.Sys[D]] extends ArtifactStore.Modifiable[S]
    with evti.StandaloneLike[S, Update[S], ArtifactStore[S]]
    with evti.Generator     [S, Update[S], ArtifactStore[S]]
    with evti.Root          [S, Update[S]] {
    store =>

    protected def _locKey: D#Var[Int]
    protected def _locations: SkipList.Map[D, Int, LocationImpl[S, D]]
    implicit def bridge: S#Tx => D#Tx

    implicit final protected def locationSerializer: Serializer[S#Tx, S#Acc, LocationImpl[S, D]] = LocationSerializer

    def changed: EventLike[S, Update[S], ArtifactStore[S]] = this

    private def getLocation(artifact: Artifact)(implicit tx: S#Tx): LocationImpl[S, D] = {
      implicit val dtx = bridge(tx)
      _locations.get(artifact.key).getOrElse(
        throw new NoSuchElementException(s"Artifact $artifact not found in store")
      )
    }

    def resolve(artifact: Artifact)(implicit tx: S#Tx): File = {
      val loc = getLocation(artifact)
      (loc.directory /: artifact.path)((res, sub) => new File(res, sub))
    }

    protected def reader: evt.Reader[S, ArtifactStore[S]] = ArtifactStoreImpl.serializer[S]

    def dispatch(update: Update[S])(implicit tx: S#Tx) { fire(update) }

    private object LocationSerializer extends Serializer[S#Tx, S#Acc, LocationImpl[S, D]] {
      def write(location: LocationImpl[S, D], out: DataOutput) { location.write(out) }
      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): LocationImpl[S, D] = {
        val dtx       = bridge(tx)
        val key       = in.readInt()
        val count     = dtx.readIntVar(id, in)
        val directory = dtx.readVar[File](id, in)
        val artifacts = LinkedList.Modifiable.read[D, Artifact](in, access)
        new LocationImpl[S, D](store, key, count, directory, artifacts)
      }
    }

    final def locations(implicit tx: S#Tx): data.Iterator[S#Tx, ArtifactStore.Location[S]] = _locations.valuesIterator

    def remove(artifact: Artifact)(implicit tx: S#Tx) {
      val loc = getLocation(artifact)
      loc.remove(artifact)
    }

    def addLocation(directory: File)(implicit tx: S#Tx): Location.Modifiable[S] = {
      implicit val dtx = bridge(tx)
      val key       = _locKey()
      _locKey()     = key + 1
      val count     = dtx.newIntVar(id, 0)
      val dirVar    = dtx.newVar[File](id, directory)
      val artifacts = LinkedList.Modifiable[D, Artifact]
      val loc       = new LocationImpl(store, key, count, dirVar, artifacts)
      _locations.add(key -> loc)
      fire(LocationAdded(store, loc))
      loc
    }

    def removeLocation(location: Location[S])(implicit tx: S#Tx) {
      implicit val dtx = bridge(tx)
      if (_locations.remove(location.key).isEmpty)
        throw new NoSuchElementException(s"Location $location was not part of this store")
      fire(LocationRemoved(store, location))
    }

    final protected def writeData(out: DataOutput) {
      out.writeShort(SER_VERSION)
      _locKey.write(out)
      _locations.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx) {
      implicit val dtx = bridge(tx)
      _locKey.dispose()
      _locations.dispose()
    }
  }
}