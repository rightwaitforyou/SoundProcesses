///*
// *  ArtifactStoreImpl.scala
// *  (SoundProcesses)
// *
// *  Copyright (c) 2010-2013 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is free software; you can redistribute it and/or
// *  modify it under the terms of the GNU General Public License
// *  as published by the Free Software Foundation; either
// *  version 2, june 1991 of the License, or (at your option) any later version.
// *
// *  This software is distributed in the hope that it will be useful,
// *  but WITHOUT ANY WARRANTY; without even the implied warranty of
// *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// *  General Public License for more details.
// *
// *  You should have received a copy of the GNU General Public
// *  License (gpl.txt) along with this software; if not, write to the Free Software
// *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss
//package synth
//package proc
//package impl
//
//import java.io.File
//import lucre.{expr, data, event => evt, stm}
//import expr.LinkedList
//import serial.{DataOutput, Serializer, DataInput}
//import lucre.data.SkipList
//import evt.{impl => evti}
//import evt.EventLike
//import scala.annotation.tailrec
//import proc.ArtifactStore.{Location, Update, ArtifactAdded, ArtifactRemoved, LocationAdded, LocationRemoved, LocationMoved}
//import stm.IdentifierMap
//
//object ArtifactStoreImpl {
//  private final val SER_VERSION = 0x4153
//
//  def apply[S <: evt.Sys[S]](implicit tx: S#Tx): ArtifactStore.Modifiable[S] = {
//    val targets = evt.Targets[S]
//    new NewImpl[S](targets)
//  }
//
//  def serializer[S <: evt.Sys[S]]: Serializer[S#Tx, S#Acc, ArtifactStore[S]] with evt.Reader[S, ArtifactStore[S]] =
//    anySer.asInstanceOf[Ser[S]]
//
//  type LocSer[S <: evt.Sys[S]] = Serializer[S#Tx, S#Acc, ArtifactStore.Location[S]]
//
//  def locSerializer[S <: evt.Sys[S]]: LocSer[S] = ??? // anySer.asInstanceOf[ModSer[S]]
//
//  type ModSer[S <: evt.Sys[S]] = Serializer[S#Tx, S#Acc, ArtifactStore.Modifiable[S]] with evt.Reader[S, ArtifactStore.Modifiable[S]]
//
//  def modSerializer[S <: evt.Sys[S]]: ModSer[S] = anySer.asInstanceOf[ModSer[S]]
//
//  def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ArtifactStore[S] =
//    serializer[S].read(in, access)
//
//  def modRead[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ArtifactStore.Modifiable[S] =
//    modSerializer[S].read(in, access)
//
//  def locRead[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Location[S] =
//    locSerializer[S].read(in, access)
//
//  private[ArtifactStoreImpl] def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
//                                                      (implicit tx: S#Tx): Impl[S] = {
//    val cookie = in.readShort()
//    require(cookie == SER_VERSION, s"Version mismatch. Expected $SER_VERSION but found $cookie")
//    new ReadImpl(targets, in, access)
//  }
//
//  private final val anySer = new Ser[evt.InMemory]
//
//  private final class Ser[S <: evt.Sys[S]]
//    extends Serializer[S#Tx, S#Acc, ArtifactStore[S]] with evt.Reader[S, ArtifactStore[S]] {
//    def write(v: ArtifactStore[S], out: DataOutput) {
//      v.write(out)
//    }
//
//    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): ArtifactStore.Modifiable[S] = {
//      val targets = evt.Targets.read(in, access)
//      read(in, access, targets)
//    }
//
//    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])
//            (implicit tx: S#Tx): ArtifactStore.Modifiable[S] with evt.Node[S] =
//      ArtifactStoreImpl.read(in, access, targets)
//  }
//
////  private final class ModSer[S <: evt.Sys[S]]
////    extends Serializer[S#Tx, S#Acc, ArtifactStore.Modifiable[S]] with evt.Reader[S, ArtifactStore.Modifiable[S]] {
////    def write(v: ArtifactStore.Modifiable[S], out: DataOutput) {
////      v.write(out)
////    }
////
////    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): ArtifactStore.Modifiable[S] = {
////      val targets = evt.Targets.read(in, access)
////      read(in, access, targets)
////    }
////
////    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])
////            (implicit tx: S#Tx): ArtifactStore.Modifiable[S] with evt.Node[S] = ArtifactStoreImpl.modRead(in, access, targets)
////  }
//
//  private final class LocationImpl[S <: evt.Sys[S]](protected val targets: evt.Targets[S],
//                                                    _directory: S#Var[File],
//                                                    artifacts: LinkedList.Modifiable[S, Artifact[S], Unit])
//    extends Location.Modifiable[S] {
//    loc =>
//
//    def id = targets.id
//
//    def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Artifact[S]] = artifacts.iterator
//
//    def modifiableOption: Option[Location.Modifiable[S]] = Some(this)
//
//    def directory(implicit tx: S#Tx): File = _directory()
//    def directory_=(value: File)(implicit tx: S#Tx) {
//      val change = evt.Change(_directory(), value)
//      if (change.isSignificant) {
//        _directory() = value
//        ??? // store.dispatch(LocationMoved(store, loc, change))
//      }
//    }
//
//    // def createFile(): File = File.createTempFile("artifact", ".bin", directory())
//
//    def remove(artifact: Artifact[S])(implicit tx: S#Tx) {
//      if (!artifacts.remove(artifact)) throw new NoSuchElementException(s"Artifact $artifact was not found in the store")
//      ??? // store.dispatch(ArtifactRemoved(store, loc, artifact))
//    }
//
//    def add(file: File)(implicit tx: S#Tx): Artifact[S] = {
//      val can     = file.getCanonicalFile
//      val name    = can.getName
//      val base    = _directory().getCanonicalFile
//      val folder  = can.getParentFile
//
//      @tailrec def loop(res: List[String], left: File): List[String] = {
//        if (left == null)
//          throw new IllegalArgumentException(s"File $file is not inside artifact store's base directory $base")
//
//        if (left == base) res
//        else {
//          val last  = left.getName
//          val init  = left.getParentFile
//          loop(last :: res, init)
//        }
//      }
//
//      val path      = loop(Nil, folder)
//      val artifact  = ArtifactImpl(id, path, name)
//      ??? // store.dispatch(ArtifactAdded(store, loc, artifact))
//      artifact
//    }
//
//    def write(out: DataOutput) {
//      targets.write(out)
//      writeData(out)
//    }
//
//    protected def writeData(out: DataOutput) {
//      _directory.write(out)
//      artifacts .write(out)
//    }
//
//    def dispose()(implicit tx: S#Tx) {
//      targets.dispose()
//      disposeData()
//    }
//
//    protected def disposeData()(implicit tx: S#Tx) {
//      _directory.dispose()
//      artifacts .dispose()
//    }
//  }
//
//  private final class ReadImpl[S <: evt.Sys[S]](protected val targets: evt.Targets[S],
//                                                in: DataInput, access: S#Acc)(implicit tx0: S#Tx)
//    extends Impl[S] {
//
//    // protected val _locations  = SkipList.Map.read[S, Int, LocationImpl[S]](in, access)
//    protected val _locations    = LinkedList.Modifiable.read[S, Location[S]](in, access)
//    protected val _locationMap  = tx0.readDurableIDMap[LocationImpl[S]](in)
//  }
//
//  private final class NewImpl[S <: evt.Sys[S]](protected val targets: evt.Targets[S])(implicit tx0: S#Tx)
//    extends Impl[S] {
//
//    // protected val _locations    = SkipList.Map.empty[S, Int, LocationImpl[S]]
//    protected val _locations    = LinkedList.Modifiable[S, Location[S]]
//    protected val _locationMap  = tx0.newDurableIDMap[LocationImpl[S]]
//  }
//
//  private sealed trait Impl[S <: evt.Sys[S]] extends ArtifactStore.Modifiable[S]
//    with evti.StandaloneLike[S, Update[S], ArtifactStore[S]]
//    with evti.Generator     [S, Update[S], ArtifactStore[S]]
//    with evti.Root          [S, Update[S]] {
//    store =>
//
//    // protected def _locations: SkipList.Map[S, Int, LocationImpl[S]]
//    protected def _locations: LinkedList.Modifiable[S, Location[S], Unit]
//    protected def _locationMap: IdentifierMap[S#ID, S#Tx, LocationImpl[S]]
//
//    implicit final protected def locationSerializer: Serializer[S#Tx, S#Acc, LocationImpl[S]] = LocationSerializer
//
//    final def changed: EventLike[S, Update[S], ArtifactStore[S]] = this
//
//    final def modifiableOption: Option[ArtifactStore.Modifiable[S]] = Some(this)
//
//    private def getLocation(artifact: Artifact[S])(implicit tx: S#Tx): LocationImpl[S] =
//      _locationMap.get(artifact.location).getOrElse(
//        throw new NoSuchElementException(s"Artifact $artifact not found in store")
//      )
//
//    final def resolve(artifact: Artifact[S])(implicit tx: S#Tx): File = {
//      val loc = getLocation(artifact)
//      (loc.directory /: artifact.path)((res, sub) => new File(res, sub))
//    }
//
//    final protected def reader: evt.Reader[S, ArtifactStore[S]] = ArtifactStoreImpl.serializer[S]
//
//    final def dispatch(update: Update[S])(implicit tx: S#Tx) { fire(update) }
//
//    private object LocationSerializer extends Serializer[S#Tx, S#Acc, LocationImpl[S]] {
//      def write(location: LocationImpl[S], out: DataOutput) { location.write(out) }
//      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): LocationImpl[S] = {
//        val targets   = evt.Targets.read(in, access)
//        val directory = tx.readVar[File](id, in)
//        val artifacts = LinkedList.Modifiable.read[S, Artifact[S]](in, access)
//        new LocationImpl(targets, directory, artifacts)
//      }
//    }
//
//    final def locations(implicit tx: S#Tx): data.Iterator[S#Tx, ArtifactStore.Location[S]] = _locations.iterator
//
//    final def remove(artifact: Artifact[S])(implicit tx: S#Tx) {
//      val loc = getLocation(artifact)
//      loc.remove(artifact)
//    }
//
//    final def addLocation(directory: File)(implicit tx: S#Tx): Location.Modifiable[S] = {
//      val targets   = evt.Targets[S]
//      val dirVar    = tx.newVar[File](id, directory)
//      val artifacts = LinkedList.Modifiable[S, Artifact[S]]
//      val loc       = new LocationImpl(targets, dirVar, artifacts)
//      _locations.addLast(loc)
//      _locationMap.put(loc.id, loc)
//      fire(LocationAdded(store, loc))
//      loc
//    }
//
//    final def removeLocation(location: Location[S])(implicit tx: S#Tx) {
//      if (!_locations.remove(location))
//        throw new NoSuchElementException(s"Location $location was not part of this store")
//      _locationMap.remove(location.id)
//      location.dispose()
//      fire(LocationRemoved(store, location))
//    }
//
//    final protected def writeData(out: DataOutput) {
//      out.writeShort(SER_VERSION)
//      _locations.write(out)
//      _locationMap.write(out)
//    }
//
//    final protected def disposeData()(implicit tx: S#Tx) {
//      _locations.dispose()
//      _locationMap.dispose()
//    }
//  }
//}