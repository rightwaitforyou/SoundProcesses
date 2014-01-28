/*
 *  ArtifactImpl.scala
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
package impl

import de.sciss.serial.{DataOutput, DataInput}
import lucre.{event => evt, data, expr}
import java.io.File
import expr.List
import proc.Artifact.Location.Update
import evt.{EventLike, NodeSerializer}
import de.sciss.synth.proc.Artifact.Modifiable
import de.sciss.model.Change

object ArtifactImpl {
  import Artifact.{Child, Location}

  private final val SER_VERSION = 0x4172

  // ---- artifact ----

  def apply[S <: evt.Sys[S]](location: Location[S], child: Child)(implicit tx: S#Tx): Artifact.Modifiable[S] = {
    val targets = evt.Targets[S]
    val _child  = tx.newVar(targets.id, child.path)
    new Impl(targets, location, _child)
  }

  def copy[S <: evt.Sys[S]](from: Artifact[S])(implicit tx: S#Tx): Artifact.Modifiable[S] = {
    apply(from.location, from.child)
  }

  def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Artifact[S] =
    serializer[S].read(in, access)

  def readMod[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Artifact.Modifiable[S] =
    modSerializer[S].read(in, access)

  def serializer   [S <: evt.Sys[S]]: NodeSerializer[S, Artifact           [S]] = anySer  .asInstanceOf[Ser[S]]
  def modSerializer[S <: evt.Sys[S]]: NodeSerializer[S, Artifact.Modifiable[S]] = anyModSer.asInstanceOf[ModSer[S]]

  private val anySer    = new Ser   [evt.InMemory]
  private val anyModSer = new ModSer[evt.InMemory]

  private final class Ser[S <: evt.Sys[S]] extends NodeSerializer[S, Artifact[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Artifact[S] with evt.Node[S] = {
      val cookie    = in.readShort()
      require(cookie == SER_VERSION, s"Version mismatch. Expected $SER_VERSION but found $cookie")
      val location  = readLocation(in, access)
      val _child    = tx.readVar[String](targets.id, in)
      new Impl(targets, location, _child)
    }
  }

  private final class ModSer[S <: evt.Sys[S]] extends NodeSerializer[S, Artifact.Modifiable[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Artifact.Modifiable[S] with evt.Node[S] = {
      val cookie    = in.readShort()
      require(cookie == SER_VERSION, s"Version mismatch. Expected $SER_VERSION but found $cookie")
      val location  = readLocation(in, access)
      val _child    = tx.readVar[String](targets.id, in)
      new Impl(targets, location, _child)
    }
  }

  // ---- location ----

  def newLocation[S <: evt.Sys[S]](init: File)(implicit tx: S#Tx): Location.Modifiable[S] = {
    val targets   = evt.Targets[S]
    val directory = tx.newVar(targets.id, init)
    val artifacts = List.Modifiable[S, Artifact[S]]
    new LocationImpl(targets, directory, artifacts)
  }

  def readLocation[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Location[S] =
    locationSerializer[S].read(in, access)

  def readModLocation[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Location.Modifiable[S] =
    modLocationSerializer[S].read(in, access)

  def locationSerializer[S <: evt.Sys[S]]: NodeSerializer[S, Location[S]] = anyLocSer.asInstanceOf[LocSer[S]]

  def modLocationSerializer[S <: evt.Sys[S]]: NodeSerializer[S, Location.Modifiable[S]] =
    anyModLocSer.asInstanceOf[ModLocSer[S]]

  private val anyLocSer     = new LocSer[evt.InMemory]
  private val anyModLocSer  = new ModLocSer[evt.InMemory]

  private def readLoc[S <: evt.Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
          (implicit tx: S#Tx): Location.Modifiable[S] with evt.Node[S] = {
    val cookie    = in.readShort()
    require(cookie == SER_VERSION, s"Version mismatch. Expected $SER_VERSION but found $cookie")
    val directory = tx.readVar[File](targets.id, in)
    val artifacts = List.Modifiable.read[S, Artifact[S]](in, access)
    new LocationImpl(targets, directory, artifacts)
  }

  private final class LocSer[S <: evt.Sys[S]] extends NodeSerializer[S, Location[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])
            (implicit tx: S#Tx): Location[S] with evt.Node[S] = readLoc(in, access,targets)
  }

  private final class ModLocSer[S <: evt.Sys[S]] extends NodeSerializer[S, Location.Modifiable[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])
            (implicit tx: S#Tx): Location.Modifiable[S] with evt.Node[S] =
      readLoc(in, access,targets)
  }

  private final class LocationImpl[S <: evt.Sys[S]](protected val targets: evt.Targets[S],
                                                    _directory: S#Var[File],
                                                    artifacts: List.Modifiable[S, Artifact[S], Unit])
    extends Location.Modifiable[S]
    with evt.impl.Generator     [S, Location.Update[S], Location[S]]
    with evt.impl.Root          [S, Location.Update[S]]
    with evt.impl.StandaloneLike[S, Location.Update[S], Location[S]] {
    loc =>

    def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Artifact[S]] = artifacts.iterator

    def modifiableOption: Option[Location.Modifiable[S]] = Some(this)

    def changed: EventLike[S, Update[S]] = this

    def directory(implicit tx: S#Tx): File = _directory()
    def directory_=(value: File)(implicit tx: S#Tx): Unit = {
      val change = Change(_directory(), value)
      if (change.isSignificant) {
        _directory() = value
        fire(Location.Moved(loc, change))
      }
    }

    // def createFile(): File = File.createTempFile("artifact", ".bin", directory())

    def remove(artifact: Artifact[S])(implicit tx: S#Tx): Unit = {
      val idx = artifacts.indexOf(artifact)
      if (!artifacts.remove(artifact)) throw new NoSuchElementException(s"Artifact $artifact was not found in the store")
      fire(Location.Removed(loc, idx, artifact))
    }

    def add(file: File)(implicit tx: S#Tx): Artifact.Modifiable[S] = {
      val base      = _directory()
      val child     = Artifact.relativize(base, file)
      val artifact  = ArtifactImpl.apply(loc, child)
      val idx       = artifacts.size
      artifacts.addLast(artifact)
      fire(Location.Added(loc, idx, artifact))
      artifact
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      _directory.write(out)
      artifacts .write(out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      _directory.dispose()
      artifacts .dispose()
    }

    protected def reader: evt.Reader[S, Location[S]] = locationSerializer[S]
  }

  private final class Impl[S <: evt.Sys[S]](protected val targets: evt.Targets[S],
                                            val location: Location[S], _child: S#Var[String])
    extends Artifact.Modifiable[S]
    with evt.impl.MappingGenerator[S, Change[File], Location.Update[S], Artifact[S]] {

    // override def toString = s"Artifact(${if (path.isEmpty) "" else path.mkString("", "/", "/")}$name)"
    override def toString() = s"Artifact@${hashCode().toHexString}"

    def modifiableOption: Option[Modifiable[S]] = Some(this)

    def child(implicit tx: S#Tx): Child = Child(_child())

    def child_=(value: Child)(implicit tx: S#Tx) {
      val oldP  = _child()
      val newP  = value.path
      if (oldP != newP) {
        val base    = location.directory
        _child()    = newP
        val change  = Change(new File(base, oldP), new File(base, newP))
        fire(change)
      }
    }

    protected def inputEvent: EventLike[S, Location.Update[S]] = location.changed

    protected def foldUpdate(generated: Option[Change[File]],
                             input: Update[S])(implicit tx: S#Tx): Option[Change[File]] =
      generated.orElse {
        input match {
          case Location.Moved(_, Change(oldBase, newBase)) =>
            val path    = _child()
            val change  = Change(new File(oldBase, path), new File(newBase, path))
            Some(change)
          case _ => None
        }
      }

    protected def reader: evt.Reader[S, Artifact[S]] = ArtifactImpl.serializer

    def value(implicit tx: S#Tx): Artifact.Value = {
      val base   = location.directory
      val child  = _child()
      new File(base, child)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      // location.dispose()
      _child.dispose()
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      location.write(out)
      _child  .write(out)
      //      if (path.isEmpty) out.writeShort(0) else {
      //        out.writeShort(path.size)
      //        path.foreach(out.writeUTF _)
      //      }
      //      out.writeUTF(name)
    }
  }
}