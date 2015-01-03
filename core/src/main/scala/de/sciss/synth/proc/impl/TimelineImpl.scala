/*
 *  TimelineImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.bitemp.impl.BiGroupImpl
import de.sciss.lucre.{event => evt}
import de.sciss.lucre.event.{EventLike, Sys}
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc.Obj.Update

object TimelineImpl {
  def apply[S <: Sys[S]](implicit tx: S#Tx): Timeline.Modifiable[S] =
    new Impl[S](evt.Targets[S]) {
      val tree = newTree()
    }

  // ---- serialization ----

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Timeline[S]] =
    anySer.asInstanceOf[Ser[S]]

  implicit def modSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Timeline.Modifiable[S]] =
    anyModSer.asInstanceOf[ModSer[S]]

  // currently there is only modifiable instance
  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Timeline[S] = modRead(in, access)

  def modRead[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Timeline.Modifiable[S] = {
    val targets = evt.Targets.read[S](in, access)
    read(in, access, targets)
  }

  private def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                               (implicit tx: S#Tx): Timeline.Modifiable[S] =
    new Impl[S](targets) {
      val tree = readTree(in,access)
    }

  private val anySer    = new Ser   [evt.InMemory]
  private val anyModSer = new ModSer[evt.InMemory]

  private class Ser[S <: Sys[S]] extends evt.NodeSerializer[S, Timeline[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Timeline[S] = {
      TimelineImpl.read(in, access, targets)
    }
  }

  private class ModSer[S <: Sys[S]] extends evt.NodeSerializer[S, Timeline.Modifiable[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Timeline.Modifiable[S] = {
      TimelineImpl.read(in, access, targets)
    }
  }

  // ---- impl ----

  private abstract class Impl[S <: Sys[S]](protected val targets: evt.Targets[S])
    extends BiGroupImpl.Impl[S, Obj[S], Obj.Update[S]] with Timeline.Modifiable[S] {

    override def modifiableOption: Option[Timeline.Modifiable[S]] = Some(this)

    def eventView(obj: Obj[S]): EventLike[S, Update[S]] = obj.changed

    def elemSerializer: Serializer[S#Tx, S#Acc, Obj[S]] = Obj.serializer[S]

    override def toString() = s"Timeline${tree.id}"
  }
}