/*
 *  Timeline.scala
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

import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.event.Sys
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc
import impl.{TimelineImpl => Impl}

object Timeline {
  final val SampleRate = 14112000.0 // lcm(88.2k, 96k); note: value is copied in AuralContextImpl

  final val typeID = 0x10006

  type Update[S <: Sys[S]]  = BiGroup.Update[S, proc.Obj[S], proc.Obj.Update[S]]
  val  Update               = BiGroup.Update

  def apply[S <: Sys[S]](implicit tx: S#Tx): Modifiable[S] = Impl[S]

  object Modifiable {
    // def apply[S <: Sys[S]](implicit tx: S#Tx): Modifiable[S] = Impl[S]

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Modifiable[S]] = Impl.modSerializer[S]

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Modifiable[S] = Impl.modRead(in, access)
  }
  trait Modifiable[S <: Sys[S]] extends Timeline[S] with BiGroup.Modifiable[S, proc.Obj[S], proc.Obj.Update[S]]

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Timeline[S]] = Impl.serializer[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Timeline[S] = Impl.read(in, access)

  implicit object Elem extends proc.Elem.Companion[Elem] {
    def typeID = Timeline.typeID

    def apply[S <: Sys[S]](peer: Timeline[S])(implicit tx: S#Tx): Timeline.Elem[S] =
      proc.impl.ElemImpl.Timeline(peer)

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Timeline.Elem[S]] =
      proc.impl.ElemImpl.Timeline.serializer[S]
  }
  trait Elem[S <: Sys[S]] extends proc.Elem[S] {
    type Peer       = Timeline[S]
    type PeerUpdate = Timeline.Update[S]
  }

  /** Convenient short-cut */

  object Obj {
    def unapply[S <: Sys[S]](obj: proc.Obj[S]): Option[Timeline.Obj[S]] =
      if (obj.elem.isInstanceOf[Timeline.Elem[S]]) Some(obj.asInstanceOf[Timeline.Obj[S]])
      else None
  }
  type Obj[S <: Sys[S]] = proc.Obj.T[S, Timeline.Elem]

  // ---- events ----
  val Added     = BiGroup.Added
  val Removed   = BiGroup.Removed
  val Moved     = BiGroup.ElementMoved
  val Element   = BiGroup.ElementMutated

  type Timed[S <: Sys[S]] = BiGroup.TimedElem[S, proc.Obj[S]]
}
trait Timeline[S <: Sys[S]] extends BiGroup[S, Obj[S], Obj.Update[S]] {
  override def modifiableOption: Option[Timeline.Modifiable[S]]

  // def sampleRate: Double
}