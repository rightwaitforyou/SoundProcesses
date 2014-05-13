/*
 *  ProcGroup.scala
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

import de.sciss.lucre.{bitemp, event => evt}
import bitemp.BiGroup
import evt.{EventLike, Sys}
import de.sciss.serial.{Serializer, DataInput}
import de.sciss.synth.proc

object ProcGroup {
  type Update    [S <: Sys[S]] = BiGroup.Update    [S, Obj.T[S, Proc.Elem], Obj.UpdateT[S, Proc.Elem[S]]]
  type Modifiable[S <: Sys[S]] = BiGroup.Modifiable[S, Obj.T[S, Proc.Elem], Obj.UpdateT[S, Proc.Elem[S]]]

  private def eventView[S <: Sys[S]](proc: Obj.T[S, Proc.Elem]): EventLike[S, Obj.UpdateT[S, Proc.Elem[S]]] =
    proc.changed

  object Modifiable {
    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ProcGroup.Modifiable[S]] =
      BiGroup.Modifiable.serializer[S, Obj.T[S, Proc.Elem], Obj.UpdateT[S, Proc.Elem[S]]](eventView)

    def apply[S <: Sys[S]](implicit tx: S#Tx): ProcGroup.Modifiable[S] =
      BiGroup.Modifiable[S, Obj.T[S, Proc.Elem], Obj.UpdateT[S, Proc.Elem[S]]](eventView)

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ProcGroup.Modifiable[S] =
      BiGroup.Modifiable.read[S, Obj.T[S, Proc.Elem], Obj.UpdateT[S, Proc.Elem[S]]](in, access, eventView)
  }


  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ProcGroup[S] =
    BiGroup.Modifiable.read[S, Obj.T[S, Proc.Elem], Obj.UpdateT[S, Proc.Elem[S]]](in, access, eventView)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ProcGroup[S]] =
    BiGroup.serializer[S, Obj.T[S, Proc.Elem], Obj.UpdateT[S, Proc.Elem[S]]](eventView)
}

// ---- Elem ----

object ProcGroupElem {
  def apply[S <: Sys[S]](peer: ProcGroup[S])(implicit tx: S#Tx): ProcGroupElem[S] =
    proc.impl.ElemImpl.ProcGroup(peer)

  object Obj {
    def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, ProcGroupElem]] =
      if (obj.elem.isInstanceOf[ProcGroupElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, ProcGroupElem]])
      else None
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ProcGroupElem[S]] =
    proc.impl.ElemImpl.ProcGroup.serializer[S]
}
trait ProcGroupElem[S <: Sys[S]] extends proc.Elem[S] {
  type Peer       = ProcGroup[S]
  type PeerUpdate = ProcGroup.Update[S]
}
