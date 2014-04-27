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

object ProcGroup {
  type Update    [S <: Sys[S]] = BiGroup.Update    [S, Obj.T[S, ProcElem], Obj.UpdateT[S, ProcElem[S]]]
  type Modifiable[S <: Sys[S]] = BiGroup.Modifiable[S, Obj.T[S, ProcElem], Obj.UpdateT[S, ProcElem[S]]]

  private def eventView[S <: Sys[S]](proc: Obj.T[S, ProcElem]): EventLike[S, Obj.UpdateT[S, ProcElem[S]]] =
    proc.changed

  object Modifiable {
    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ProcGroup.Modifiable[S]] =
      BiGroup.Modifiable.serializer[S, Obj.T[S, ProcElem], Obj.UpdateT[S, ProcElem[S]]](eventView)

    def apply[S <: Sys[S]](implicit tx: S#Tx): ProcGroup.Modifiable[S] =
      BiGroup.Modifiable[S, Obj.T[S, ProcElem], Obj.UpdateT[S, ProcElem[S]]](eventView)

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ProcGroup.Modifiable[S] =
      BiGroup.Modifiable.read[S, Obj.T[S, ProcElem], Obj.UpdateT[S, ProcElem[S]]](in, access, eventView)
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ProcGroup[S] =
    BiGroup.Modifiable.read[S, Obj.T[S, ProcElem], Obj.UpdateT[S, ProcElem[S]]](in, access, eventView)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ProcGroup[S]] =
    BiGroup.serializer[S, Obj.T[S, ProcElem], Obj.UpdateT[S, ProcElem[S]]](eventView)
}