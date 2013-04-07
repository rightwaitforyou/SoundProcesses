/*
 *  ProcGroup.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.{bitemp, expr, event => evt}
import bitemp.BiGroup
import expr.Type
import de.sciss.synth.expr.SpanLikes
import evt.EventLike
import de.sciss.span.SpanLike
import de.sciss.serial.{Serializer, DataInput}

object ProcGroup {
  type Update[S <: evt.Sys[S]] = BiGroup.Update[S, Proc[S], Proc.Update[S]]

  type Modifiable[S <: evt.Sys[S]] = BiGroup.Modifiable[S, Proc[S], Proc.Update[S]]

  private implicit val spanType: Type[SpanLike] = SpanLikes

  private def eventView[S <: evt.Sys[S]](proc: Proc[S]): EventLike[S, Proc.Update[S], Proc[S]] = proc.changed

  object Modifiable {
    def serializer[S <: evt.Sys[S]]: Serializer[S#Tx, S#Acc, ProcGroup.Modifiable[S]] = {
      BiGroup.Modifiable.serializer[S, Proc[S], Proc.Update[S]](eventView)
    }

    def apply[S <: evt.Sys[S]](implicit tx: S#Tx): ProcGroup.Modifiable[S] =
      BiGroup.Modifiable[S, Proc[S], Proc.Update[S]](eventView)

    def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ProcGroup.Modifiable[S] =
      BiGroup.Modifiable.read[S, Proc[S], Proc.Update[S]](in, access, eventView)
  }

  def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ProcGroup[S] =
    BiGroup.Modifiable.read[S, Proc[S], Proc.Update[S]](in, access, eventView)

  implicit def serializer[S <: evt.Sys[S]]: Serializer[S#Tx, S#Acc, ProcGroup[S]] = {
    BiGroup.serializer[S, Proc[S], Proc.Update[S]](eventView)
  }
}
