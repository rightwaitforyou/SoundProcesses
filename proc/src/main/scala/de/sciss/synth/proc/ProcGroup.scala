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

import de.sciss.lucre.{bitemp, expr, event => evt}
import bitemp.BiGroup
import expr.Type
import evt.EventLike
import de.sciss.span.SpanLike
import de.sciss.serial.{Serializer, DataInput}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.synth.expr.SpanLikes

object ProcGroup {
  type Update[S <: Sys[S]] = BiGroup.Update[S, Proc[S], Proc.Update[S]]

  type Modifiable[S <: Sys[S]] = BiGroup.Modifiable[S, Proc[S], Proc.Update[S]]

  private implicit val spanType: Type[SpanLike] = SpanLikes

  private def eventView[S <: Sys[S]](proc: Proc[S]): EventLike[S, Proc.Update[S]] = proc.changed

  object Modifiable {
    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ProcGroup.Modifiable[S]] =
      BiGroup.Modifiable.serializer[S, Proc[S], Proc.Update[S]](eventView)

    def apply[S <: Sys[S]](implicit tx: S#Tx): ProcGroup.Modifiable[S] =
      BiGroup.Modifiable[S, Proc[S], Proc.Update[S]](eventView)

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ProcGroup.Modifiable[S] =
      BiGroup.Modifiable.read[S, Proc[S], Proc.Update[S]](in, access, eventView)
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ProcGroup[S] =
    BiGroup.Modifiable.read[S, Proc[S], Proc.Update[S]](in, access, eventView)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ProcGroup[S]] = {
    BiGroup.serializer[S, Proc[S], Proc.Update[S]](eventView)
  }
}
