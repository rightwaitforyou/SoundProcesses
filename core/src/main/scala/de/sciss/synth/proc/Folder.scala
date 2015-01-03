/*
 *  Folder.scala
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

import de.sciss.lucre.event.{InMemory, Sys}
import de.sciss.lucre.expr
import de.sciss.serial.{Serializer, DataInput}
import language.existentials

object Folder {
  def apply[S <: Sys[S]](implicit tx: S#Tx): Folder[S] = expr.List.Modifiable[S, Obj[S], Obj.Update[S]]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Folder[S] =
    expr.List.Modifiable.read[S, Obj[S], Obj.Update[S]](in, access)

  type Update[S <: Sys[S]] = expr.List.Update[S, Obj[S], Obj.Update[S]]

  //  type Changes[S <: Sys[S]] = Vec[Change[S]]
  //  sealed trait Change[S <: Sys[S]] { def obj: Obj[S] }
  //  final case class Added  [S <: Sys[S]](idx: Int, obj: Obj[S]) extends Change[S]
  //  final case class Removed[S <: Sys[S]](idx: Int, obj: Obj[S]) extends Change[S]
  //  final case class Element[S <: Sys[S]](obj: Obj[S], update: Obj.Update[S]) extends Change[S]

  type Change[S <: Sys[S]]  = expr.List.Change[S, Obj[S], Obj.Update[S]]
  type Added [S <: Sys[S]]  = expr.List.Added[S, Obj[S]]
  val Added                 = expr.List.Added
  type Removed[S <: Sys[S]] = expr.List.Removed[S, Obj[S]]
  val Removed               = expr.List.Removed
  type Element[S <: Sys[S]] = expr.List.Element[S, Obj[S], Obj.Update[S]]
  val Element               = expr.List.Element

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Folder[S]] =
    anySer.asInstanceOf[Serializer[S#Tx, S#Acc, Folder[S]]]

  private val anySer: Serializer[InMemory#Tx, InMemory#Acc, Folder[InMemory]] =
    expr.List.Modifiable.serializer[InMemory, Obj[InMemory], Obj.Update[InMemory]]
}