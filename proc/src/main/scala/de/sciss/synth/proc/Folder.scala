/*
 *  Folder.scala
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

import de.sciss.lucre.event.{InMemory, Sys}
import de.sciss.lucre.expr
import impl.{FolderElemImpl => Impl}
import de.sciss.serial
import de.sciss.synth.proc
import de.sciss.serial.{Serializer, DataInput}
import scala.collection.immutable.{IndexedSeq => Vec}
import language.existentials

object Folder {
  def apply[S <: Sys[S]](implicit tx: S#Tx): Folder[S] = expr.List.Modifiable[S, Obj[S], Obj.Update[S]]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Folder[S] =
    expr.List.Modifiable.read[S, Obj[S], Obj.Update[S]](in, access)

  // private[Folder] type _Update[S <: Sys[S]] = expr.List.Update[S, _Element[S], _Element.Update[S]]

  type Changes[S <: Sys[S]] = Vec[Change[S]]
  sealed trait Change[S <: Sys[S]] { def obj: Obj[S] }
  final case class Added  [S <: Sys[S]](idx: Int, obj: Obj[S]) extends Change[S]
  final case class Removed[S <: Sys[S]](idx: Int, obj: Obj[S]) extends Change[S]
  final case class Element[S <: Sys[S]](obj: Obj[S], update: Obj.Update[S]) extends Change[S]

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Folder[S]] =
    anySer.asInstanceOf[Serializer[S#Tx, S#Acc, Folder[S]]]

  private val anySer: Serializer[InMemory#Tx, InMemory#Acc, Folder[InMemory]] =
    expr.List.Modifiable.serializer[InMemory, Obj[InMemory], Obj.Update[InMemory]]
}

object FolderElem {
  type Peer[S <: Sys[S]] = Folder[S] // expr.List.Modifiable[S, proc.Obj[S], proc.Obj.Update[S]]

  // def empty[S <: Sys[S]]()(implicit tx: S#Tx): Folder[S] = Impl.empty[S]()

  def apply[S <: Sys[S]](peer: Peer[S])(implicit tx: S#Tx): FolderElem[S] =
    Impl(peer)

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): FolderElem[S] =
    Impl.read(in, access)

  object Obj {
    def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, FolderElem]] =
      if (obj.elem.isInstanceOf[FolderElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, FolderElem]])
      else None
  }

  implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, FolderElem[S]] =
    Impl.serializer[S]
}
trait FolderElem[S <: Sys[S]] extends Elem[S] {
  type Peer       = Folder[S]
  // type PeerUpdate = Folder.Update[S]
  type PeerUpdate = expr.List.Update[S, Obj[S], Obj.Update[S]] // XXX TODO: make alias Folder.Update
}