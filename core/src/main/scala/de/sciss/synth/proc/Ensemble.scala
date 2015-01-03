/*
 *  Ensemble.scala
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

import de.sciss.lucre.event.{Publisher, Sys}
import de.sciss.lucre.expr
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.Disposable
import de.sciss.model
import de.sciss.serial.{DataInput, Serializer, Writable}
import de.sciss.synth.proc
import impl.{EnsembleImpl => Impl}

object Ensemble {
  final val typeID = 0x10007

  def apply[S <: Sys[S]](folder: proc.Folder /* Elem.Obj */[S], offset: Expr[S, Long], playing: Expr[S, Boolean])
                        (implicit tx: S#Tx): Ensemble[S] = Impl(folder, offset, playing)

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Ensemble[S] =
    Impl.serializer[S].read(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Ensemble[S]] = Impl.serializer[S]

  final case class Update[S <: Sys[S]](ensemble: Ensemble[S], changes: List[Change[S]])

  sealed trait Change[S]
  final case class Folder [S <: Sys[S]](peer: expr.List.Update[S, proc.Obj[S], proc.Obj.Update[S]] /* SCALAC BUG: proc.Folder.Update[S] */) extends Change[S]
  final case class Offset [S <: Sys[S]](peer: model.Change[Long   ]) extends Change[S]
  final case class Playing[S <: Sys[S]](peer: model.Change[Boolean]) extends Change[S]

  // ---- Elem ----

  implicit object Elem extends proc.Elem.Companion[Elem] {
    def typeID = Ensemble.typeID

    def apply[S <: Sys[S]](peer: Ensemble[S])(implicit tx: S#Tx): Ensemble.Elem[S] = Impl.ElemImpl(peer)

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Ensemble.Elem[S]] = Impl.ElemImpl.serializer[S]
  }
  trait Elem[S <: Sys[S]] extends proc.Elem[S] {
    type Peer       = Ensemble[S]
    type PeerUpdate = Ensemble.Update[S]
  }

  /** Convenient short-cut */

  object Obj {
    def unapply[S <: Sys[S]](obj: proc.Obj[S]): Option[Ensemble.Obj[S]] =
      if (obj.elem.isInstanceOf[Ensemble.Elem[S]]) Some(obj.asInstanceOf[Ensemble.Obj[S]])
      else None
  }
  type Obj[S <: Sys[S]] = proc.Obj.T[S, Ensemble.Elem]
}

/** An `Ensemble` is sort of a persistent transport model.
  * It maintains a list of transported objects through the
  * `folder` member. The `playing` expression determines
  * when the transport is playing or not. Upon a transition
  * from stopped to playing, the `offset` member determines
  * the "seek" position.
  */
trait Ensemble[S <: Sys[S]] extends Writable with Disposable[S#Tx] with Publisher[S, Ensemble.Update[S]] {
  def folder (implicit tx: S#Tx): Folder /* Elem.Obj */ [S]
  def offset (implicit tx: S#Tx): Expr[S, Long]
  def playing(implicit tx: S#Tx): Expr[S, Boolean]
}
