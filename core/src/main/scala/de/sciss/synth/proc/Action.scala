/*
 *  Action.scala
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

import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.{TxnLike, Disposable}
import de.sciss.lucre.{stm, event => evt}
import de.sciss.serial.{DataInput, Serializer, Writable}
import de.sciss.synth.proc
import de.sciss.synth.proc.impl.{ActionImpl => Impl}

import scala.concurrent.Future
import scala.collection.immutable.{IndexedSeq => Vec}

object Action {
  final val typeID = 19

  final val attrSource = "action-source"

  def compile[S <: Sys[S]](source: Code.Action)
                          (implicit tx: S#Tx, cursor: stm.Cursor[S],
                           compiler: Code.Compiler): Future[stm.Source[S#Tx, Action[S]]] =
    Impl.compile(source)

  def empty[S <: Sys[S]](implicit tx: S#Tx): Action[S] = Impl.empty[S]

  def apply[S <: Sys[S]](name: String, jar: Array[Byte])(implicit tx: S#Tx): Action[S] =
    Impl.newConst(name, jar)

  def predef[S <: Sys[S]](id: String)(implicit tx: S#Tx): Action[S] = Impl.predef(id)

  def registerPredef(id: String, body: Body)(implicit tx: TxnLike): Unit = Impl.registerPredef(id, body)

  object Var {
    def apply[S <: Sys[S]](init: Action[S])(implicit tx: S#Tx): Var[S] = Impl.newVar(init)

    def unapply[S <: Sys[S]](a: Action[S]): Option[Var[S]] =
      a match {
        case x: Var[S] => Some(x)
        case _ => None
      }

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Var[S]] = Impl.varSerializer[S]
  }
  trait Var[S <: Sys[S]] extends Action[S] with stm.Var[S#Tx, Action[S]]

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Action[S]] = Impl.serializer[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Action[S] = serializer[S].read(in, access)

  // ---- body ----

  trait Body {
    def apply[S <: Sys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit
  }

  object Universe {
    def apply[S <: Sys[S]](self: Action.Obj[S], workspace: WorkspaceHandle[S],
                           values: Vec[Float] = Vector.empty): Universe[S] =
      new Impl.UniverseImpl(self, workspace, values)
  }
  trait Universe[S <: Sys[S]] {
    /** The action object itself, most prominently giving access to
      * linked objects via its attributes.
      */
    def self: Action.Obj[S]

    /** Root folder of the workspace containing the action. */
    def root(implicit tx: S#Tx): Folder[S]

    /** A collection of sampled values if the action was invoked through
      * `graph.Reaction` (otherwise empty).
      */
    def values: Vec[Float]
  }

  // ---- element ----

  implicit object Elem extends proc.Elem.Companion[Elem] {
    def typeID = Action.typeID

    def apply[S <: Sys[S]](peer: Action[S])(implicit tx: S#Tx): Action.Elem[S] = Impl.ElemImpl(peer)

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Action.Elem[S]] = Impl.ElemImpl.serializer
  }

  trait Elem[S <: Sys[S]] extends proc.Elem[S] {
    type Peer         = Action[S]
    type PeerUpdate   = Unit

    def mkCopy()(implicit tx: S#Tx): Elem[S]
  }

  object Obj {
    def unapply[S <: Sys[S]](obj: proc.Obj[S]): Option[Action.Obj[S]] =
      if (obj.elem.isInstanceOf[Action.Elem[S]]) Some(obj.asInstanceOf[Action.Obj[S]])
      else None
  }
  type Obj[S <: Sys[S]] = proc.Obj.T[S, Action.Elem]
}
trait Action[S <: Sys[S]] extends Writable with Disposable[S#Tx] with evt.Publisher[S, Unit] {
  def execute(universe: Action.Universe[S])(implicit tx: S#Tx): Unit
}