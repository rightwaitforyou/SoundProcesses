/*
 *  Object.scala
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

import de.sciss.lucre.{event => evt}
import evt.Sys
import scala.collection.immutable.{IndexedSeq => Vec}
import impl.{ObjImpl => Impl}
import de.sciss.serial.{Serializer, DataInput}
import scala.language.{existentials, higherKinds}

object Obj {
  // ---- factory ----

  // yo fucking bullshit. `with Elem[S]` fixes the type inference. Thhhhhhhhhhhhhhank you.
  def apply[S <: Sys[S], E1 <: Elem[S]](elem: E1 with Elem[S])(implicit tx: S#Tx): Obj[S] { type E = E1 } = Impl(elem)

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = Impl.read(in, access)

  def readT[S <: Sys[S], E1[~ <: Sys[~]] <: Elem[~]](in: DataInput, access: S#Acc)
                                                    (implicit tx: S#Tx,
                                                     peer: Serializer[S#Tx, S#Acc, E1[S]]): Obj[S] { type E = E1[S] } =
      Impl.readT[S, E1](in, access)

  //  def copy[S <: Sys[S], E1[~ <: Sys[~]] <: Elem[~]](in: Obj.T[S, E1])(implicit tx: S#Tx): Obj.T[S, E1] = {
  //    val res = apply[S, E1[S]](in.elem.mkCopy().asInstanceOf[E1[S]]) // XXX TODO damn
  //    val outAttr = res.attr
  //    in.attr.iterator.foreach { case (key, value) =>
  //      outAttr.put(key, Obj.copy(value))
  //    }
  //    res
  //  }

  def copy[S <: Sys[S]](in: Obj[S])(implicit tx: S#Tx): Obj[S] = {
    val res = apply(in.elem.mkCopy())
    val outAttr = res.attr
    in.attr.iterator.foreach { case (key, value) =>
      outAttr.put(key, Obj.copy(value))
    }
    res
  }

  // ---- serializer ----

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Obj[S]] = Impl.serializer[S]

  implicit def typedSerializer[S <: Sys[S], E1 <: Elem[S]](
    implicit peer: Serializer[S#Tx, S#Acc, E1]): evt.Serializer[S, Obj[S] { type E = E1 }] =
      Impl.typedSerializer[S, E1]

  // ---- updates ----

  /** An update is a sequence of changes */
  final case class UpdateT[S <: Sys[S], E1 <: Elem[S]](obj: Obj[S] { type E = E1 },
                                                       changes: Vec[Change[S, E1#PeerUpdate]])

  type Update[S <: Sys[S]] = UpdateT[S, _ <: Elem[S]] // (proc: Obj[S], changes: Vec[Change[S, Upd]])

  /** A change is either a element state change, or an attribute change */
  sealed trait Change[S <: Sys[S], +Upd]

  sealed trait AttrUpdate[S <: Sys[S]] extends Change[S, Nothing] {
    def key: String
    def elem: Obj[S]
  }

  final case class AttrAdded  [S <: Sys[S]](key: String, elem: Obj[S]) extends AttrUpdate[S]
  final case class AttrRemoved[S <: Sys[S]](key: String, elem: Obj[S]) extends AttrUpdate[S]

  // final case class AttrChange [S <: Sys[S]](key: String, elem: Obj[S], change: Obj.Update[S]) extends AttrUpdate[S]
  final case class AttrChange [S <: Sys[S]](key: String, elem: Obj[S], changes: Vec[Change[S, Any]])
    extends AttrUpdate[S]

  final case class ElemChange [S <: Sys[S], Upd](change: Upd) extends Change[S, Upd]

  type T[S <: Sys[S], E1[~ <: Sys[~]] <: Elem[~]] = Obj[S] { type E = E1[S] }

  //  def test[S <: Sys[S]](elem: Grapheme.Elem.Audio[S])(implicit tx: S#Tx): Unit = {
  //    val audio = AudioGraphemeElem(elem)
  //    Object(audio)
  //  }
}
/** Objects are attributed elements.
  * If you find a better name than `Object`, let me know! No fizz-buzz though, must be con- and precise.
  *
  * Ideas: `ElemAssoc`, `ElemMap`, `Token`, `ElemAttr`, `Item`, `Entry`, `Record`, `LabelledElem`, `Entity`,
  * `Image`, `Figure`, `DecoratedElem`, `Concept`
  */
trait Obj[S <: Sys[S]] extends evt.Publisher[S, Obj.Update[S]] with evt.Node[S] {
  type E <: Elem[S]

  def attr: AttrMap.Modifiable[S]

  def elem: E

  override def changed: evt.EventLike[S, Obj.UpdateT[S, E]]
}