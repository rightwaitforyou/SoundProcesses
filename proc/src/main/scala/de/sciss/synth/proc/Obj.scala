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
import impl.{ObjectImpl => Impl}
import de.sciss.serial.{Serializer, DataInput}
import scala.language.higherKinds

object Obj {
  // ---- factory ----

  // yo fucking bullshit. `with Elem[S]` fixes the type inference. Thhhhhhhhhhhhhhank you.
  def apply[S <: Sys[S], E1 <: Elem[S]](elem: E1 with Elem[S])(implicit tx: S#Tx): Obj[S] { type E = E1 } = Impl(elem)

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] = Impl.read(in, access)

  def readT[S <: Sys[S], E1[~ <: Sys[~]] <: Elem[~]](in: DataInput, access: S#Acc)
                                                    (implicit tx: S#Tx,
                                                     peer: Serializer[S#Tx, S#Acc, E1[S]]): Obj[S] { type E = E1[S] } =
      Impl.readT[S, E1](in, access)

  // ---- serializer ----

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Obj[S]] = Impl.serializer[S]

  implicit def typedSerializer[S <: Sys[S], E1 <: Elem[S]](
    implicit peer: Serializer[S#Tx, S#Acc, E1]): evt.Serializer[S, Obj[S] { type E = E1 }] =
      Impl.typedSerializer[S, E1]

  // ---- updates ----

  /** An update is a sequence of changes */
  final case class Update[S <: Sys[S]](proc: Obj[S], changes: Vec[Change[S]])

  /** A change is either a state change, or a scan or a grapheme change */
  sealed trait Change[S <: Sys[S]]

  sealed trait AttrUpdate[S <: Sys[S]] extends Change[S] {
    def key: String

    def elem: Elem[S]
  }

  final case class AttrAdded  [S <: Sys[S]](key: String, elem: Elem[S]) extends AttrUpdate[S]

  final case class AttrRemoved[S <: Sys[S]](key: String, elem: Elem[S]) extends AttrUpdate[S]

  final case class AttrChange [S <: Sys[S]](key: String, elem: Elem[S], change: Any) extends AttrUpdate[S]

  final case class ElemChange [S <: Sys[S]](change: Any) extends Change[S]

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
}