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
import de.sciss.serial.DataInput

object Object {
  // ---- factory ----

  def apply[S <: Sys[S]](elem: Elem[S])(implicit tx: S#Tx): Object[S] = Impl(elem)

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Object[S] = Impl.read(in, access)

  // ---- serializer ----

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Object[S]] = Impl.serializer[S]

  // ---- updates ----

  /** An update is a sequence of changes */
  final case class Update[S <: Sys[S]](proc: Object[S], changes: Vec[Change[S]])

  /** A change is either a state change, or a scan or a grapheme change */
  sealed trait Change[S <: Sys[S]]

  sealed trait AttrUpdate[S <: Sys[S]] extends Change[S] {
    def key  : String
    def value: Elem[S]
  }

  final case class AttrAdded  [S <: Sys[S]](key: String, value: Elem[S]) extends AttrUpdate[S]
  final case class AttrRemoved[S <: Sys[S]](key: String, value: Elem[S]) extends AttrUpdate[S]
  final case class AttrChange [S <: Sys[S]](key: String, value: Elem[S], change: Any) extends AttrUpdate[S]

  final case class ElemChange[S <: Sys[S]](change: Any) extends Change[S]
}
/** Objects are attributed elements.
  * If you find a better name than `Object`, let me know! No fizz-buzz though, must be con- and precise.
  *
  * Ideas: `ElemAssoc`, `ElemMap`, `Token`, `ElemAttr`, `Item`, `Entry`, `Record`, `LabelledElem`, `Entity`,
  * `Image`, `Figure`, `DecoratedElem`, `Concept`
  */
trait Object[S <: Sys[S]] extends evt.Publisher[S, Object.Update[S]] with evt.Node[S] {
  // type E <: Elem[S]

  def attributes: AttrMap.Modifiable[S]

  def element: Elem[S] // E
}