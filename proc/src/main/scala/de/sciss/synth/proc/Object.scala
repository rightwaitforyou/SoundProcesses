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

object Object {
  // ---- serializer ----

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Object[S]] = ???

  // ---- updates ----

  /** An update is a sequence of changes */
  final case class Update[S <: Sys[S]](proc: Object[S], changes: Vec[Change[S]])

  /** A change is either a state change, or a scan or a grapheme change */
  sealed trait Change[S <: Sys[S]]

  final case class AttrChange[S <: Sys[S]](key: String, value: Elem[S], change: Any)
    extends Change[S] {
    override def toString = s"AttrChange($key, $value, $change)"
  }

  final case class ElemChange[S <: Sys[S]](change: Any) extends Change[S]
}
/** Objects are attributed elements.
  * If you find a better name than `Object`, let me know! No fizz-buzz though, must be con- and precise.
  *
  * Ideas: `ElemAssoc`, `ElemMap`, `Token`, `ElemAttr`, `Item`, `Entry`, `Record`, `LabelledElem`, `Entity`,
  * `Image`, `Figure`, `DecoratedElem`, `Concept`
  */
trait Object[S <: Sys[S]] extends evt.Publisher[S, Object.Update[S]] with evt.Node[S] {
  type E <: Elem[S]

  def attributes: AttrMap.Modifiable[S]

  def element: E
}