/*
 *  AttrMap.scala
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

package de.sciss
package synth
package proc

import de.sciss.lucre.data
import de.sciss.lucre.event.Sys

import scala.language.higherKinds

object AttrMap {
  trait Modifiable[S <: Sys[S]] extends AttrMap[S] {
    def put   (key: String, value: Obj[S])(implicit tx: S#Tx): Unit
    def remove(key: String               )(implicit tx: S#Tx): Boolean
  }
}

trait AttrMap[S <: Sys[S]] {
  // def apply[Attr <: Attr[S]](key: String)(implicit tx: S#Tx, tag: ClassTag[Attr]): Option[Attr#Peer]

  //  def apply[A[~ <: Sys[~]] <: El[_]](key: String)(implicit tx: S#Tx,
  //                                                      tag: reflect.ClassTag[A[S]]): Option[A[S]#Peer]

  /** Tries to look up a value of a given peer type.
    *
    * @param key  the map key
    * @tparam E   the elem type, e.g. `Proc.Elem` for an `Obj.T[S, Proc.Elem]`
    * @return the unwrapped peer value, if an entry for the key exists and the value has the expected type
    */
  def apply[E[~ <: Sys[~]] <: Elem[~]](key: String)(implicit tx: S#Tx, companion: Elem.Companion[E]): Option[E[S]#Peer]

  //  def expr[A](key: String)(implicit tx: S#Tx, tag: ClassTag[Expr[S, A]]): Option[Expr[S, A]] =
  //    apply[({type Ex[~ <: Sys[~]] = Expr[~, A]})#Ex](key)

  // def expr[A](key: String)(implicit tx: S#Tx, tpe: ExprType[A]): Option[Expr[S, A]]

  def get     (key: String)(implicit tx: S#Tx): Option[Obj[S]]
  def getElem (key: String)(implicit tx: S#Tx): Option[Elem[S]]
  def contains(key: String)(implicit tx: S#Tx): Boolean

  def keys    (implicit tx: S#Tx): Set[String]
  def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, (String, Obj[S])]
}