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

import lucre.data
import language.higherKinds
import scala.reflect.ClassTag
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.event.Sys

object AttrMap {
  trait Modifiable[S <: Sys[S]] extends AttrMap[S] {
    def put(key: String, value: El[S])(implicit tx: S#Tx): Unit
    def remove(key: String)(implicit tx: S#Tx): Boolean
  }
}

trait AttrMap[S <: Sys[S]] {
  // def apply[Attr <: Attr[S]](key: String)(implicit tx: S#Tx, tag: ClassTag[Attr]): Option[Attr#Peer]

  //  def apply[A[~ <: Sys[~]] <: El[_]](key: String)(implicit tx: S#Tx,
  //                                                      tag: reflect.ClassTag[A[S]]): Option[A[S]#Peer]

  def apply[A[~ <: Sys[~]]](key: String)(implicit tx: S#Tx, tag: ClassTag[A[S]]): Option[A[S]]

  def expr[A](key: String)(implicit tx: S#Tx, tag: ClassTag[Expr[S, A]]): Option[Expr[S, A]] =
    apply[({type Ex[~ <: Sys[~]] = Expr[~, A]})#Ex](key)

  def get(key: String)(implicit tx: S#Tx): Option[El[S]]
  def contains(key: String)(implicit tx: S#Tx): Boolean
  def keys(implicit tx: S#Tx): Set[String]
  def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, (String, El[S])]
}
