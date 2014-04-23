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

import lucre.{data, event => evt}
import language.higherKinds

object AttrMap {
  trait Modifiable[S <: evt.Sys[S]] extends AttrMap[S] {
    def put(key: String, value: Elem[S])(implicit tx: S#Tx): Unit
    def remove(key: String)(implicit tx: S#Tx): Boolean
  }
}

trait AttrMap[S <: evt.Sys[S]] {
  // def apply[Attr <: Attr[S]](key: String)(implicit tx: S#Tx, tag: ClassTag[Attr]): Option[Attr#Peer]
  def apply[A[~ <: evt.Sys[~]] <: Elem[_]](key: String)
                                          (implicit tx: S#Tx, tag: reflect.ClassTag[A[S]]): Option[A[S]#Peer]
  def get(key: String)(implicit tx: S#Tx): Option[Elem[S]]
  def contains(key: String)(implicit tx: S#Tx): Boolean
  def keys(implicit tx: S#Tx): Set[String]
  def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, (String, Elem[S])]
}
