/*
 *  Attributes.scala
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

object Attributes {
  trait Modifiable[S <: evt.Sys[S]] extends Attributes[S] {
    def put(key: String, value: Attribute[S])(implicit tx: S#Tx): Unit
    def remove(key: String)(implicit tx: S#Tx): Boolean
  }
}

trait Attributes[S <: evt.Sys[S]] {
  // def apply[Attr <: Attribute[S]](key: String)(implicit tx: S#Tx, tag: ClassTag[Attr]): Option[Attr#Peer]
  def apply[Attr[~ <: evt.Sys[~]] <: Attribute[_]](key: String)
                                                  (implicit tx: S#Tx, tag: reflect.ClassTag[Attr[S]]): Option[Attr[S]#Peer]
  def get(key: String)(implicit tx: S#Tx): Option[Attribute[S]]
  def contains(key: String)(implicit tx: S#Tx): Boolean
  def keys(implicit tx: S#Tx): Set[String]
  def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, (String, Attribute[S])]
}
