package de.sciss
package synth
package proc

import lucre.data

object Elements {
  trait Modifiable[S <: Sys[S]] extends Elements[S] {
    def add(key: String)(implicit tx: S#Tx): Element[S]
    def remove(key: String)(implicit tx: S#Tx): Boolean
  }
}

trait Elements[S <: Sys[S]] {
  def get(key: String)(implicit tx: S#Tx): Option[Element[S]]
  def keys(implicit tx: S#Tx): Set[String]
  def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, (String, Element[S])]
}
