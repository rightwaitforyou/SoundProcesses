package de.sciss.synth.proc

import de.sciss.lucre.data
import de.sciss.lucre.stm.{Sys, Obj}

object TransitoryAPI {
  implicit final class objAttrOps[S <: Sys[S]](val `this`: Obj[S]) extends AnyVal {
    def attr[A <: Obj[S]](key: String)(implicit tx: S#Tx): Option[A] = ???

    def attrGet(key: String)(implicit tx: S#Tx): Option[Obj[S]] = ???

    def attrContains(key: String)(implicit tx: S#Tx): Boolean = ???

    def attrKeys(implicit tx: S#Tx): Set[String] = ???

    def attrIterator(implicit tx: S#Tx): data.Iterator[S#Tx, (String, Obj[S])] = ???

    // def attrMod(implicit tx: S#Tx): Option[AttrMap.Modifiable[S]]

    def attrPut   (key: String, value: Obj[S])(implicit tx: S#Tx): Unit     = ???
    def attrRemove(key: String               )(implicit tx: S#Tx): Boolean  = ???
  }
}
