package de.sciss.synth.proc

import de.sciss.lucre.data
import de.sciss.lucre.event.{EventLike, Event}
import de.sciss.lucre.stm.{Sys, Obj}

import scala.language.higherKinds

object TransitoryAPI {
  implicit final class objAttrOps[S <: Sys[S]](val `this`: Obj[S]) extends AnyVal { me =>
    import me.{`this` => obj}

    def attr[Repr[~ <: Sys[~]] <: Obj[~]](key: String)(implicit tx: S#Tx): Option[Repr[S]] =
      tx.attrGet(obj, key)

    def attrGet(key: String)(implicit tx: S#Tx): Option[Obj[S]] =
      tx.attrGet[Obj](obj, key)

    // def attrContains(key: String)(implicit tx: S#Tx): Boolean = ...

    // def attrKeys(implicit tx: S#Tx): Set[String] = ...

    // def attrIterator(implicit tx: S#Tx): Iterator[(String, Obj[S])] = ...

    // def attrMod(implicit tx: S#Tx): Option[AttrMap.Modifiable[S]]

    def attrPut[Repr[~ <: Sys[~]] <: Obj[~]](key: String, value: Repr[S])(implicit tx: S#Tx): Unit =
      tx.attrPut(obj, key, value)

    def attrRemove(key: String)(implicit tx: S#Tx): Unit =
      tx.attrRemove(obj, key)

    def attrChanged: EventLike[S, AttrUpdate[S]] = ???
  }

  sealed trait AttrUpdate[S <: Sys[S]] {
    def key  : String
    def value: Obj[S]
  }

  final case class AttrAdded  [S <: Sys[S]](key: String, value: Obj[S]) extends AttrUpdate[S]
  final case class AttrRemoved[S <: Sys[S]](key: String, value: Obj[S]) extends AttrUpdate[S]
}
