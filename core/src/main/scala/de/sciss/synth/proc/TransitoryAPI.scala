//package de.sciss.synth.proc
//
//import de.sciss.lucre.event.EventLike
//import de.sciss.lucre.stm.{Obj, Sys}
//
//import scala.language.higherKinds
//import scala.reflect.ClassTag
//
//object TransitoryAPI {
//  implicit final class objAttrOps[S <: Sys[S]](val `this`: Obj[S]) extends AnyVal { me =>
//    import me.{`this` => obj}
//
//    def attr[Repr[~ <: Sys[~]] <: Obj[~]](key: String)(implicit tx: S#Tx, mf: ClassTag[Repr[S]]): Option[Repr[S]] = {
//      val opt = tx.attrMap(obj).get(key)
//      if (opt.isEmpty) None else {
//        // Any better idea how to do this most efficiently?
//        val repr = opt.get
//        val ok   = mf.runtimeClass.isAssignableFrom(repr.getClass)
//        if (ok) opt.asInstanceOf[Option[Repr[S]]] else None
//      }
//    }
//
//    def attrGet(key: String)(implicit tx: S#Tx): Option[Obj[S]] = tx.attrMap(obj).get(key)
//
//    // def attrContains(key: String)(implicit tx: S#Tx): Boolean = ...
//
//    // def attrKeys(implicit tx: S#Tx): Set[String] = ...
//
//    // def attrIterator(implicit tx: S#Tx): Iterator[(String, Obj[S])] = ...
//
//    // def attrMod(implicit tx: S#Tx): Option[AttrMap.Modifiable[S]]
//
//    def attrPut[Repr[~ <: Sys[~]] <: Obj[~]](key: String, value: Repr[S])(implicit tx: S#Tx): Unit =
//      tx.attrMap(obj).put(key, value)
//
//    def attrRemove(key: String)(implicit tx: S#Tx): Unit =
//      tx.attrMap(obj).remove(key)
//
//    def attrChanged(implicit tx: S#Tx): EventLike[S, AttrUpdate[S]] = tx.attrMap(obj).changed
//  }
//
//  type AttrUpdate[S <: Sys[S]] = Obj.AttrUpdate[S]
////  sealed trait AttrUpdate[S <: Sys[S]] {
////    def key  : String
////    def value: Obj[S]
////  }
//
////  final case class AttrAdded  [S <: Sys[S]](key: String, value: Obj[S]) extends AttrUpdate[S]
////  final case class AttrRemoved[S <: Sys[S]](key: String, value: Obj[S]) extends AttrUpdate[S]
//
//  val  AttrAdded                = Obj.AttrAdded
//  type AttrAdded  [S <: Sys[S]] = Obj.AttrAdded  [S]
//  val  AttrRemoved              = Obj.AttrRemoved
//  type AttrRemoved[S <: Sys[S]] = Obj.AttrRemoved[S]
//}
