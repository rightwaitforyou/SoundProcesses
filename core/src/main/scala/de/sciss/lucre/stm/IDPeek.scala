package de.sciss.lucre.stm

import de.sciss.lucre.confluent

// XXX TODO - perhaps this should become public API?
object IDPeek {
  def apply[S <: Sys[S]](id: S#ID): Int = id match {
    case x: InMemoryLike .ID[_] => x.id
    case x: DurableLike  .ID[_] => x.id
    case x: confluent.Sys.ID[_] => x.base
    case _ => sys.error(s"Unsupported identifier $id")
  }
}
