package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm.Obj
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.AuralAttribute.Factory

object AuralAttributeImpl {
  private[this] val sync = new AnyRef

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.typeID
    if (map.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    map += tid -> f
  }

  def factories: Iterable[Factory] = map.values

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] = {
    val tid     = obj.tpe.typeID
    val factory = map.getOrElse(tid, throw new IllegalArgumentException(s"No AuralAttribute available for $obj"))
    factory(obj.asInstanceOf[factory.Repr[S]])
  }

  private[this] var map = scala.Predef.Map[Int, Factory](
    // AudioGrapheme   .typeID -> AudioGrapheme,
    Folder          .typeID -> ???,
    Proc            .typeID -> ???,
    Timeline        .typeID -> ???,
    Ensemble        .typeID -> ???
  )
}
