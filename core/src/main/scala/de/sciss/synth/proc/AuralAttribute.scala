package de.sciss.synth.proc

import de.sciss.lucre.stm.{Obj, Disposable, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import impl.{AuralAttributeImpl => Impl}

import scala.language.higherKinds

object AuralAttribute {
  trait Factory {
    def typeID: Int

    type Repr[~ <: Sys[~]] <: Obj[~]

    def apply[S <: SSys[S]](key: String, value: Repr[S])
                           (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S]
  }

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  def apply[S <: SSys[S]](key: String, value: Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
    Impl(key, value)
}
trait AuralAttribute[S <: Sys[S]] extends Disposable[S#Tx] {
  def preferredNumChannels(implicit tx: S#Tx): Int

  def accept()(implicit tx: S#Tx): Unit

  def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit
  def play   (timeRef: TimeRef, builder: NodeOwner[S], numChannels: Int)(implicit tx: S#Tx): Unit

  // def stop   ()(implicit tx: S#Tx): Unit
}
