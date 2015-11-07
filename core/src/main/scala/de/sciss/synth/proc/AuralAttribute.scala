package de.sciss.synth.proc

import de.sciss.lucre.stm.{Obj, Disposable, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import impl.{AuralAttributeImpl => Impl}

import scala.language.higherKinds

object AuralAttribute {
  trait Factory {
    def typeID: Int

    type Repr[~ <: Sys[~]] <: Obj[~]

    def apply[S <: SSys[S]](obj: Repr[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S]
  }

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  def apply[S <: SSys[S]](value: Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
    Impl(value)
}
trait AuralAttribute[S <: Sys[S]] extends Disposable[S#Tx] {
  def numChannels(implicit tx: S#Tx): Int

  def accept()(implicit tx: S#Tx): Unit

  def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit
  def play   (timeRef: TimeRef, builder: NodeDependencyBuilder[S])(implicit tx: S#Tx): Unit

  // def stop   ()(implicit tx: S#Tx): Unit
}
