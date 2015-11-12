package de.sciss.synth.proc

import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, Disposable, Sys}

object AuralView {
  /* The current state a view is in. */
  sealed trait State
  case object Stopped   extends State
  case object Preparing extends State
  case object Prepared  extends State
  case object Playing   extends State
}
trait AuralView[S <: Sys[S], Target] extends Observable[S#Tx, AuralView.State] with Disposable[S#Tx] {
  def typeID: Int

  /** The view must store a handle to its underlying model. */
  def obj: stm.Source[S#Tx, Obj[S]]

  def state(implicit tx: S#Tx): AuralView.State

  def prepare(timeRef: TimeRef                )(implicit tx: S#Tx): Unit
  def play   (timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit

  def stop()(implicit tx: S#Tx): Unit
}