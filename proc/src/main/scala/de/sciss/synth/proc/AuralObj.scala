/*
 *  AuralObj.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.synth.Sys
import de.sciss.lucre.{event => evt, stm}
import de.sciss.processor.{GenericProcessor, Processor}
import de.sciss.span.SpanLike
import de.sciss.synth.proc
import language.higherKinds
import impl.{AuralObjImpl => Impl}

object AuralObj {
  import proc.{Proc => _Proc}

  trait Factory {
    def typeID: Int

    type E[~ <: evt.Sys[~]] <: Elem[~]

    def apply[S <: Sys[S]](obj: Obj.T[S, E])(implicit tx: S#Tx): AuralObj[S]
  }

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): AuralObj[S] = Impl(obj)

  // --------------

  trait Proc[S <: Sys[S]] extends AuralObj[S] {
    override def obj: stm.Source[S#Tx, Obj.T[S, _Proc.Elem]]
  }
}
trait AuralObj[S <: Sys[S]] /* extends Observable[...] */ {
  def typeID: Int

  /** The view must store a handle to its underlying model. */
  def obj: stm.Source[S#Tx, Obj[S]]

  def latencyEstimate(implicit tx: S#Tx): Long

  def prepare()(implicit tx: S#Tx): GenericProcessor[Unit]

  def isPrepared(implicit tx: S#Tx): Boolean

  def play(time: SpanLike)(implicit tx: S#Tx): Unit
  def stop(time: Long    )(implicit tx: S#Tx): Unit
}