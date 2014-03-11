/*
 *  package.scala
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

package de.sciss.lucre.synth

import de.sciss.serial.{Serializer, DataInput, DataOutput}
import de.sciss.lucre.stm
import de.sciss.synth
import de.sciss.lucre.expr.impl.ExprTypeImplA
import de.sciss.lucre.expr.{Expr, Type1A, ExprType}
import de.sciss.lucre.event.Sys

package object expr {
  private type ExprTypeA[A] = ExprType[A] with Type1A[({type Repr[~ <: Sys[~]] = Expr[~, A]})#Repr]

  // this is plain stupid... another reason why the scan should reproduce the proc and key (problem though: proc -> timed-proc)
  implicit def IdentifierSerializer[S <: stm.Sys[S]]: Serializer[S#Tx, S#Acc, S#ID] =
    anyIDSer.asInstanceOf[Serializer[S#Tx, S#Acc, S#ID]]

  private val anyIDSer = new IDSer[stm.InMemory]

  private final class IDSer[S <: stm.Sys[S]] extends Serializer[S#Tx, S#Acc, S#ID] {
    def write(id: S#ID, out: DataOutput): Unit = id.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): S#ID = tx.readID(in, access)
  }

  val Curve : ExprTypeA[synth.Curve] = CurveImpl

  private[this] object CurveImpl extends ExprTypeImplA[synth.Curve] {
    final val typeID = 15

    def readValue (                    in : DataInput ): synth.Curve  = synth.Curve.serializer.read (       in )
    def writeValue(value: synth.Curve, out: DataOutput): Unit         = synth.Curve.serializer.write(value, out)
  }

  /** Initializes types and thus installs some type extensions. */
  def initTypes(): Unit = {
    // BooleanExtensions
    // Curve
    DoubleExtensions
    IntExtensions
    LongExtensions
    SpanLikeExtensions
    SpanExtensions
    StringExtensions
    DoubleVec
    IntVec
    LongVec
  }
}