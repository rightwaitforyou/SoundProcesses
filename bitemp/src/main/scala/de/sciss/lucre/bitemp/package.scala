/*
 *  package.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre

import de.sciss.lucre.stm.Sys
import de.sciss.serial.{DataOutput, DataInput}
import de.sciss.span
import expr.{Expr, ExprType1, Type1A}

package object bitemp {
  private type ExprType1A[A] = ExprType1[A] with Type1A[({type Repr[~ <: Sys[~]] = Expr[~, A]})#Repr]

  val SpanLike  : ExprType1A[span.SpanLike] = SpanLikeImpl
  val Span      : ExprType1A[span.Span    ] = SpanImpl

  private[this] object SpanLikeImpl extends expr.impl.ExprTypeImplA[span.SpanLike] {
    final val typeID = 9

    def readValue (                      in : DataInput ): span.SpanLike  = span.SpanLike.read(in)
    def writeValue(value: span.SpanLike, out: DataOutput): Unit           = value.write(out)
  }

  private[this] object SpanImpl extends expr.impl.ExprTypeImplA[span.Span] {
    final val typeID = 10

    def readValue (                  in : DataInput ): span.Span  = span.Span.read(in)
    def writeValue(value: span.Span, out: DataOutput): Unit       = value.write(out)
  }
}
