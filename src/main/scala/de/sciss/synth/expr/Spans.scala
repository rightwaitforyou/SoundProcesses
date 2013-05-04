/*
 *  Booleans.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.expr

import de.sciss.lucre.{stm, event => evt, expr}
import evt.Targets
import expr.Expr
import de.sciss.span.Span
import de.sciss.serial.{DataOutput, DataInput}

object Spans extends BiTypeImpl[Span] {
  final val typeID = 10

  def readValue(in: DataInput): Span = Span.read(in)

  def writeValue(value: Span, out: DataOutput) {
    value.write(out)
  }

  final class Ops[S <: evt.Sys[S]](ex: Ex[S])(implicit tx: S#Tx) {
    // ---- unary ----
    def start : Expr[S, Long] = UnaryOp.Start(ex)
    def stop  : Expr[S, Long] = UnaryOp.Stop(ex)
    def length: Expr[S, Long] = UnaryOp.Length(ex)

    // ---- binary ----
    def shift(delta: Expr[S, Long]): Ex[S] = BinaryOp.Shift(ex, delta)
  }

  // ---- protected ----

  def readTuple[S <: stm.Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])(implicit tx: S#Tx): ExN[S] =
    sys.error("Invalid cookie " + cookie)

  object UnaryOp {
    //      sealed trait OpLike[ T1 ] {
    //         def toString[ S <: Sys[ S ]]( _1: Expr[ S, T1 ]) : String = _1.toString + "." + name
    //
    //         def name: String = { val cn = getClass.getName
    //            val sz   = cn.length
    //            val i    = cn.indexOf( '$' ) + 1
    //            "" + cn.charAt( i ).toLower + cn.substring( i + 1, if( cn.charAt( sz - 1 ) == '$' ) sz - 1 else sz )
    //         }
    //      }

    sealed abstract class LongOp extends Longs.UnaryOp.Op[Span] {
      def id: Int
      final def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                     (implicit tx: S#Tx): Longs.Tuple1[S, Span] = {
        val _1 = readExpr(in, access)
        new Longs.Tuple1(typeID, this, targets, _1)
      }

      //         final def make[ S <: Sys[ S ]]( a: Ex[ S ])( implicit tx: S#Tx ) : Expr[ S, Long ] = {
      //            new Longs.Tuple1( typeID, this, Targets.partial[ S ], a )
      //         }
    }

    case object Start extends LongOp {
      final val id = 0x1000
      def value(a: Span): Long = a.start
    }

    case object Stop extends LongOp {
      final val id = 0x1001
      def value(a: Span): Long = a.stop
    }

    case object Length extends LongOp {
      final val id = 0x1002
      def value(a: Span): Long = a.length
    }
  }

  private object BinaryOp {
    sealed trait OpLike[T1, T2] {
      def toString[S <: stm.Sys[S]](_1: Expr[S, T1], _2: Expr[S, T2]): String = s"${_1}.$name(${_2})"

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i = cn.lastIndexOf('$', sz - 2) + 1
        "" + cn.charAt(i).toLower + cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
      }
    }

    sealed abstract class LongSpanOp(val id: Int) extends Tuple2Op[Span, Long] with OpLike[Span, Long] {
      final def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                     (implicit tx: S#Tx): Tuple2[S, Span, Long] = {
        val _1 = readExpr(in, access)
        val _2 = Longs.readExpr(in, access)
        new Tuple2(typeID, this, targets, _1, _2)
      }

      final def apply[S <: evt.Sys[S]](a: Ex[S], b: Expr[S, Long])(implicit tx: S#Tx): Ex[S] = (a, b) match {
        case (Expr.Const(ca), Expr.Const(cb)) => newConst(value(ca, cb))
        case _                                => new Tuple2(typeID, this, Targets.partial[S], a, b)
      }
    }

    case object Shift extends LongSpanOp(0x1100) {
      def value(a: Span, b: Long): Span = a.shift(b)
    }
  }
}