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

import de.sciss.lucre.{stm, event => evt, expr, bitemp}
import evt.{Targets, Sys}
import expr.Expr
import annotation.switch
import de.sciss.span.{Span, SpanLike}
import de.sciss.serial.{DataOutput, DataInput}

object SpanLikes extends BiTypeImpl[SpanLike] {
  final val typeID = 9

  /* protected */ def readValue(in: DataInput): SpanLike = SpanLike.read(in)

  /* protected */ def writeValue(value: SpanLike, out: DataOutput) {
    value.write(out)
  }

  def newExpr[S <: Sys[S]](start: Expr[S, Long], stop: Expr[S, Long])(implicit tx: S#Tx): Ex[S] =
    BinaryOp.Apply(start, stop)

  def from[S <: Sys[S]](start: Expr[S, Long])(implicit tx: S#Tx): Ex[S] =
    UnaryOp.From(start)

  def until[S <: Sys[S]](stop: Expr[S, Long])(implicit tx: S#Tx): Ex[S] =
    UnaryOp.Until(stop)

  final class Ops[S <: Sys[S]](ex: Ex[S])(implicit tx: S#Tx) {
    // ---- binary ----
    def shift(delta: Expr[S, Long]): Ex[S] = BinaryOp.Shift(ex, delta)
  }

  private object UnaryOp {

    sealed trait Op[T1] {
      def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                           (implicit tx: S#Tx): Tuple1[S, T1]

      def toString[S <: stm.Sys[S]](_1: Expr[S, T1]): String = s"$name(${_1})"

      def name: String = {
        val cn  = getClass.getName
        val sz  = cn.length
        val i   = cn.lastIndexOf('$', sz - 2) + 1
        "" + cn.charAt(i).toLower + cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
      }
    }

    sealed abstract class LongOp(val id: Int) extends Tuple1Op[Long] with Op[Long] {
      final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                 (implicit tx: S#Tx): Tuple1[S, Long] = {
        val _1 = Longs.readExpr(in, access)
        new Tuple1(typeID, this, targets, _1)
      }

      final def apply[S <: Sys[S]](a: Expr[S, Long])(implicit tx: S#Tx): Ex[S] = {
        new Tuple1(typeID, this, Targets.partial[S], a)
      }
    }

    case object From extends LongOp(0) {
      def value(a: Long): SpanLike = Span.from(a)

      override def toString[S <: stm.Sys[S]](_1: Expr[S, Long]): String = s"Span.from(${_1 })"
    }

    case object Until extends LongOp(1) {
      def value(a: Long): SpanLike = Span.until(a)

      override def toString[S <: stm.Sys[S]](_1: Expr[S, Long]): String = s"Span.until(${_1})"
    }
  }

  private object BinaryOp {
    sealed trait Op[T1, T2] {
      def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                           (implicit tx: S#Tx): Tuple2[S, T1, T2]

      def toString[S <: stm.Sys[S]](_1: Expr[S, T1], _2: Expr[S, T2]): String =
        _1.toString + "." + name + "(" + _2 + ")"

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i = cn.lastIndexOf('$', sz - 2) + 1
        "" + cn.charAt(i).toLower + cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
      }
    }

    sealed abstract class LongSpanOp(val id: Int) extends Tuple2Op[SpanLike, Long] with Op[SpanLike, Long] {
      final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                 (implicit tx: S#Tx): Tuple2[S, SpanLike, Long] = {
        val _1 = readExpr(in, access)
        val _2 = Longs.readExpr(in, access)
        new Tuple2(typeID, this, targets, _1, _2)
      }

      final def apply[S <: Sys[S]](a: Ex[S], b: Expr[S, Long])(implicit tx: S#Tx): Ex[S] = {
        new Tuple2(typeID, this, Targets.partial[S], a, b)
      }
    }

    sealed abstract class LongLongOp(val id: Int) extends Tuple2Op[Long, Long] with Op[Long, Long] {
      final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                 (implicit tx: S#Tx): Tuple2[S, Long, Long] = {
        val _1 = Longs.readExpr(in, access)
        val _2 = Longs.readExpr(in, access)
        new Tuple2(typeID, this, targets, _1, _2)
      }

      final def apply[S <: Sys[S]](a: Expr[S, Long], b: Expr[S, Long])(implicit tx: S#Tx): Ex[S] = {
        new Tuple2(typeID, this, Targets.partial[S], a, b)
      }
    }

    case object Apply extends LongLongOp(0) {
      override def toString[S <: stm.Sys[S]](_1: Expr[S, Long], _2: Expr[S, Long]): String =
        "Span(" + _1 + ", " + _2 + ")"

      def value(a: Long, b: Long): SpanLike = Span(a, b)
    }

    case object Shift extends LongSpanOp(1) {
      def value(a: SpanLike, b: Long): SpanLike = a.shift(b)
    }
  }

  def readTuple[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                            (implicit tx: S#Tx): ExN[S] =
    (cookie: @switch) match {
      case 1 =>
        val tpe = in.readInt()
        require(tpe == typeID, s"Invalid type id (found $tpe, required $typeID)")
        val opID = in.readInt()
        import UnaryOp._
        val op: Op[_] = (opID: @switch) match {
          case 0 => From
          case 1 => Until
          case _ => sys.error(s"Invalid operation id $opID")
        }
        op.read(in, access, targets)

      case 2 =>
        val tpe = in.readInt()
        require(tpe == typeID, s"Invalid type id (found $tpe, required $typeID)")
        val opID = in.readInt()
        import BinaryOp._
        val op: Op[_, _] = (opID: @switch) match {
          case 0 => Apply
          case 1 => Shift
          case _ => sys.error(s"Invalid operation id $opID")
        }
        op.read(in, access, targets)

      //         case 3 =>
      //            readProjection[ S ]( in, access, targets )

      case _ => sys.error(s"Invalid cookie $cookie")
    }
}