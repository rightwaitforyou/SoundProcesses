/*
 *  Booleans.scala
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
package expr

import de.sciss.lucre.{event => evt, expr}
import evt.{Targets, Sys}
import expr.Expr
import annotation.switch
import de.sciss.span.{Span, SpanLike}
import de.sciss.serial.{DataOutput, DataInput}
import de.sciss.lucre.bitemp.BiType

object SpanLikes extends BiTypeImpl[SpanLike] {
  final val typeID = 9

  def readValue (                 in : DataInput ): SpanLike  = SpanLike.read (in )
  def writeValue(value: SpanLike, out: DataOutput): Unit      = value   .write(out)

  lazy val install: Unit = {
    registerOp(SpanLikeTuple1s)
    registerOp(SpanLikeTuple2s)
  }

  def newExpr[S <: Sys[S]](start: Expr[S, Long], stop: Expr[S, Long])(implicit tx: S#Tx): Ex[S] =
    BinaryOp.Apply(start, stop)

  def from[S <: Sys[S]](start: Expr[S, Long])(implicit tx: S#Tx): Ex[S] =
    UnaryOp.From(start)

  def until[S <: Sys[S]](stop: Expr[S, Long])(implicit tx: S#Tx): Ex[S] =
    UnaryOp.Until(stop)

  private[this] object SpanLikeTuple1s extends BiType.TupleReader[SpanLike] {
    final val arity = 1
    final val opLo  = UnaryOp.From .id
    final val opHi  = UnaryOp.Until.id

    val name = "Long-SpanLike Ops"

    def readTuple[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                              (implicit tx: S#Tx): Expr.Node[S, SpanLike] = {
      import UnaryOp._
      val op: Op[_] = (opID: @switch) match {
        case From .id => From
        case Until.id => Until
        case _ => sys.error(s"Invalid operation id $opID")
      }
      op.read(in, access, targets)
    }
  }

  private[this] object SpanLikeTuple2s extends BiType.TupleReader[SpanLike] {
    final val arity = 2
    final val opLo  = BinaryOp.Apply.id
    final val opHi  = BinaryOp.Shift.id

    val name = "Int-Int Ops"

    def readTuple[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                              (implicit tx: S#Tx): Expr.Node[S, SpanLike] = {
      import BinaryOp._
      val op: Op[_, _] = (opID: @switch) match {
        case Apply.id => Apply
        case Shift.id => Shift
      }
      op.read(in, access, targets)
    }
  }

  // ---- operators ----

  final class Ops[S <: Sys[S]](ex: Ex[S])(implicit tx: S#Tx) {
    // ---- binary ----
    def shift(delta: Expr[S, Long]): Ex[S] = BinaryOp.Shift(ex, delta)
  }

  private object UnaryOp {

    sealed trait Op[T1] {
      def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                           (implicit tx: S#Tx): Tuple1[S, T1]

      def toString[S <: Sys[S]](_1: Expr[S, T1]): String = s"$name(${_1})"

      def name: String = {
        val cn  = getClass.getName
        val sz  = cn.length
        val i   = cn.lastIndexOf('$', sz - 2) + 1
        "" + cn.charAt(i).toLower + cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
      }
    }

    sealed abstract class LongOp extends Tuple1Op[Long] with Op[Long] {
      def id: Int
      final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                 (implicit tx: S#Tx): Tuple1[S, Long] = {
        val _1 = Longs.readExpr(in, access)
        new Tuple1(typeID, this, targets, _1)
      }

      final def apply[S <: Sys[S]](a: Expr[S, Long])(implicit tx: S#Tx): Ex[S] = {
        new Tuple1(typeID, this, Targets.partial[S], a)
      }
    }

    case object From extends LongOp {
      final val id = 0
      def value(a: Long): SpanLike = Span.from(a)

      override def toString[S <: Sys[S]](_1: Expr[S, Long]): String = s"Span.from(${_1 })"
    }

    case object Until extends LongOp {
      final val id = 1
      def value(a: Long): SpanLike = Span.until(a)

      override def toString[S <: Sys[S]](_1: Expr[S, Long]): String = s"Span.until(${_1})"
    }
  }

  private object BinaryOp {
    sealed trait Op[T1, T2] {
      def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                           (implicit tx: S#Tx): Tuple2[S, T1, T2]

      def toString[S <: Sys[S]](_1: Expr[S, T1], _2: Expr[S, T2]): String =
        _1.toString + "." + name + "(" + _2 + ")"

      def value(a: T1, b: T2): SpanLike

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i = cn.lastIndexOf('$', sz - 2) + 1
        "" + cn.charAt(i).toLower + cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
      }
    }

    sealed abstract class LongSpanOp extends Tuple2Op[SpanLike, Long] with Op[SpanLike, Long] {
      def id: Int
      final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                 (implicit tx: S#Tx): Tuple2[S, SpanLike, Long] = {
        val _1 = readExpr(in, access)
        val _2 = Longs.readExpr(in, access)
        new Tuple2(typeID, this, targets, _1, _2)
      }

      final def apply[S <: Sys[S]](a: Ex[S], b: Expr[S, Long])(implicit tx: S#Tx): Ex[S] = (a, b) match {
        case (Expr.Const(ca), Expr.Const(cb)) => newConst(value(ca, cb))
        case _                                => new Tuple2(typeID, this, Targets.partial[S], a, b)
      }
    }

    sealed abstract class LongLongOp extends Tuple2Op[Long, Long] with Op[Long, Long] {
      def id: Int
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

    case object Apply extends LongLongOp {
      final val id = 0
      override def toString[S <: Sys[S]](_1: Expr[S, Long], _2: Expr[S, Long]): String =
        "Span(" + _1 + ", " + _2 + ")"

      def value(a: Long, b: Long): SpanLike = Span(a, b)
    }

    case object Shift extends LongSpanOp {
      final val id = 1
      def value(a: SpanLike, b: Long): SpanLike = a.shift(b)
    }
  }
}