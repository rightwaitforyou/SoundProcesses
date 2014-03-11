/*
 *  SpanExtensions.scala
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
import de.sciss.lucre.event.{Node, Targets, Sys}
import de.sciss.lucre.expr.{Type, Expr}
import de.sciss.span.Span
import de.sciss.serial.{DataOutput, DataInput}
import scala.annotation.switch
import de.sciss.lucre
import de.sciss.lucre.bitemp
import de.sciss.span

object SpanExtensions  {
  import bitemp.Span.{newConst, read, typeID}

  private[this] type Ex[S <: Sys[S]] = Expr[S, span.Span]

  lucre.expr.Long.registerExtension(1, LongTuple1s)
  bitemp    .Span.registerExtension(2, SpanTuple2s)

  private[this] object LongTuple1s extends Type.Extension1[({type Repr[~ <: Sys[~]] = Expr[~, Long]})#Repr] {
    final val arity = 1
    final val opLo  = UnaryOp.Start .id
    final val opHi  = UnaryOp.Length.id

    val name = "Span-Long Ops"

    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Expr.Node[S, Long] = {
      import UnaryOp._
      val op: LongOp = (opID: @switch) match {
        // ---- Span ----
        case Start  .id => Start
        case Stop   .id => Stop
        case Length .id => Length
      }
      op.read(in, access, targets)
    }
  }

  private[this] object SpanTuple2s extends Type.Extension1[({type Repr[~ <: Sys[~]] = Expr[~, span.Span]})#Repr] {
    final val arity = 2
    final val opLo  = BinaryOp.Apply.id
    final val opHi  = BinaryOp.Shift.id

    val name = "Int-Int Ops"

    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Expr.Node[S, span.Span] = {
      import BinaryOp._
      val op: Op[_, _] = (opID: @switch) match {
        case Apply.id => Apply
        case Shift.id => Shift
      }
      op.read(in, access, targets)
    }
  }

  def apply[S <: Sys[S]](start: Expr[S, Long], stop: Expr[S, Long])(implicit tx: S#Tx): Ex[S] =
    (start, stop) match {
      case (Expr.Const(startC), Expr.Const(stopC)) => newConst(span.Span(startC, stopC))
      case _ =>
        new impl.Tuple2(bitemp.Span, BinaryOp.Apply.id, BinaryOp.Apply, Targets.partial[S], start, stop)
    }

  // XXX TODO: fold constants
  final class Ops[S <: Sys[S]](val `this`: Ex[S]) extends AnyVal { me =>
    import me.{`this` => ex}
    // ---- unary ----
    def start (implicit tx: S#Tx): Expr[S, Long] = UnaryOp.Start (ex)
    def stop  (implicit tx: S#Tx): Expr[S, Long] = UnaryOp.Stop  (ex)
    def length(implicit tx: S#Tx): Expr[S, Long] = UnaryOp.Length(ex)

    // ---- binary ----
    def shift(delta: Expr[S, Long])(implicit tx: S#Tx): Ex[S] = BinaryOp.Shift(ex, delta)
  }

  // ----- operators -----

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

    sealed abstract class LongOp extends LongExtensions.UnaryOp.Op[span.Span] /* Longs.UnaryOp.Op[Span] */ {
      def id: Int
      final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                     (implicit tx: S#Tx): impl.Tuple1[S, Long, span.Span] = {
        val _1 = bitemp.Span.read(in, access)
        new impl.Tuple1(lucre.expr.Long, typeID, this, targets, _1)
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
    sealed trait Op[T1, T2] {
      def toString[S <: Sys[S]](_1: Expr[S, T1], _2: Expr[S, T2]): String = s"${_1}.$name(${_2})"

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i  = cn.lastIndexOf('$', sz - 2) + 1
        "" + cn.charAt(i).toLower + cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
      }

      def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                           (implicit tx: S#Tx): impl.Tuple2[S, span.Span, T1, T2]
    }

    sealed abstract class LongSpanOp(val id: Int)
      extends impl.Tuple2Op[span.Span, span.Span, Long] with Op[span.Span, Long] {

      final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                     (implicit tx: S#Tx): impl.Tuple2[S, span.Span, span.Span, Long] = {
        val _1 = bitemp    .Span.read(in, access)
        val _2 = lucre.expr.Long.read(in, access)
        new impl.Tuple2(bitemp.Span, typeID, this, targets, _1, _2)
      }

      final def apply[S <: Sys[S]](a: Ex[S], b: Expr[S, Long])(implicit tx: S#Tx): Ex[S] = (a, b) match {
        case (Expr.Const(ca), Expr.Const(cb)) => newConst(value(ca, cb))
        case _                                => new impl.Tuple2(bitemp.Span, typeID, this, Targets.partial[S], a, b)
      }
    }

    case object Shift extends LongSpanOp(0x1100) {
      def value(a: span.Span, b: Long): span.Span = a.shift(b)
    }

    object Apply extends impl.Tuple2Op[Span, Long, Long] with Op[Long, Long] {
      final val id = 0
      def value(a: Long, b: Long): Span = Span(a, b)
      override def toString[S1 <: Sys[S1]](_1: Expr[S1, Long], _2: Expr[S1, Long]): String = s"Span(${_1}, ${_2})"

      def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                           (implicit tx: S#Tx): impl.Tuple2[S, Span, Long, Long] = {
        val _1 = lucre.expr.Long.read(in, access)
        val _2 = lucre.expr.Long.read(in, access)
        new impl.Tuple2(bitemp.Span, typeID, this, targets, _1, _2)
      }
    }
  }
}