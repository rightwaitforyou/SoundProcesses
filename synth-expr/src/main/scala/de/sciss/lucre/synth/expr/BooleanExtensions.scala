/*
 *  BooleanExtensions.scala
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

import de.sciss.lucre.{event => evt}
import de.sciss.lucre.event.{Targets, Sys}
import de.sciss.serial.DataInput
import de.sciss.lucre.expr.{Type, Expr, Boolean => BooleanEx, Int => IntEx}
import scala.annotation.switch

object BooleanExtensions  {
  private[this] type Ex[S <: Sys[S]] = Expr[S, Boolean]

  BooleanEx.registerExtension(1, BooleanTuple1s)
  BooleanEx.registerExtension(2, BooleanTuple2s)

  private[this] object BooleanTuple1s extends Type.Extension1[({type Repr[~ <: Sys[~]] = Expr[~, Boolean]})#Repr] {
    final val arity = 1
    final val opLo  = Not.id
    final val opHi  = Not.id

    val name = "Boolean-1 Ops"

    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Expr.Node[S, Boolean] = {
      val op: UnaryOp[Boolean] = (opID: @switch) match {
        case Not.id => Not
      }
      val _1 = BooleanEx.read(in, access)
      new impl.Tuple1(BooleanEx, BooleanEx.typeID, op, targets, _1)
    }
  }

  private[this] object BooleanTuple2s extends Type.Extension1[({type Repr[~ <: Sys[~]] = Expr[~, Boolean]})#Repr] {
    final val arity = 2
    final val opLo  = And   .id
    final val opHi  = IntGeq.id

    val name = "Boolean-2 Ops"

    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Expr.Node[S, Boolean] = {
      val op: BinaryOp[_] = (opID: @switch) match {
        case And   .id => And
        case Or    .id => Or
        case Xor   .id => Xor

        case IntEq .id => IntEq
        case IntNeq.id => IntNeq
        case IntLt .id => IntLt
        case IntGt .id => IntGt
        case IntLeq.id => IntLeq
        case IntGeq.id => IntGeq
      }
      op.read(in, access, targets)
    }
  }

  // ----- operators -----

  final class Ops[S <: Sys[S]](val `this`: Expr[S, Boolean]) extends AnyVal { me =>
    import me.{`this` => a}
    private type E = Expr[S, Boolean]

    def unary_!(implicit tx: S#Tx): Ex[S] = Not(a)

    def && (b: E)(implicit tx: S#Tx): Ex[S] = And(a, b)
    def || (b: E)(implicit tx: S#Tx): Ex[S] = Or (a, b)
    def ^^ (b: E)(implicit tx: S#Tx): Ex[S] = Xor(a, b)
  }

  // ----- impl -----

  private[this] abstract class UnaryOp[T1] extends impl.Tuple1Op[Boolean, T1] {
    def id: Int
    final def apply[S <: Sys[S]](a: Expr[S, T1])(implicit tx: S#Tx): Ex[S] = a match {
      case Expr.Const(ca) => BooleanEx.newConst(value(ca))
      case _ => new impl.Tuple1(BooleanEx, BooleanEx.typeID, this, Targets.partial[S], a)
    }

    def toString[S <: Sys[S]](_1: Expr[S, T1]): String = s"$name${_1}"

    def name: String
  }

  private[this] case object Not extends UnaryOp[Boolean] {
    final val id = 0
    def value(a: Boolean): Boolean = !a
    def name = "!"
  }

  sealed trait BinaryOp[T1] extends impl.Tuple2Op[Boolean, T1, T1] {
    // ---- abstract ----

    def name: String

    def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Expr.Node[S, Boolean]

    // ---- impl ----

    final def apply[S <: Sys[S]](a: Expr[S, T1], b: Expr[S, T1])(implicit tx: S#Tx): Ex[S] = (a, b) match {
      case (Expr.Const(ca), Expr.Const(cb)) => BooleanEx.newConst(value(ca, cb))
      case _ => new impl.Tuple2(BooleanEx, BooleanEx.typeID, this, Targets.partial[S], a, b)
    }

    final def toString[S <: Sys[S]](_1: Expr[S, T1], _2: Expr[S, T1]): String = s"(${_1} $name ${_2})"
  }

  private[this] trait BooleanBinaryOp extends BinaryOp[Boolean] { op =>
    def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Expr.Node[S, Boolean] = {
      val _1 = BooleanEx.read(in, access)
      val _2 = BooleanEx.read(in, access)
      new impl.Tuple2(BooleanEx, BooleanEx.typeID, op, targets, _1, _2)
    }
  }

  sealed trait IntBinaryOp extends BinaryOp[Int] { op =>
    final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Expr.Node[S, Boolean] = {
      val _1 = IntEx.read(in, access)
      val _2 = IntEx.read(in, access)
      new impl.Tuple2(BooleanEx, BooleanEx.typeID, op, targets, _1, _2)
    }
  }

  // ---- (Boolean, Boolean) => Boolean ----

  private[this] case object And extends BooleanBinaryOp {
    final val id = 0
    def value(a: Boolean, b: Boolean): Boolean = a && b
    def name = "&&"
  }

  private[this] case object Or extends BooleanBinaryOp {
    final val id = 1
    def value(a: Boolean, b: Boolean): Boolean = a || b
    def name = "||"
  }

  private[this] case object Xor extends BooleanBinaryOp {
    final val id = 2
    def value(a: Boolean, b: Boolean): Boolean = a && b
    def name = "&&"
  }

  // ---- (Int, Int) => Boolean ----

 case object IntEq extends IntBinaryOp {
    final val id = 10
    def value(a: Int, b: Int): Boolean = a == b
    def name = "sig_=="
  }

  case object IntNeq extends IntBinaryOp {
    final val id = 11
    def value(a: Int, b: Int): Boolean = a != b
    def name = "sig_!="
  }

  case object IntLt extends IntBinaryOp {
    final val id = 12
    def value(a: Int, b: Int): Boolean = a < b
    def name = "<"
  }

  case object IntGt extends IntBinaryOp {
    final val id = 13
    def value(a: Int, b: Int): Boolean = a > b
    def name = ">"
  }

  case object IntLeq extends IntBinaryOp {
    final val id = 14
    def value(a: Int, b: Int): Boolean = a <= b
    def name = "<="
  }

  case object IntGeq extends IntBinaryOp {
    final val id = 15
    def value(a: Int, b: Int): Boolean = a >= b
    def name = ">="
  }
}