/*
 *  IntExtensions.scala
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

package de.sciss.lucre.synth.expr

import de.sciss.lucre
import de.sciss.lucre.event.{Targets, Sys}
import de.sciss.lucre.expr.{Type, Expr, Int => IntEx, Boolean => BooleanEx}
import de.sciss.serial.DataInput

import scala.annotation.switch

object IntExtensions {
  private[this] type Ex[S <: Sys[S]] = Expr[S, Int]

  IntEx.registerExtension(1, IntTuple1s)
  IntEx.registerExtension(2, IntTuple2s)

  private[this] object IntTuple1s extends Type.Extension1[({type Repr[~ <: Sys[~]] = Expr[~, Int]})#Repr] {
    final val arity = 1
    final val opLo  = Neg         .id
    final val opHi  = BooleanToInt.id

    val name = "Int-1 Ops"

    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Expr.Node[S, Int] = {
      val op: UnaryOp[_] = (opID: @switch) match {
        // ---- Int => Int ----
        case Neg    .id => Neg
        case BitNot .id => BitNot
        case Abs    .id => Abs
        case Signum .id => Signum
        case Squared.id => Squared
        case Cubed  .id => Cubed
        // ---- Boolean => Int ----
        case BooleanToInt.id => BooleanToInt
      }
      op.read(in, access, targets)
    }
  }

  private[this] object IntTuple2s extends Type.Extension1[({type Repr[~ <: Sys[~]] = Expr[~, Int]})#Repr] {
    final val arity = 2
    final val opLo  = Plus  .id
    final val opHi  = Absdif.id

    val name = "Int-2 Ops"

    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Expr.Node[S, Int] = {
      val op: BinaryOp = (opID: @switch) match {
        case Plus               .id => Plus
        case Minus              .id => Minus
        case Times              .id => Times
        case IDiv               .id => IDiv
        //               case 4 => Div
        //               case 5 => Mod
        //      case 6 => Eq
        //      case 7 => Neq
        //      case 8 => Lt
        //      case 9 => Gt
        //      case 10 => Leq
        //      case 11 => Geq
        case Min                .id => Min
        case Max                .id => Max
        case BitAnd             .id => BitAnd
        case BitOr              .id => BitOr
        case BitXor             .id => BitXor
        // case 17 => Lcm
        // case 18 => Gcd
        //               case 19 => Round
        //               case 20 => Roundup
        case ShiftLeft          .id => ShiftLeft
        case ShiftRight         .id => ShiftRight
        case UnsignedShiftRight .id => UnsignedShiftRight
        case Absdif             .id => Absdif
        //               case 42 => Clip2
        //               case 44 => Fold2
        //               case 45 => Wrap2
      }
      val _1 = IntEx.read(in, access)
      val _2 = IntEx.read(in, access)
      new impl.Tuple2(lucre.expr.Int, IntEx.typeID, op, targets, _1, _2)
    }
  }

  // ---- operators ----

  sealed trait UnaryOp[T1] extends impl.Tuple1Op[Int, T1] {
    def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                         (implicit tx: S#Tx): impl.Tuple1[S, Int, T1]

    def toString[S <: Sys[S]](_1: Expr[S, T1]): String = _1.toString + "." + name

    def apply[S <: Sys[S]](a: Expr[S, T1])(implicit tx: S#Tx): Ex[S] = a match {
      case Expr.Const(c)  => IntEx.newConst(value(c))
      case _              => new impl.Tuple1(lucre.expr.Int, IntEx.typeID, this, Targets.partial[S], a)
    }

    def name: String = {
      val cn  = getClass.getName
      val sz  = cn.length
      val i   = cn.lastIndexOf('$', sz - 2) + 1
      "" + cn.charAt(i).toLower + cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
    }
  }

  // ---- Int => Int ----

  private[this] sealed abstract class IntUnaryOp extends UnaryOp[Int] {
    final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                               (implicit tx: S#Tx): impl.Tuple1[S, Int, Int] = {
      val _1 = lucre.expr.Int.read(in, access)
      new impl.Tuple1(lucre.expr.Int, IntEx.typeID, this, targets, _1)
    }
  }

  private[this] case object Neg extends IntUnaryOp {
    final val id = 0
    def value(a: Int): Int = -a
    override def toString[S <: Sys[S]](_1: Ex[S]): String = "-" + _1
  }

  private[this] case object Abs extends IntUnaryOp {
    final val id = 5
    def value(a: Int): Int = math.abs(a)
  }

  private[this] case object BitNot extends IntUnaryOp {
    final val id = 4
    def value(a: Int): Int = ~a
    override def toString[S <: Sys[S]](_1: Ex[S]): String = "~" + _1
  }

  // case object ToLong     extends Op(  6 )
  // case object ToInt       extends Op(  7 )
  private[this] case object Signum extends IntUnaryOp {
    final val id = 11
    def value(a: Int): Int = math.signum(a)
  }

  private[this] case object Squared extends IntUnaryOp {
    final val id = 12
    def value(a: Int): Int = a * a
  }

  private[this] case object Cubed extends IntUnaryOp {
    final val id = 13
    def value(a: Int): Int = a * a * a
  }

  // ---- Boolean => Int ----

  sealed trait BooleanUnaryOp extends UnaryOp[Boolean] {
    final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                               (implicit tx: S#Tx): impl.Tuple1[S, Int, Boolean] = {
      val _1 = BooleanEx.read(in, access)
      new impl.Tuple1(IntEx, IntEx.typeID, this, targets, _1)
    }
  }

  case object BooleanToInt extends BooleanUnaryOp {
    final val id = 100
    def value(a: Boolean): Int = if (a) 1 else 0
  }

  // ---- (Int, Int) => Int ----

  private[this] sealed trait BinaryOp extends impl.Tuple2Op[Int, Int, Int] {
    final def apply[S <: Sys[S]](a: Ex[S], b: Ex[S])(implicit tx: S#Tx): Ex[S] = (a, b) match {
      case (Expr.Const(ca), Expr.Const(cb)) => IntEx.newConst(value(ca, cb))
      case _                                => new impl.Tuple2(IntEx, IntEx.typeID, this, Targets.partial[S], a, b)
    }

    def value(a: Int, b: Int): Int

    def isInfix: Boolean

    final def toString[S <: Sys[S]](_1: Ex[S], _2: Ex[S]): String =
      if (isInfix) s"(${_1} $name ${_2})" else s"${_1}.$name(${_2})"

    def name: String = {
      val cn = getClass.getName
      val sz = cn.length
      val i  = cn.indexOf('$') + 1
      "" + cn.charAt(i).toLower + cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
    }
  }

  //  trait Infix {
  //    _: BinaryOp =>
  //
  //    override def toString[S <: Sys[S]](_1: Ex[S], _2: Ex[S]): String =
  //      "(" + _1 + " " + name + " " + _2 + ")"
  //  }

  //      sealed trait MathStyle {
  //         def name: String
  //         override def toString[ S <: Sys[ S ]]( _1: Ex[ S ], _2: Ex[ S ]) : String =
  //            "(" + _1 + " " + name + " " + _2 + ")"
  //      }

  private[this] case object Plus extends BinaryOp {
    final val id = 0
    override val name = "+"
    def value(a: Int, b: Int): Int = a + b
    final val isInfix = true
  }

  private[this] case object Minus extends BinaryOp {
    final val id = 1
    override val name = "-"
    def value(a: Int, b: Int): Int = a - b
    final val isInfix = true
  }

  private[this] case object Times extends BinaryOp {
    final val id = 2
    override val name = "*"
    def value(a: Int, b: Int): Int = a * b
    final val isInfix = true
  }

  private[this] case object IDiv extends BinaryOp {
    final val id = 3
    override val name = "div"
    def value(a: Int, b: Int): Int = a / b
    val isInfix = false
  }

  private[this] case object Min extends BinaryOp {
    final val id = 12
    def value(a: Int, b: Int): Int = math.min(a, b)
    val isInfix = false
  }

  private[this] case object Max extends BinaryOp {
    final val id = 13
    def value(a: Int, b: Int): Int = math.max(a, b)
    val isInfix = false
  }

  private[this] case object BitAnd extends BinaryOp {
    final val id = 14
    def value(a: Int, b: Int): Int = a & b
    val isInfix = false
  }

  private[this] case object BitOr extends BinaryOp {
    final val id = 15
    def value(a: Int, b: Int): Int = a | b
    val isInfix = false
  }

  private[this] case object BitXor extends BinaryOp {
    final val id = 16
    def value(a: Int, b: Int): Int = a ^ b
    val isInfix = false
  }

  private[this] case object ShiftLeft extends BinaryOp {
    final val id = 26
    override val name = "<<"
    def value(a: Int, b: Int): Int = a << b
    val isInfix = false
  }

  private[this] case object ShiftRight extends BinaryOp {
    final val id = 27
    override val name = ">>"
    def value(a: Int, b: Int): Int = a >> b
    val isInfix = false
  }

  private[this] case object UnsignedShiftRight extends BinaryOp {
    final val id = 28
    override val name = ">>>"
    def value(a: Int, b: Int): Int = a >>> b
    val isInfix = false
  }

  private[this] case object Absdif extends BinaryOp {
    final val id = 38
    def value(a: Int, b: Int): Int = math.abs(a - b)
    val isInfix = false
  }

  //      case object Clip2          extends Op( 42 ) {
  //         def value( a: Int, b: Int ) : Int = ri_clip2( a, b )
  //      }
  //      case object Fold2          extends Op( 44 ) {
  //         def value( a: Int, b: Int ) : Int = ri_fold2( a, b )
  //      }
  //      case object Wrap2          extends Op( 45 ) {
  //         def value( a: Int, b: Int ) : Int = ri_wrap2( a, b )
  //      }

  final class Ops[S <: Sys[S]](val `this`: Ex[S]) extends AnyVal { me =>
    import me.{`this` => a}
    private type E = Ex[S]

    // ---- Int => Int ----

    def unary_- (implicit tx: S#Tx): E = Neg   (a)
    def unary_~ (implicit tx: S#Tx): E = BitNot(a)

    // ---- (Int, Int) => Int ----

    def +   (b: E)(implicit tx: S#Tx): E = Plus              (a, b)
    def -   (b: E)(implicit tx: S#Tx): E = Minus             (a, b)
    def *   (b: E)(implicit tx: S#Tx): E = Times             (a, b)
    def /   (b: E)(implicit tx: S#Tx): E = IDiv              (a, b)
    def &   (b: E)(implicit tx: S#Tx): E = BitAnd            (a, b)
    def |   (b: E)(implicit tx: S#Tx): E = BitOr             (a, b)
    def ^   (b: E)(implicit tx: S#Tx): E = BitXor            (a, b)
    def <<  (b: E)(implicit tx: S#Tx): E = ShiftLeft         (a, b)
    def >>  (b: E)(implicit tx: S#Tx): E = ShiftRight        (a, b)
    def >>> (b: E)(implicit tx: S#Tx): E = UnsignedShiftRight(a, b)

    // ---- (Int, Int) => Boolean ----

    def sig_==(b: E)(implicit tx: S#Tx): Expr[S, Boolean] = BooleanExtensions.IntEq (a, b)
    def sig_!=(b: E)(implicit tx: S#Tx): Expr[S, Boolean] = BooleanExtensions.IntNeq(a, b)
    def <     (b: E)(implicit tx: S#Tx): Expr[S, Boolean] = BooleanExtensions.IntLt (a, b)
    def >     (b: E)(implicit tx: S#Tx): Expr[S, Boolean] = BooleanExtensions.IntGt (a, b)
    def <=    (b: E)(implicit tx: S#Tx): Expr[S, Boolean] = BooleanExtensions.IntLeq(a, b)
    def >=    (b: E)(implicit tx: S#Tx): Expr[S, Boolean] = BooleanExtensions.IntGeq(a, b)
  }

  final class RichOps[S <: Sys[S]](val `this`: Ex[S]) extends AnyVal { me =>
    import me.{`this` => ex}
    private type E = Ex[S]

    def abs     (implicit tx: S#Tx): E = Abs     (ex)
    // def toLong : E	         = UnOp.make( 'asLong, ex )
    // def toInteger : E	      = UnOp.make( 'asInteger, ex )
    def signum  (implicit tx: S#Tx): E = Signum  (ex)
    def squared (implicit tx: S#Tx): E = Squared (ex)
    def cubed   (implicit tx: S#Tx): E = Cubed   (ex)

    def min   (b: E)(implicit tx: S#Tx): E = Min   (ex, b)
    def max   (b: E)(implicit tx: S#Tx): E = Max   (ex, b)
    def absdif(b: E)(implicit tx: S#Tx): E = Absdif(ex, b)

    //      def clip2( b: E ) : E      = Clip2.make( ex, b )
    //      def fold2( b: E ) : E      = Fold2.make( ex, b )
    //      def wrap2( b: E ) : E      = Wrap2.make( ex, b )
  }
}