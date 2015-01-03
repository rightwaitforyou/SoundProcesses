/*
 *  LongExtensions.scala
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

package de.sciss.lucre.synth
package expr

import de.sciss.lucre.{event => evt, expr}
import de.sciss.lucre.event.{Node, Targets, Sys}
import annotation.switch
import de.sciss.lucre.expr.{Type, Expr}
import de.sciss.serial.{DataOutput, DataInput}
import de.sciss.lucre

object LongExtensions /* extends BiTypeImpl[Long] */ {
  import de.sciss.lucre.expr.Long.{newConst, read, typeID, registerExtension}

  private[this] type Ex[S <: Sys[S]] = Expr[S, Long]

  registerExtension(1, LongTuple1s)
  registerExtension(2, LongTuple2s)

  private[this] object LongTuple1s extends Type.Extension1[({type Repr[~ <: Sys[~]] = Expr[~, Long]})#Repr] {
    final val arity = 1
    final val opLo  = UnaryOp.Neg  .id
    final val opHi  = UnaryOp.Cubed.id

    val name = "Long-Long Ops"

    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Expr.Node[S, Long] = {
      import UnaryOp._
      val op: Op[_] = (opID: @switch) match {
        // ---- Long ----
        case Neg    .id => Neg
        case BitNot .id => BitNot
        case Abs    .id => Abs
        case Signum .id => Signum
        case Squared.id => Squared
        case Cubed  .id => Cubed
      }
      op.read(in, access, targets)
    }
  }

  private[this] object LongTuple2s extends Type.Extension1[({type Repr[~ <: Sys[~]] = Expr[~, Long]})#Repr] {
    final val arity = 2
    final val opLo  = BinaryOp.Plus  .id
    final val opHi  = BinaryOp.Absdif.id

    val name = "Long-Long Ops"

    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Expr.Node[S, Long] = {
      import BinaryOp._
      val op: Op = (opID: @switch) match {
        // ---- Long ----
        case Plus   .id => Plus
        case Minus  .id => Minus
        case Times  .id => Times
        case IDiv   .id => IDiv
        //               case 4 => Div
        //               case 5 => Mod
        //      case 6 => Eq
        //      case 7 => Neq
        //      case 8 => Lt
        //      case 9 => Gt
        //      case 10 => Leq
        //      case 11 => Geq
        case Min    .id => Min
        case Max    .id => Max
        case BitAnd .id => BitAnd
        case BitOr  .id => BitOr
        case BitXor .id => BitXor
        // case 17 => Lcm
        // case 18 => Gcd
        //               case 19 => Round
        //               case 20 => Roundup
        //               case 26 => <<
        //               case 27 => >>
        //               case 28 => >>>
        case Absdif .id => Absdif
        //               case 42 => Clip2
        //               case 44 => Fold2
        //               case 45 => Wrap2
      }
      val _1 = read(in, access)
      val _2 = read(in, access)
      new impl.Tuple2(lucre.expr.Long, typeID, op, targets, _1, _2)
    }
  }

  // ---- operators ----

  object UnaryOp {
    trait Op[T1] extends impl.Tuple1Op[Long, T1] {
      def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                           (implicit tx: S#Tx): impl.Tuple1[S, Long, T1]

      def toString[S <: Sys[S]](_1: Expr[S, T1]): String = s"${_1}.$name"

      def apply[S <: Sys[S]](a: Expr[S, T1])(implicit tx: S#Tx): Ex[S] = a match {
        case Expr.Const(c)  => newConst(value(c))
        case _              => new impl.Tuple1(lucre.expr.Long, typeID, this, Targets.partial[S], a)
      }

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i  = cn.lastIndexOf('$', sz - 2) + 1
        "" + cn.charAt(i).toLower + cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
      }
    }

    sealed abstract class LongOp extends Op[Long] {
      // def id: Int

      final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                 (implicit tx: S#Tx): impl.Tuple1[S, Long, Long] = {
        val _1 = lucre.expr.Long.read(in, access)
        new impl.Tuple1(lucre.expr.Long, typeID, this, targets, _1)
      }
    }

    case object Neg extends LongOp {
      final val id = 0
      def value(a: Long): Long = -a
      override def toString[S <: Sys[S]](_1: Ex[S]): String = "-" + _1
    }

    case object Abs extends LongOp {
      final val id = 5
      def value(a: Long): Long = math.abs(a)
    }

    case object BitNot extends LongOp {
      final val id = 4
      def value(a: Long): Long = ~a
      override def toString[S <: Sys[S]](_1: Ex[S]): String = "~" + _1
    }

    // case object ToLong     extends Op(  6 )
    // case object ToInt       extends Op(  7 )

    case object Signum extends LongOp {
      final val id = 11
      def value(a: Long): Long = math.signum(a)
    }

    case object Squared extends LongOp {
      final val id = 12
      def value(a: Long): Long = a * a
    }

    case object Cubed extends LongOp {
      final val id = 13
      def value(a: Long): Long = a * a * a
    }
  }

  private object BinaryOp {
    sealed trait Op extends impl.Tuple2Op[Long, Long, Long] {
      final def apply[S <: Sys[S]](a: Ex[S], b: Ex[S])(implicit tx: S#Tx): Ex[S] = (a, b) match {
        case (Expr.Const(ca), Expr.Const(cb)) => newConst(value(ca, cb))
        case _                                => new impl.Tuple2(lucre.expr.Long, typeID, this, Targets.partial[S], a, b)
      }

      def value(a: Long, b: Long): Long

      def toString[S <: Sys[S]](_1: Ex[S], _2: Ex[S]): String = s"${_1}.$name(${_2})"

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i  = cn.indexOf('$') + 1
        "" + cn.charAt(i).toLower + cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
      }
    }

    trait Infix {
      _: Op =>

      override def toString[S <: Sys[S]](_1: Ex[S], _2: Ex[S]): String = s"(${_1} $name ${_2})"
    }

    case object Plus extends Op with Infix {
      final val id = 0
      override val name = "+"
      def value(a: Long, b: Long): Long = a + b
    }

    case object Minus extends Op with Infix {
      final val id = 1
      override val name = "-"
      def value(a: Long, b: Long): Long = a - b
    }

    case object Times extends Op with Infix {
      final val id = 2
      override val name = "*"
      def value(a: Long, b: Long): Long = a * b
    }

    case object IDiv extends Op {
      final val id = 3
      override val name = "div"
      def value(a: Long, b: Long): Long = a / b
    }

    case object Min extends Op {
      final val id = 12
      def value(a: Long, b: Long): Long = math.min(a, b)
    }

    case object Max extends Op {
      final val id = 13
      def value(a: Long, b: Long): Long = math.max(a, b)
    }

    case object BitAnd extends Op {
      final val id = 14
      def value(a: Long, b: Long): Long = a & b
    }

    case object BitOr extends Op {
      final val id = 15
      def value(a: Long, b: Long): Long = a | b
    }

    case object BitXor extends Op {
      final val id = 16
      def value(a: Long, b: Long): Long = a ^ b
    }

    //      case object <<             extends Op( 26 ) {
    //         def value( a: Long, b: Long ) : Long = a << b
    //      }
    //      case object >>             extends Op( 27 ) {
    //         def value( a: Long, b: Long ) : Long = a >> b
    //      }
    //      case object >>>            extends Op( 28 ) {
    //         def value( a: Long, b: Long ) : Long = a >>> b
    //      }
    case object Absdif extends Op {
      final val id = 38
      def value(a: Long, b: Long): Long = math.abs(a - b)
    }

    //      case object Clip2          extends Op( 42 ) {
    //         def value( a: Long, b: Long ) : Long = rd_clip2( a, b )
    //      }
    //      case object Fold2          extends Op( 44 ) {
    //         def value( a: Long, b: Long ) : Long = rd_fold2( a, b )
    //      }
    //      case object Wrap2          extends Op( 45 ) {
    //         def value( a: Long, b: Long ) : Long = rd_wrap2( a, b )
    //      }
  }

  final class Ops[S <: Sys[S]](val `this`: Ex[S]) extends AnyVal { me =>
    import me.{`this` => ex}
    private type E = Ex[S]

    import UnaryOp._

    def unary_- (implicit tx: S#Tx): E = Neg   (ex)
    def unary_~ (implicit tx: S#Tx): E = BitNot(ex)

    import BinaryOp._

    def + (b: E)(implicit tx: S#Tx): E = Plus  (ex, b)
    def - (b: E)(implicit tx: S#Tx): E = Minus (ex, b)
    def * (b: E)(implicit tx: S#Tx): E = Times (ex, b)
    def / (b: E)(implicit tx: S#Tx): E = IDiv  (ex, b)
    def & (b: E)(implicit tx: S#Tx): E = BitAnd(ex, b)
    def | (b: E)(implicit tx: S#Tx): E = BitOr (ex, b)
    def ^ (b: E)(implicit tx: S#Tx): E = BitXor(ex, b)
  }

  final class RichOps[S <: Sys[S]](val `this`: Ex[S]) extends AnyVal { me =>
    import me.{`this` => ex}
    private type E = Ex[S]

    import UnaryOp._

    def abs     (implicit tx: S#Tx): E = Abs     (ex)

    // def toLong : E	         = UnOp( 'asLong, ex )
    // def toInteger : E	      = UnOp( 'asInteger, ex )
    def signum  (implicit tx: S#Tx): E = Signum  (ex)
    def squared (implicit tx: S#Tx): E = Squared (ex)
    def cubed   (implicit tx: S#Tx): E = Cubed   (ex)

    import BinaryOp._

    def min   (b: E)(implicit tx: S#Tx): E = Min   (ex, b)
    def max   (b: E)(implicit tx: S#Tx): E = Max   (ex, b)
    def absdif(b: E)(implicit tx: S#Tx): E = Absdif(ex, b)

    //      def clip2( b: E ) : E      = Clip2( ex, b )
    //      def fold2( b: E ) : E      = Fold2( ex, b )
    //      def wrap2( b: E ) : E      = Wrap2( ex, b )
  }
}