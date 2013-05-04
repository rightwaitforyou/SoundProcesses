/*
 *  Ints.scala
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
import evt.{Targets, Sys}
import annotation.switch
import expr.Expr
import de.sciss.serial.{DataOutput, DataInput}

object Ints extends BiTypeImpl[Int] {
  final val typeID = 2

  /* protected */ def readValue(in: DataInput): Int = in.readInt()

  /* protected */ def writeValue(value: Int, out: DataOutput) {
    out.writeInt(value)
  }

  def readTuple[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                            (implicit tx: S#Tx): ExN[S] = {
    (cookie: @switch) match {
      case 1 =>
        val tpe = in.readInt()
        require(tpe == typeID, "Invalid type id (found " + tpe + ", required " + typeID + ")")
        val opID = in.readInt()
        import UnaryOp._
        val op: Op[_] = (opID: @switch) match {
          // ---- Int ----
          case Neg    .id => Neg
          case BitNot .id => BitNot
          case Abs    .id => Abs
          case Signum .id => Signum
          case Squared.id => Squared
          case Cubed  .id => Cubed

          case _ => sys.error("Invalid operation id " + opID)
        }
        op.read(in, access, targets)
      //            val _1 = readExpr( in, access )
      //            new Tuple1( typeID, op, targets, _1 )

      case 2 =>
        val tpe = in.readInt()
        require(tpe == typeID, "Invalid type id (found " + tpe + ", required " + typeID + ")")
        val opID = in.readInt()
        import BinaryOp._
        val op: Op = (opID: @switch) match {
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
        val _1 = readExpr(in, access)
        val _2 = readExpr(in, access)
        new Tuple2(typeID, op, targets, _1, _2)

      //         case 3 =>
      //            readProjection[ S ]( in, access, targets )

      case _ => sys.error("Invalid cookie " + cookie)
    }
  }

  object UnaryOp {
    /* sealed */ trait Op[T1] extends Tuple1Op[T1] {
      def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                           (implicit tx: S#Tx): Tuple1[S, T1]

      def toString[S <: stm.Sys[S]](_1: Expr[S, T1]): String = _1.toString + "." + name

      def apply[S <: Sys[S]](a: Expr[S, T1])(implicit tx: S#Tx): Ex[S] = a match {
        case Expr.Const(c)  => newConst(value(c))
        case _              => new Tuple1(typeID, this, Targets.partial[S], a)
      }

      def name: String = {
        val cn  = getClass.getName
        val sz  = cn.length
        val i   = cn.lastIndexOf('$', sz - 2) + 1
        "" + cn.charAt(i).toLower + cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
      }
    }

    sealed abstract class IntOp extends Op[Int] {
      def id: Int
      final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                 (implicit tx: S#Tx): Tuple1[S, Int] = {
        val _1 = readExpr(in, access)
        new Tuple1(typeID, this, targets, _1)
      }
    }

    case object Neg extends IntOp {
      final val id = 0
      def value(a: Int): Int = -a
      override def toString[S <: stm.Sys[S]](_1: Ex[S]): String = "-" + _1
    }

    case object Abs extends IntOp {
      final val id = 5
      def value(a: Int): Int = math.abs(a)
    }

    case object BitNot extends IntOp {
      final val id = 4
      def value(a: Int): Int = ~a
      override def toString[S <: stm.Sys[S]](_1: Ex[S]): String = "~" + _1
    }

    // case object ToLong     extends Op(  6 )
    // case object ToInt       extends Op(  7 )
    case object Signum extends IntOp {
      final val id = 11
      def value(a: Int): Int = math.signum(a)
    }

    case object Squared extends IntOp {
      final val id = 12
      def value(a: Int): Int = a * a
    }

    case object Cubed extends IntOp {
      final val id = 13
      def value(a: Int): Int = a * a * a
    }
  }

  object BinaryOp {
    sealed abstract class Op extends Tuple2Op[Int, Int] {
      def id: Int

      final def apply[S <: Sys[S]](a: Ex[S], b: Ex[S])(implicit tx: S#Tx): Ex[S] = (a, b) match {
        case (Expr.Const(ca), Expr.Const(cb)) => newConst(value(ca, cb))
        case _                                => new Tuple2(typeID, this, Targets.partial[S], a, b)
      }

      def value(a: Int, b: Int): Int

      def toString[S <: stm.Sys[S]](_1: Ex[S], _2: Ex[S]): String = s"${_1}.$name(${_2})"

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i  = cn.indexOf('$') + 1
        "" + cn.charAt(i).toLower + cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
      }
    }

    trait Infix {
      _: Op =>

      override def toString[S <: stm.Sys[S]](_1: Ex[S], _2: Ex[S]): String =
        "(" + _1 + " " + name + " " + _2 + ")"
    }

    //      sealed trait MathStyle {
    //         def name: String
    //         override def toString[ S <: Sys[ S ]]( _1: Ex[ S ], _2: Ex[ S ]) : String =
    //            "(" + _1 + " " + name + " " + _2 + ")"
    //      }

    case object Plus extends Op with Infix {
      final val id = 0
      override val name = "+"
      def value(a: Int, b: Int): Int = a + b
    }

    case object Minus extends Op with Infix {
      final val id = 1
      override val name = "-"
      def value(a: Int, b: Int): Int = a - b
    }

    case object Times extends Op with Infix {
      final val id = 2
      override val name = "*"
      def value(a: Int, b: Int): Int = a * b
    }

    case object IDiv extends Op {
      final val id = 3
      override val name = "div"
      def value(a: Int, b: Int): Int = a / b
    }

    case object Min extends Op {
      final val id = 12
      def value(a: Int, b: Int): Int = math.min(a, b)
    }

    case object Max extends Op {
      final val id = 13
      def value(a: Int, b: Int): Int = math.max(a, b)
    }

    case object BitAnd extends Op {
      final val id = 14
      def value(a: Int, b: Int): Int = a & b
    }

    case object BitOr extends Op {
      final val id = 15
      def value(a: Int, b: Int): Int = a | b
    }

    case object BitXor extends Op {
      final val id = 16
      def value(a: Int, b: Int): Int = a ^ b
    }

    case object ShiftLeft extends Op {
      final val id = 26
      override val name = "<<"
      def value(a: Int, b: Int): Int = a << b
    }

    case object ShiftRight extends Op {
      final val id = 27
      override val name = ">>"
      def value(a: Int, b: Int): Int = a >> b
    }

    case object UnsignedShiftRight extends Op {
      final val id = 28
      override val name = ">>>"
      def value(a: Int, b: Int): Int = a >>> b
    }

    case object Absdif extends Op {
      final val id = 38
      def value(a: Int, b: Int): Int = math.abs(a - b)
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
  }

  final class Ops[S <: Sys[S]](ex: Ex[S])(implicit tx: S#Tx) {
    private type E = Ex[S]

    import UnaryOp._

    def unary_- : E = Neg   (ex)
    def unary_~ : E = BitNot(ex)

    import BinaryOp._

    def +   (b: E): E = Plus              (ex, b)
    def -   (b: E): E = Minus             (ex, b)
    def *   (b: E): E = Times             (ex, b)
    def /   (b: E): E = IDiv              (ex, b)
    def &   (b: E): E = BitAnd            (ex, b)
    def |   (b: E): E = BitOr             (ex, b)
    def ^   (b: E): E = BitXor            (ex, b)
    def <<  (b: E): E = ShiftLeft         (ex, b)
    def >>  (b: E): E = ShiftRight        (ex, b)
    def >>> (b: E): E = UnsignedShiftRight(ex, b)
  }

  final class RichOps[S <: Sys[S]](ex: Ex[S])(implicit tx: S#Tx) {
    private type E = Ex[S]

    import UnaryOp._

    def abs     : E = Abs     (ex)
    // def toLong : E	         = UnOp.make( 'asLong, ex )
    // def toInteger : E	      = UnOp.make( 'asInteger, ex )
    def signum  : E = Signum  (ex)
    def squared : E = Squared (ex)
    def cubed   : E = Cubed   (ex)

    import BinaryOp._

    def min   (b: E): E = Min   (ex, b)
    def max   (b: E): E = Max   (ex, b)
    def absdif(b: E): E = Absdif(ex, b)

    //      def clip2( b: E ) : E      = Clip2.make( ex, b )
    //      def fold2( b: E ) : E      = Fold2.make( ex, b )
    //      def wrap2( b: E ) : E      = Wrap2.make( ex, b )
  }

}