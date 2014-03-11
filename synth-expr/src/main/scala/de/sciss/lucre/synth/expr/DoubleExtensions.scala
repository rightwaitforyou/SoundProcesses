/*
 *  DoubleExtensions.scala
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

import de.sciss.lucre
import de.sciss.lucre.{event => evt}
import annotation.switch
import de.sciss.numbers
import de.sciss.serial.{DataInput, DataOutput}
import de.sciss.lucre.expr.{Type, Expr}
import de.sciss.lucre.event.{Node, Targets, Sys}

object DoubleExtensions {
  import de.sciss.lucre.expr.Double.{newConst, read, typeID, registerExtension}

  private[this] type Ex[S <: Sys[S]] = Expr[S, Double]

  registerExtension(1, DoubleTuple1s)
  registerExtension(2, DoubleTuple2s)

  private[this] object DoubleTuple1s extends Type.Extension1[({type Repr[~ <: Sys[~]] = Expr[~, Double]})#Repr] {
    final val arity = 1
    final val opLo  = UnaryOp.Neg .id
    final val opHi  = UnaryOp.Tanh.id

    val name = "Double-Double Ops"


    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Expr.Node[S, Double] = {
      import UnaryOp._
      val op: Op = (opID: @switch) match {
        case Neg        .id => Neg
        case Abs        .id => Abs
        case Ceil       .id => Ceil
        case Floor      .id => Floor
        case Frac       .id => Frac
        case Signum     .id => Signum
        case Squared    .id => Squared
        // case Cubed      .id => Cubed
        case Sqrt       .id => Sqrt
        case Exp        .id => Exp
        case Reciprocal .id => Reciprocal
        case Midicps    .id => Midicps
        case Cpsmidi    .id => Cpsmidi
        case Midiratio  .id => Midiratio
        case Ratiomidi  .id => Ratiomidi
        case Dbamp      .id => Dbamp
        case Ampdb      .id => Ampdb
        case Octcps     .id => Octcps
        case Cpsoct     .id => Cpsoct
        case Log        .id => Log
        case Log2       .id => Log2
        case Log10      .id => Log10
        case Sin        .id => Sin
        case Cos        .id => Cos
        case Tan        .id => Tan
        case Asin       .id => Asin
        case Acos       .id => Acos
        case Atan       .id => Atan
        case Sinh       .id => Sinh
        case Cosh       .id => Cosh
        case Tanh       .id => Tanh
      }
      val _1 = read(in, access)
      new impl.Tuple1(lucre.expr.Double, typeID, op, targets, _1)
    }
  }

  private[this] object DoubleTuple2s extends Type.Extension1[({type Repr[~ <: Sys[~]] = Expr[~, Double]})#Repr] {
    final val arity = 2
    final val opLo  = BinaryOp.Plus .id
    final val opHi  = BinaryOp.Wrap2.id

    val name = "Double-Double Ops"

    def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                  (implicit tx: S#Tx): Expr.Node[S, Double] = {
      import BinaryOp._
      val op: Op = (opID: @switch) match {
        case Plus   .id => Plus
        case Minus  .id => Minus
        case Times  .id => Times
        //      case 3 => IDiv
        case Div    .id => Div
        case Mod    .id => Mod
        //      case 6 => Eq
        //      case 7 => Neq
        //      case 8 => Lt
        //      case 9 => Gt
        //      case 10 => Leq
        //      case 11 => Geq
        case Min    .id => Min
        case Max    .id => Max
        //      case 14 => BitAnd
        //      case 15 => BitOr
        //      case 16 => BitXor
        // case 17 => Lcm
        // case 18 => Gcd
        case RoundTo  .id => RoundTo
        case RoundUpTo.id => RoundUpTo
        case Trunc  .id => Trunc
        case Atan2  .id => Atan2
        case Hypot  .id => Hypot
        case Hypotx .id => Hypotx
        case Pow    .id => Pow
        // case 26 => <<
        // case 27 => >>
        // case 28 => UnsgnRghtShft
        // case 29 => Fill
        //      case 30 => Ring1
        //      case 31 => Ring2
        //      case 32 => Ring3
        //      case 33 => Ring4
        case Difsqr .id => Difsqr
        case Sumsqr .id => Sumsqr
        case Sqrsum .id => Sqrsum
        case Sqrdif .id => Sqrdif
        case Absdif .id => Absdif
        // case Thresh .id => Thresh
        //      case 40 => Amclip
        //      case 41 => Scaleneg
        case Clip2.id => Clip2
        //      case 43 => Excess
        case Fold2.id => Fold2
        case Wrap2.id => Wrap2
      }
      val _1 = read(in, access)
      val _2 = read(in, access)
      new impl.Tuple2(lucre.expr.Double, typeID, op, targets, _1, _2)
    }
  }

  // ----- operators -----

  private object UnaryOp {

    import de.sciss.numbers.{DoubleFunctions => rd}

    sealed abstract class Op extends impl.Tuple1Op[Double, Double] {
      def id: Int
      final def apply[S <: Sys[S]](a: Ex[S])(implicit tx: S#Tx): Ex[S] = a match {
        case Expr.Const(c)  => newConst(value(c))
        case _              => new impl.Tuple1(lucre.expr.Double, typeID, this, evt.Targets.partial[S], a)
      }

      //         def value( a: Double ) : Double

      def toString[S <: Sys[S]](_1: Ex[S]): String = s"${_1}.$name"

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i = cn.lastIndexOf('$', sz - 2) + 1
        "" + cn.charAt(i).toLower + cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
      }
    }

    case object Neg extends Op {
      final val id = 0
      def value(a: Double): Double = -a // rd.neg(a)

      override def toString[S <: Sys[S]](_1: Ex[S]): String = "-" + _1
    }

    case object Abs extends Op {
      final val id = 5
      def value(a: Double): Double = rd.abs(a)
    }

    // case object ToDouble     extends Op(  6 )
    // case object ToInt       extends Op(  7 )
    case object Ceil extends Op {
      final val id = 8
      def value(a: Double): Double = rd.ceil(a)
    }

    case object Floor extends Op {
      final val id = 9
      def value(a: Double): Double = rd.floor(a)
    }

    case object Frac extends Op {
      final val id = 10
      def value(a: Double): Double = rd.frac(a)
    }

    case object Signum extends Op {
      final val id = 11
      def value(a: Double): Double = rd.signum(a)
    }

    case object Squared extends Op {
      final val id = 12
      def value(a: Double): Double = rd.squared(a)
    }

    //    case object Cubed extends Op {
    //      final val id = 13
    //      def value(a: Double): Double = rd.cubed(a)
    //    }

    case object Sqrt extends Op {
      final val id = 14
      def value(a: Double): Double = rd.sqrt(a)
    }

    case object Exp extends Op {
      final val id = 15
      def value(a: Double): Double = rd.exp(a)
    }

    case object Reciprocal extends Op {
      final val id = 16
      def value(a: Double): Double = 1.0 / a // rd.reciprocal(a)
    }

    case object Midicps extends Op {
      final val id = 17
      def value(a: Double): Double = rd.midicps(a)
    }

    case object Cpsmidi extends Op {
      final val id = 18
      def value(a: Double): Double = rd.cpsmidi(a)
    }

    case object Midiratio extends Op {
      final val id = 19
      def value(a: Double): Double = rd.midiratio(a)
    }

    case object Ratiomidi extends Op {
      final val id = 20
      def value(a: Double): Double = rd.ratiomidi(a)
    }

    case object Dbamp extends Op {
      final val id = 21
      def value(a: Double): Double = rd.dbamp(a)
    }

    case object Ampdb extends Op {
      final val id = 22
      def value(a: Double): Double = rd.ampdb(a)
    }

    case object Octcps extends Op {
      final val id = 23
      def value(a: Double): Double = rd.octcps(a)
    }

    case object Cpsoct extends Op {
      final val id = 24
      def value(a: Double): Double = rd.cpsoct(a)
    }

    case object Log extends Op {
      final val id = 25
      def value(a: Double): Double = rd.log(a)
    }

    case object Log2 extends Op {
      final val id = 26
      def value(a: Double): Double = rd.log2(a)
    }

    case object Log10 extends Op {
      final val id = 27
      def value(a: Double): Double = rd.log10(a)
    }

    case object Sin extends Op {
      final val id = 28
      def value(a: Double): Double = rd.sin(a)
    }

    case object Cos extends Op {
      final val id = 29
      def value(a: Double): Double = rd.cos(a)
    }

    case object Tan extends Op {
      final val id = 30
      def value(a: Double): Double = rd.tan(a)
    }

    case object Asin extends Op {
      final val id = 31
      def value(a: Double): Double = rd.asin(a)
    }

    case object Acos extends Op {
      final val id = 32
      def value(a: Double): Double = rd.acos(a)
    }

    case object Atan extends Op {
      final val id = 33
      def value(a: Double): Double = rd.atan(a)
    }

    case object Sinh extends Op {
      final val id = 34
      def value(a: Double): Double = rd.sinh(a)
    }

    case object Cosh extends Op {
      final val id = 35
      def value(a: Double): Double = rd.cosh(a)
    }

    case object Tanh extends Op {
      final val id = 36
      def value(a: Double): Double = rd.tanh(a)
    }

    // class Rand              extends Op( 37 )
    // class Rand2             extends Op( 38 )
    // class Linrand           extends Op( 39 )
    // class Bilinrand         extends Op( 40 )
    // class Sum3rand          extends Op( 41 )
    // case object Distort     extends Op( 42 )
    // case object Softclip    extends Op( 43 )
    // class Coin              extends Op( 44 )
    // case object DigitValue  extends Op( 45 )
    // case object Silence     extends Op( 46 )
    // case object Thru        extends Op( 47 )
    // case object RectWindow  extends Op( 48 )
    // case object HanWindow   extends Op( 49 )
    // case object WelWindow   extends Op( 50 )
    // case object TriWindow   extends Op( 51 )
    // case object Ramp        extends Op( 52 )
    // case object Scurve      extends Op( 53 )
  }

  private object BinaryOp {

    import numbers.{DoubleFunctions => rd}

    sealed abstract class Op extends impl.Tuple2Op[Double, Double, Double] {
      def id: Int
      final def apply[S <: Sys[S]](a: Ex[S], b: Ex[S])(implicit tx: S#Tx): Ex[S] = (a, b) match {
        case (Expr.Const(ca), Expr.Const(cb)) => newConst(value(ca, cb))
        case _                                => new impl.Tuple2(lucre.expr.Double, typeID, this, evt.Targets.partial[S], a, b)
      }

      def value(a: Double, b: Double): Double

      def toString[S <: Sys[S]](_1: Ex[S], _2: Ex[S]): String = s"${_1}.$name(${_2})"

      def name: String = {
        val cn = getClass.getName
        val sz = cn.length
        val i = cn.lastIndexOf('$', sz - 2) + 1
        "" + cn.charAt(i).toLower + cn.substring(i + 1, if (cn.charAt(sz - 1) == '$') sz - 1 else sz)
      }
    }

    trait Infix {
      _: Op =>

      override def toString[S <: Sys[S]](_1: Ex[S], _2: Ex[S]): String =
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

      def value(a: Double, b: Double): Double = rd.+(a, b)
    }

    case object Minus extends Op with Infix {
      final val id = 1
      override val name = "-"

      def value(a: Double, b: Double): Double = rd.-(a, b)
    }

    case object Times extends Op with Infix {
      final val id = 2
      override val name = "*"

      def value(a: Double, b: Double): Double = rd.*(a, b)
    }

    //      case object IDiv           extends Op(  3 ) {
    //         override val name = "div"
    //         protected def make1( a: Double, b: Double ) : Int = rd.div( a, b )
    //      }
    case object Div extends Op with Infix {
      final val id = 4
      override val name = "/"

      def value(a: Double, b: Double): Double = rd./(a, b)
    }

    case object Mod extends Op with Infix {
      final val id = 5
      override val name = "%"

      def value(a: Double, b: Double): Double = rd.%(a, b)
    }

    //      case object Eq             extends Op(  6 )
    //      case object Neq            extends Op(  7 )
    //      case object Lt             extends Op(  8 )
    //      case object Gt             extends Op(  9 )
    //      case object Leq            extends Op( 10 )
    //      case object Geq            extends Op( 11 )
    case object Min extends Op {
      final val id = 12
      def value(a: Double, b: Double): Double = rd.min(a, b)
    }

    case object Max extends Op {
      final val id = 13
      def value(a: Double, b: Double): Double = rd.max(a, b)
    }

    //      case object BitAnd         extends Op( 14 )
    //      case object BitOr          extends Op( 15 )
    //      case object BitXor         extends Op( 16 )
    // case object Lcm            extends Op( 17 )
    // case object Gcd            extends Op( 18 )
    case object RoundTo extends Op {
      final val id = 19
      def value(a: Double, b: Double): Double = rd.roundTo(a, b)
    }

    case object RoundUpTo extends Op {
      final val id = 20
      def value(a: Double, b: Double): Double = rd.roundUpTo(a, b)
    }

    case object Trunc extends Op {
      final val id = 21
      def value(a: Double, b: Double): Double = rd.trunc(a, b)
    }

    case object Atan2 extends Op {
      final val id = 22
      def value(a: Double, b: Double): Double = rd.atan2(a, b)
    }

    case object Hypot extends Op {
      final val id = 23
      def value(a: Double, b: Double): Double = rd.hypot(a, b)
    }

    case object Hypotx extends Op {
      final val id = 24
      def value(a: Double, b: Double): Double = rd.hypotx(a, b)
    }

    case object Pow extends Op {
      final val id = 25
      def value(a: Double, b: Double): Double = rd.pow(a, b)
    }

    // case object <<             extends Op( 26 )
    // case object >>             extends Op( 27 )
    // case object UnsgnRghtShft  extends Op( 28 )
    // case object Fill           extends Op( 29 )
    //      case object Ring1          extends Op( 30 )
    //      case object Ring2          extends Op( 31 )
    //      case object Ring3          extends Op( 32 )
    //      case object Ring4          extends Op( 33 )
    case object Difsqr extends Op {
      final val id = 34
      def value(a: Double, b: Double): Double = rd.difsqr(a, b)
    }

    case object Sumsqr extends Op {
      final val id = 35
      def value(a: Double, b: Double): Double = rd.sumsqr(a, b)
    }

    case object Sqrsum extends Op {
      final val id = 36
      def value(a: Double, b: Double): Double = rd.sqrsum(a, b)
    }

    case object Sqrdif extends Op {
      final val id = 37
      def value(a: Double, b: Double): Double = rd.sqrdif(a, b)
    }

    case object Absdif extends Op {
      final val id = 38
      def value(a: Double, b: Double): Double = rd.absdif(a, b)
    }

    //    case object Thresh extends Op {
    //      final val id = 39
    //      def value(a: Double, b: Double): Double = rd.thresh(a, b)
    //    }

    //      case object Amclip         extends Op( 40 )
    //      case object Scaleneg       extends Op( 41 )
    case object Clip2 extends Op {
      final val id = 42
      def value(a: Double, b: Double): Double = rd.clip2(a, b)
    }

    //      case object Excess         extends Op( 43 )
    case object Fold2 extends Op {
      final val id = 44
      def value(a: Double, b: Double): Double = rd.fold2(a, b)
    }

    case object Wrap2 extends Op {
      final val id = 45
      def value(a: Double, b: Double): Double = rd.wrap2(a, b)
    }

    //      case object Firstarg       extends Op( 46 )
  }

  final class Ops[S <: Sys[S]](val `this`: Ex[S]) extends AnyVal { me =>
    import me.{`this` => ex}
    private type E = Ex[S]

    import UnaryOp._

    def unary_- (implicit tx: S#Tx): E = Neg(ex)

    // def bitNot : E	         = BitNot.make( ex )
    // def toDouble : E	         = UnOp.make( 'asDouble, ex )
    // def toInteger : E	      = UnOp.make( 'asInteger, ex )

    import BinaryOp._

    def +(b: E)(implicit tx: S#Tx): E = Plus(ex, b)

    def -(b: E)(implicit tx: S#Tx): E = Minus(ex, b)

    def *(b: E)(implicit tx: S#Tx): E = Times(ex, b)

    def /(b: E)(implicit tx: S#Tx): E = Div(ex, b)
  }

  final class RichOps[S <: Sys[S]](val `this`: Ex[S]) extends AnyVal { me =>
    import me.{`this` => ex}

    private type E = Ex[S]

    import UnaryOp._

    def abs       (implicit tx: S#Tx): E = Abs       (ex)
    def ceil      (implicit tx: S#Tx): E = Ceil      (ex)
    def floor     (implicit tx: S#Tx): E = Floor     (ex)
    def frac      (implicit tx: S#Tx): E = Frac      (ex)
    def signum    (implicit tx: S#Tx): E = Signum    (ex)
    def squared   (implicit tx: S#Tx): E = Squared   (ex)
    // def cubed     : E = Cubed     (ex)
    def sqrt      (implicit tx: S#Tx): E = Sqrt      (ex)
    def exp       (implicit tx: S#Tx): E = Exp       (ex)
    def reciprocal(implicit tx: S#Tx): E = Reciprocal(ex)
    def midicps   (implicit tx: S#Tx): E = Midicps   (ex)
    def cpsmidi   (implicit tx: S#Tx): E = Cpsmidi   (ex)
    def midiratio (implicit tx: S#Tx): E = Midiratio (ex)
    def ratiomidi (implicit tx: S#Tx): E = Ratiomidi (ex)
    def dbamp     (implicit tx: S#Tx): E = Dbamp     (ex)
    def ampdb     (implicit tx: S#Tx): E = Ampdb     (ex)
    def octcps    (implicit tx: S#Tx): E = Octcps    (ex)
    def cpsoct    (implicit tx: S#Tx): E = Cpsoct    (ex)
    def log       (implicit tx: S#Tx): E = Log       (ex)
    def log2      (implicit tx: S#Tx): E = Log2      (ex)
    def log10     (implicit tx: S#Tx): E = Log10     (ex)
    def sin       (implicit tx: S#Tx): E = Sin       (ex)
    def cos       (implicit tx: S#Tx): E = Cos       (ex)
    def tan       (implicit tx: S#Tx): E = Tan       (ex)
    def asin      (implicit tx: S#Tx): E = Asin      (ex)
    def acos      (implicit tx: S#Tx): E = Acos      (ex)
    def atan      (implicit tx: S#Tx): E = Atan      (ex)
    def sinh      (implicit tx: S#Tx): E = Sinh      (ex)
    def cosh      (implicit tx: S#Tx): E = Cosh      (ex)
    def tanh      (implicit tx: S#Tx): E = Tanh      (ex)

    // def rand : E              = UnOp.make( 'rand, ex )
    // def rand2 : E             = UnOp.make( 'rand2, ex )
    // def linrand : E           = UnOp.make( 'linrand, ex )
    // def bilinrand : E         = UnOp.make( 'bilinrand, ex )
    // def sum3rand : E          = UnOp.make( 'sum3rand, ex )
    // def distort : E   = Distort.make( ex )
    // def softclip : E  = Softclip.make( ex )
    // def coin : E              = UnOp.make( 'coin, ex )
    // def even : E              = UnOp.make( 'even, ex )
    // def odd : E               = UnOp.make( 'odd, ex )
    // def rectWindow : E        = UnOp.make( 'rectWindow, ex )
    // def hanWindow : E         = UnOp.make( 'hanWindow, ex )
    // def welWindow : E         = UnOp.make( 'sum3rand, ex )
    // def triWindow : E         = UnOp.make( 'triWindow, ex )
    // def ramp : E      = Ramp.make( ex )
    // def scurve : E    = Scurve.make( ex )
    // def isPositive : E        = UnOp.make( 'isPositive, ex )
    // def isNegative : E        = UnOp.make( 'isNegative, ex )
    // def isStrictlyPositive : E= UnOp.make( 'isStrictlyPositive, ex )
    // def rho : E               = UnOp.make( 'rho, ex )
    // def theta : E             = UnOp.make( 'theta, ex )

    import BinaryOp._

    def min     (b: E)(implicit tx: S#Tx): E = Min     (ex, b)
    def max     (b: E)(implicit tx: S#Tx): E = Max     (ex, b)
    def round   (b: E)(implicit tx: S#Tx): E = RoundTo   (ex, b)
    def roundup (b: E)(implicit tx: S#Tx): E = RoundUpTo (ex, b)
    def trunc   (b: E)(implicit tx: S#Tx): E = Trunc   (ex, b)
    def atan2   (b: E)(implicit tx: S#Tx): E = Atan2   (ex, b)
    def hypot   (b: E)(implicit tx: S#Tx): E = Hypot   (ex, b)
    def hypotx  (b: E)(implicit tx: S#Tx): E = Hypotx  (ex, b)
    def pow     (b: E)(implicit tx: S#Tx): E = Pow     (ex, b)

    //      def ring1( b: E ) : E     = Ring1.make( ex, b )
    //      def ring2( b: E ) : E     = Ring2.make( ex, b )
    //      def ring3( b: E ) : E     = Ring3.make( ex, b )
    //      def ring4( b: E ) : E     = Ring4.make( ex, b )
    def difsqr  (b: E)(implicit tx: S#Tx): E = Difsqr  (ex, b)
    def sumsqr  (b: E)(implicit tx: S#Tx): E = Sumsqr  (ex, b)
    def sqrsum  (b: E)(implicit tx: S#Tx): E = Sqrsum  (ex, b)
    def sqrdif  (b: E)(implicit tx: S#Tx): E = Sqrdif  (ex, b)
    def absdif  (b: E)(implicit tx: S#Tx): E = Absdif  (ex, b)
    // def thresh  (b: E): E = Thresh  (ex, b)

    //      def amclip( b: E ) : E    = Amclip.make( ex, b )
    //      def scaleneg( b: E ) : E  = Scaleneg.make( ex, b )
    def clip2   (b: E)(implicit tx: S#Tx): E = Clip2   (ex, b)

    //      def excess( b: E ) : E    = Excess.make( ex, b )
    def fold2   (b: E)(implicit tx: S#Tx): E = Fold2   (ex, b)
    def wrap2   (b: E)(implicit tx: S#Tx): E = Wrap2   (ex, b)

    // def firstarg( b: Double ) : Double  = d

    //      def linlin( srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double ) : Double =
    //         rd.linlin( d, srcLo, srcHi, dstLo, dstHi )
    //
    //      def linexp( srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double ) : Double =
    //         rd.linexp( d, srcLo, srcHi, dstLo, dstHi )
  }
}