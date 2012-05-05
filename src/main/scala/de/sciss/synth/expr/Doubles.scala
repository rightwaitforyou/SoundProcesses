/*
 *  Doubles.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2012 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.lucre.{DataInput, DataOutput}
import de.sciss.lucre.stm.Sys
import de.sciss.synth.RichDouble
import de.sciss.lucre.expr.Type
import de.sciss.lucre.event.Targets
import annotation.switch

// typeIDs : 0 = byte, 1 = short, 2 = int, 3 = long, 4 = float, 5 = double, 6 = boolean, 7 = char,
//           8 = string
object Doubles extends BiTypeImpl[ Double ] {
   private val typeID = 5

   /* protected */ def readValue( in: DataInput ) : Double = in.readDouble()
   /* protected */ def writeValue( value: Double, out: DataOutput ) { out.writeDouble( value )}

   def readTuple[ S <: Sys[ S ]]( cookie: Int, in: DataInput, access: S#Acc, targets: Targets[ S ])
                                ( implicit tx: S#Tx ) : Ex[ S ] = {
      (cookie: @switch) match {
         case 1 =>
            val tpe  = in.readInt()
            require( tpe == typeID, "Invalid type id (found " + tpe + ", required " + typeID + ")" )
            val opID = in.readInt()
            import UnaryOp._
            val op: Op = (opID: @switch) match {
               case 0  => Neg
               case 5  => Abs
               case 8  => Ceil
               case 9  => Floor
               case 10 => Frac
               case 11 => Signum
               case 12 => Squared
               case 13 => Cubed
               case 14 => Sqrt
               case 15 => Exp
               case 16 => Reciprocal
               case 17 => Midicps
               case 18 => Cpsmidi
               case 19 => Midiratio
               case 20 => Ratiomidi
               case 21 => Dbamp
               case 22 => Ampdb
               case 23 => Octcps
               case 24 => Cpsoct
               case 25 => Log
               case 26 => Log2
               case 27 => Log10
               case 28 => Sin
               case 29 => Cos
               case 30 => Tan
               case 31 => Asin
               case 32 => Acos
               case 33 => Atan
               case 34 => Sinh
               case 35 => Cosh
               case 36 => Tanh
               case _  => sys.error( "Invalid operation id " + opID )
            }
            val _1 = readExpr( in, access )
            new Tuple1( typeID, op, targets, _1 )

         case 2 =>
            val tpe = in.readInt()
            require( tpe == typeID, "Invalid type id (found " + tpe + ", required " + typeID + ")" )
            val opID = in.readInt()
            import BinaryOp._
            val op: Op = (opID: @switch) match {
               case 0 => Plus
               case 1 => Minus
               case 2 => Times
         //      case 3 => IDiv
               case 4 => Div
               case 5 => Mod
         //      case 6 => Eq
         //      case 7 => Neq
         //      case 8 => Lt
         //      case 9 => Gt
         //      case 10 => Leq
         //      case 11 => Geq
               case 12 => Min
               case 13 => Max
         //      case 14 => BitAnd
         //      case 15 => BitOr
         //      case 16 => BitXor
            // case 17 => Lcm
            // case 18 => Gcd
               case 19 => Round
               case 20 => Roundup
               case 21 => Trunc
               case 22 => Atan2
               case 23 => Hypot
               case 24 => Hypotx
               case 25 => Pow
            // case 26 => <<
            // case 27 => >>
            // case 28 => UnsgnRghtShft
            // case 29 => Fill
         //      case 30 => Ring1
         //      case 31 => Ring2
         //      case 32 => Ring3
         //      case 33 => Ring4
               case 34 => Difsqr
               case 35 => Sumsqr
               case 36 => Sqrsum
               case 37 => Sqrdif
               case 38 => Absdif
               case 39 => Thresh
         //      case 40 => Amclip
         //      case 41 => Scaleneg
               case 42 => Clip2
         //      case 43 => Excess
               case 44 => Fold2
               case 45 => Wrap2
            }
            val _1 = readExpr( in, access )
            val _2 = readExpr( in, access )
            new Tuple2( typeID, op, targets, _1, _2 )

         case 3 =>
            readCursor[ S ]( in, access, targets )

         case _ => sys.error( "Invalid cookie " + cookie )
      }
   }

   private object UnaryOp {
      import RichDouble._

      sealed abstract class Op( val id: Int ) extends Tuple1Op[ Double ] {
         final def make[ S <: Sys[ S ]]( a: Ex[ S ])( implicit tx: S#Tx ) : Ex[ S ] = {
            new Tuple1( typeID, this, Targets.partial[ S ], a )
         }

         def value( a: Double ) : Double

         def toString[ S <: Sys[ S ]]( _1: Ex[ S ]) : String = _1.toString + "." + name

         def name: String = { val cn = getClass.getName
            val sz   = cn.length
            val i    = cn.indexOf( '$' ) + 1
            "" + cn.charAt( i ).toLower + cn.substring( i + 1, if( cn.charAt( sz - 1 ) == '$' ) sz - 1 else sz )
         }
      }
      
      case object Neg extends Op( 0 ) {
         def value( a: Double ) : Double = rd_neg( a )
         override def toString[ S <: Sys[ S ]]( _1: Ex[ S ]) : String = "-" + _1
      }
      case object Abs         extends Op(  5 ) {
         def value( a: Double ) : Double = rd_abs( a )
      }
   // case object ToDouble     extends Op(  6 )
   // case object ToInt       extends Op(  7 )
      case object Ceil        extends Op(  8 ) {
         def value( a: Double ) : Double = rd_ceil( a )
      }
      case object Floor       extends Op(  9 ) {
         def value( a: Double ) : Double = rd_floor( a )
      }
      case object Frac        extends Op( 10 ) {
         def value( a: Double ) : Double = rd_frac( a )
      }
      case object Signum      extends Op( 11 ) {
         def value( a: Double ) : Double = rd_signum( a )
      }
      case object Squared     extends Op( 12 ) {
         def value( a: Double ) : Double = rd_squared( a )
      }
      case object Cubed       extends Op( 13 ) {
         def value( a: Double ) : Double = rd_cubed( a )
      }
      case object Sqrt        extends Op( 14 ) {
         def value( a: Double ) : Double = rd_sqrt( a )
      }
      case object Exp         extends Op( 15 ) {
         def value( a: Double ) : Double = rd_exp( a )
      }
      case object Reciprocal  extends Op( 16 ) {
         def value( a: Double ) : Double = rd_reciprocal( a )
      }
      case object Midicps     extends Op( 17 ) {
         def value( a: Double ) : Double = rd_midicps( a )
      }
      case object Cpsmidi     extends Op( 18 ) {
         def value( a: Double ) : Double = rd_cpsmidi( a )
      }
      case object Midiratio   extends Op( 19 ) {
         def value( a: Double ) : Double = rd_midiratio( a )
      }
      case object Ratiomidi   extends Op( 20 ) {
         def value( a: Double ) : Double = rd_ratiomidi( a )
      }
      case object Dbamp       extends Op( 21 ) {
         def value( a: Double ) : Double = rd_dbamp( a )
      }
      case object Ampdb       extends Op( 22 ) {
         def value( a: Double ) : Double = rd_ampdb( a )
      }
      case object Octcps      extends Op( 23 ) {
         def value( a: Double ) : Double = rd_octcps( a )
      }
      case object Cpsoct      extends Op( 24 ) {
         def value( a: Double ) : Double = rd_cpsoct( a )
      }
      case object Log         extends Op( 25 ) {
         def value( a: Double ) : Double = rd_log( a )
      }
      case object Log2        extends Op( 26 ) {
         def value( a: Double ) : Double = rd_log2( a )
      }
      case object Log10       extends Op( 27 ) {
         def value( a: Double ) : Double = rd_log10( a )
      }
      case object Sin         extends Op( 28 ) {
         def value( a: Double ) : Double = rd_sin( a )
      }
      case object Cos         extends Op( 29 ) {
         def value( a: Double ) : Double = rd_cos( a )
      }
      case object Tan         extends Op( 30 ) {
         def value( a: Double ) : Double = rd_tan( a )
      }
      case object Asin        extends Op( 31 ) {
         def value( a: Double ) : Double = rd_asin( a )
      }
      case object Acos        extends Op( 32 ) {
         def value( a: Double ) : Double = rd_acos( a )
      }
      case object Atan        extends Op( 33 ) {
         def value( a: Double ) : Double = rd_atan( a )
      }
      case object Sinh        extends Op( 34 ) {
         def value( a: Double ) : Double = rd_sinh( a )
      }
      case object Cosh        extends Op( 35 ) {
         def value( a: Double ) : Double = rd_cosh( a )
      }
      case object Tanh        extends Op( 36 ) {
         def value( a: Double ) : Double = rd_tanh( a )
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
      import RichDouble._

      sealed abstract class Op( val id: Int ) extends Tuple2Op[ Double, Double ] {
         final def make[ S <: Sys[ S ]]( a: Ex[ S ], b: Ex[ S ])( implicit tx: S#Tx ) : Ex[ S ] = {
            new Tuple2( typeID, this, Targets.partial[ S ], a, b )
         }
         def value( a: Double, b: Double ) : Double

         def toString[ S <: Sys[ S ]]( _1: Ex[ S ], _2: Ex[ S ]) : String = _1.toString + "." + name + "(" + _2 + ")"

         def name: String = { val cn = getClass.getName
            val sz   = cn.length
            val i    = cn.indexOf( '$' ) + 1
            "" + cn.charAt( i ).toLower + cn.substring( i + 1, if( cn.charAt( sz - 1 ) == '$' ) sz - 1 else sz )
         }
      }

      trait Infix {
         _: Op =>

         override def toString[ S <: Sys[ S ]]( _1: Ex[ S ], _2: Ex[ S ]) : String =
            "(" + _1 + " " + name + " " + _2 + ")"
      }

//      sealed trait MathStyle {
//         def name: String
//         override def toString[ S <: Sys[ S ]]( _1: Ex[ S ], _2: Ex[ S ]) : String =
//            "(" + _1 + " " + name + " " + _2 + ")"
//      }

      case object Plus           extends Op(  0 ) with Infix {
         override val name = "+"
         def value( a: Double, b: Double ) : Double = rd_+( a, b )
      }
      case object Minus          extends Op(  1 ) with Infix {
         override val name = "-"
         def value( a: Double, b: Double ) : Double = rd_-( a, b )
      }
      case object Times          extends Op(  2 ) with Infix {
         override val name = "*"
         def value( a: Double, b: Double ) : Double = rd_*( a, b )
      }
//      case object IDiv           extends Op(  3 ) {
//         override val name = "div"
//         protected def make1( a: Double, b: Double ) : Int = rd_div( a, b )
//      }
      case object Div            extends Op(  4 ) with Infix {
         override val name = "/"
         def value( a: Double, b: Double ) : Double = rd_/( a, b )
      }
      case object Mod            extends Op(  5 ) with Infix {
         override val name = "%"
         def value( a: Double, b: Double ) : Double = rd_%( a, b )
      }
//      case object Eq             extends Op(  6 )
//      case object Neq            extends Op(  7 )
//      case object Lt             extends Op(  8 )
//      case object Gt             extends Op(  9 )
//      case object Leq            extends Op( 10 )
//      case object Geq            extends Op( 11 )
      case object Min            extends Op( 12 ) {
         def value( a: Double, b: Double ) : Double = rd_min( a, b )
      }
      case object Max            extends Op( 13 ) {
         def value( a: Double, b: Double ) : Double = rd_max( a, b )
      }
//      case object BitAnd         extends Op( 14 )
//      case object BitOr          extends Op( 15 )
//      case object BitXor         extends Op( 16 )
   // case object Lcm            extends Op( 17 )
   // case object Gcd            extends Op( 18 )
      case object Round          extends Op( 19 ) {
         def value( a: Double, b: Double ) : Double = rd_round( a, b )
      }
      case object Roundup        extends Op( 20 ) {
         def value( a: Double, b: Double ) : Double = rd_roundup( a, b )
      }
      case object Trunc          extends Op( 21 ) {
         def value( a: Double, b: Double ) : Double = rd_trunc( a, b )
      }
      case object Atan2          extends Op( 22 ) {
         def value( a: Double, b: Double ) : Double = rd_atan2( a, b )
      }
      case object Hypot          extends Op( 23 ) {
         def value( a: Double, b: Double ) : Double = rd_hypot( a, b )
      }
      case object Hypotx         extends Op( 24 ) {
         def value( a: Double, b: Double ) : Double = rd_hypotx( a, b )
      }
      case object Pow            extends Op( 25 ) {
         def value( a: Double, b: Double ) : Double = rd_pow( a, b )
      }
   // case object <<             extends Op( 26 )
   // case object >>             extends Op( 27 )
   // case object UnsgnRghtShft  extends Op( 28 )
   // case object Fill           extends Op( 29 )
//      case object Ring1          extends Op( 30 )
//      case object Ring2          extends Op( 31 )
//      case object Ring3          extends Op( 32 )
//      case object Ring4          extends Op( 33 )
      case object Difsqr         extends Op( 34 ) {
         def value( a: Double, b: Double ) : Double = rd_difsqr( a, b )
      }
      case object Sumsqr         extends Op( 35 ) {
         def value( a: Double, b: Double ) : Double = rd_sumsqr( a, b )
      }
      case object Sqrsum         extends Op( 36 ) {
         def value( a: Double, b: Double ) : Double = rd_sqrsum( a, b )
      }
      case object Sqrdif         extends Op( 37 ) {
         def value( a: Double, b: Double ) : Double = rd_sqrdif( a, b )
      }
      case object Absdif         extends Op( 38 ) {
         def value( a: Double, b: Double ) : Double = rd_absdif( a, b )
      }
      case object Thresh         extends Op( 39 ) {
         def value( a: Double, b: Double ) : Double = rd_thresh( a, b )
      }
//      case object Amclip         extends Op( 40 )
//      case object Scaleneg       extends Op( 41 )
      case object Clip2          extends Op( 42 ) {
         def value( a: Double, b: Double ) : Double = rd_clip2( a, b )
      }
//      case object Excess         extends Op( 43 )
      case object Fold2          extends Op( 44 ) {
         def value( a: Double, b: Double ) : Double = rd_fold2( a, b )
      }
      case object Wrap2          extends Op( 45 ) {
         def value( a: Double, b: Double ) : Double = rd_wrap2( a, b )
      }
//      case object Firstarg       extends Op( 46 )
   }

   final class Ops[ S <: Sys[ S ]]( ex: Ex[ S ])( implicit tx: S#Tx ) {
      private type E = Ex[ S ]

      import UnaryOp._
      def unary_- : E   = Neg.make( ex )
   // def bitNot : E	         = BitNot.make( ex )
   // def toDouble : E	         = UnOp.make( 'asDouble, ex )
   // def toInteger : E	      = UnOp.make( 'asInteger, ex )

      import BinaryOp._

      def +( b: E ) : E          = Plus.make( ex, b )
      def -( b: E ) : E          = Minus.make( ex, b )
      def *( b: E ) : E          = Times.make( ex, b )
      def /( b: E ) : E          = Div.make( ex, b )
   }

   final class RichOps[ S <: Sys[ S ]]( ex: Ex[ S ])( implicit tx: S#Tx ) {
      private type E = Ex[ S ]
      
      import UnaryOp._
      def abs : E       = Abs.make( ex )
      def ceil : E      = Ceil.make( ex )
      def floor : E     = Floor.make( ex )
      def frac : E      = Frac.make( ex )
      def signum : E    = Signum.make( ex )
      def squared : E   = Squared.make( ex )
      def cubed : E     = Cubed.make( ex )
      def sqrt : E      = Sqrt.make( ex )
      def exp : E       = Exp.make( ex )
      def reciprocal : E= Reciprocal.make( ex )
      def midicps : E   = Midicps.make( ex )
      def cpsmidi : E   = Cpsmidi.make( ex )
      def midiratio : E = Midiratio.make( ex )
      def ratiomidi : E = Ratiomidi.make( ex )
      def dbamp : E     = Dbamp.make( ex )
      def ampdb : E     = Ampdb.make( ex )
      def octcps : E    = Octcps.make( ex )
      def cpsoct : E    = Cpsoct.make( ex )
      def log : E       = Log.make( ex )
      def log2 : E      = Log2.make( ex )
      def log10 : E     = Log10.make( ex )
      def sin : E       = Sin.make( ex )
      def cos : E       = Cos.make( ex )
      def tan : E       = Tan.make( ex )
      def asin : E      = Asin.make( ex )
      def acos : E      = Acos.make( ex )
      def atan : E      = Atan.make( ex )
      def sinh : E      = Sinh.make( ex )
      def cosh : E      = Cosh.make( ex )
      def tanh : E      = Tanh.make( ex )
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

      def min( b: E ) : E        = Min.make( ex, b )
      def max( b: E ) : E        = Max.make( ex, b )
      def round( b: E ) : E      = Round.make( ex, b )
      def roundup( b: E ) : E    = Roundup.make( ex, b )
      def trunc( b: E ) : E      = Trunc.make( ex, b )
      def atan2( b: E ) : E      = Atan2.make( ex, b )
      def hypot( b: E ) : E      = Hypot.make( ex, b )
      def hypotx( b: E ) : E     = Hypotx.make( ex, b )
      def pow( b: E ) : E        = Pow.make( ex, b )
//      def ring1( b: E ) : E     = Ring1.make( ex, b )
//      def ring2( b: E ) : E     = Ring2.make( ex, b )
//      def ring3( b: E ) : E     = Ring3.make( ex, b )
//      def ring4( b: E ) : E     = Ring4.make( ex, b )
      def difsqr( b: E ) : E    = Difsqr.make( ex, b )
      def sumsqr( b: E ) : E    = Sumsqr.make( ex, b )
      def sqrsum( b: E ) : E    = Sqrsum.make( ex, b )
      def sqrdif( b: E ) : E    = Sqrdif.make( ex, b )
      def absdif( b: E ) : E    = Absdif.make( ex, b )
      def thresh( b: E ) : E    = Thresh.make( ex, b )
//      def amclip( b: E ) : E    = Amclip.make( ex, b )
//      def scaleneg( b: E ) : E  = Scaleneg.make( ex, b )
      def clip2( b: E ) : E     = Clip2.make( ex, b )
//      def excess( b: E ) : E    = Excess.make( ex, b )
      def fold2( b: E ) : E     = Fold2.make( ex, b )
      def wrap2( b: E ) : E     = Wrap2.make( ex, b )
   // def firstarg( b: Double ) : Double  = d

//      def linlin( srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double ) : Double =
//         rd_linlin( d, srcLo, srcHi, dstLo, dstHi )
//
//      def linexp( srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double ) : Double =
//         rd_linexp( d, srcLo, srcHi, dstLo, dstHi )
   }
}