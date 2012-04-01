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

object Doubles extends Type[ Double ] {
   protected def readValue( in: DataInput ) : Double = in.readDouble()
   protected def writeValue( value: Double, out: DataOutput ) { out.writeDouble( value )}

   private object UnaryOp {
      import RichDouble._

      sealed abstract class Op( private[expr] val id: Int ) {
         def make[ S <: Sys[ S ]]( a: Ex[ S ]) : Ex[ S ] = sys.error( "TODO" )
         def value( a: Double ) : Double
      }
      
      case object Neg extends Op( 0 ) {
         def value( a: Double ) : Double = rd_neg( a )
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

   final class Ops[ S <: Sys[ S ]]( ex: Ex[ S ]) {
      private type E = Ex[ S ]
      
      import UnaryOp._
      
      def unary_- : E   = Neg.make( ex )
   // def bitNot : E	         = BitNot.make( ex )
      def abs : E       = Abs.make( ex )
   // def toDouble : E	         = UnOp.make( 'asDouble, ex )
   // def toInteger : E	      = UnOp.make( 'asInteger, ex )
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
   }
}