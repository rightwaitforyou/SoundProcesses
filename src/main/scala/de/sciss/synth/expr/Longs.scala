/*
 *  Longs.scala
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
import de.sciss.lucre.expr.Type
import de.sciss.lucre.event.Targets
import annotation.switch

// typeIDs : 0 = byte, 1 = short, 2 = int, 3 = long, 4 = float, 5 = double, 6 = boolean, 7 = char,
//           8 = string
object Longs extends BiTypeImpl[ Long ] {
   private val typeID = 3

   /* protected */ def readValue( in: DataInput ) : Long = in.readLong()
   /* protected */ def writeValue( value: Long, out: DataOutput ) { out.writeLong( value )}

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
               case 4  => BitNot
               case 5  => Abs
               case 11 => Signum
               case 12 => Squared
               case 13 => Cubed
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
               case 3 => IDiv
//               case 4 => Div
//               case 5 => Mod
         //      case 6 => Eq
         //      case 7 => Neq
         //      case 8 => Lt
         //      case 9 => Gt
         //      case 10 => Leq
         //      case 11 => Geq
               case 12 => Min
               case 13 => Max
               case 14 => BitAnd
               case 15 => BitOr
               case 16 => BitXor
            // case 17 => Lcm
            // case 18 => Gcd
//               case 19 => Round
//               case 20 => Roundup
//               case 26 => <<
//               case 27 => >>
//               case 28 => >>>
               case 38 => Absdif
//               case 42 => Clip2
//               case 44 => Fold2
//               case 45 => Wrap2
            }
            val _1 = readExpr( in, access )
            val _2 = readExpr( in, access )
            new Tuple2( typeID, op, targets, _1, _2 )

         case 3 =>
            readProjection[ S ]( in, access, targets )

         case _ => sys.error( "Invalid cookie " + cookie )
      }
   }

   private object UnaryOp {
      sealed abstract class Op( val id: Int ) extends Tuple1Op[ Long ] {
         final def make[ S <: Sys[ S ]]( a: Ex[ S ])( implicit tx: S#Tx ) : Ex[ S ] = {
            new Tuple1( typeID, this, Targets.partial[ S ], a )
         }

         def value( a: Long ) : Long

         def toString[ S <: Sys[ S ]]( _1: Ex[ S ]) : String = _1.toString + "." + name

         def name: String = { val cn = getClass.getName
            val sz   = cn.length
            val i    = cn.indexOf( '$' ) + 1
            "" + cn.charAt( i ).toLower + cn.substring( i + 1, if( cn.charAt( sz - 1 ) == '$' ) sz - 1 else sz )
         }
      }
      
      case object Neg extends Op( 0 ) {
         def value( a: Long ) : Long = -a
         override def toString[ S <: Sys[ S ]]( _1: Ex[ S ]) : String = "-" + _1
      }
      case object Abs         extends Op(  5 ) {
         def value( a: Long ) : Long = math.abs( a )
      }
      case object BitNot extends Op( 4 ) {
         def value( a: Long ) : Long = ~a
         override def toString[ S <: Sys[ S ]]( _1: Ex[ S ]) : String = "~" + _1
      }
   // case object ToLong     extends Op(  6 )
   // case object ToInt       extends Op(  7 )
      case object Signum      extends Op( 11 ) {
         def value( a: Long ) : Long = math.signum( a )
      }
      case object Squared     extends Op( 12 ) {
         def value( a: Long ) : Long = a * a
      }
      case object Cubed       extends Op( 13 ) {
         def value( a: Long ) : Long = a * a * a
      }
   }

   private object BinaryOp {
      sealed abstract class Op( val id: Int ) extends Tuple2Op[ Long, Long ] {
         final def make[ S <: Sys[ S ]]( a: Ex[ S ], b: Ex[ S ])( implicit tx: S#Tx ) : Ex[ S ] = {
            new Tuple2( typeID, this, Targets.partial[ S ], a, b )
         }
         def value( a: Long, b: Long ) : Long

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
         def value( a: Long, b: Long ) : Long = a + b
      }
      case object Minus          extends Op(  1 ) with Infix {
         override val name = "-"
         def value( a: Long, b: Long ) : Long = a - b
      }
      case object Times          extends Op(  2 ) with Infix {
         override val name = "*"
         def value( a: Long, b: Long ) : Long = a * b
      }
      case object IDiv           extends Op(  3 ) {
         override val name = "div"
         def value( a: Long, b: Long ) : Long = a / b
      }
      case object Min            extends Op( 12 ) {
         def value( a: Long, b: Long ) : Long = math.min( a, b )
      }
      case object Max            extends Op( 13 ) {
         def value( a: Long, b: Long ) : Long = math.max( a, b )
      }
      case object BitAnd         extends Op( 14 ) {
         def value( a: Long, b: Long ) : Long = a & b
      }
      case object BitOr          extends Op( 15 ) {
         def value( a: Long, b: Long ) : Long = a | b
      }
      case object BitXor         extends Op( 16 ) {
         def value( a: Long, b: Long ) : Long = a ^ b
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
      case object Absdif         extends Op( 38 ) {
         def value( a: Long, b: Long ) : Long = math.abs( a - b )
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

   final class Ops[ S <: Sys[ S ]]( ex: Ex[ S ])( implicit tx: S#Tx ) {
      private type E = Ex[ S ]

      import UnaryOp._
      def unary_- : E            = Neg.make( ex )
      def unary_~ : E	         = BitNot.make( ex )

      import BinaryOp._
      def +( b: E ) : E          = Plus.make( ex, b )
      def -( b: E ) : E          = Minus.make( ex, b )
      def *( b: E ) : E          = Times.make( ex, b )
      def /( b: E ) : E          = IDiv.make( ex, b )
      def &( b: E ) : E          = BitAnd.make( ex, b )
      def |( b: E ) : E          = BitOr.make( ex, b )
      def ^( b: E ) : E          = BitXor.make( ex, b )
   }

   final class RichOps[ S <: Sys[ S ]]( ex: Ex[ S ])( implicit tx: S#Tx ) {
      private type E = Ex[ S ]
      
      import UnaryOp._
      def abs : E                = Abs.make( ex )
   // def toLong : E	         = UnOp.make( 'asLong, ex )
   // def toInteger : E	      = UnOp.make( 'asInteger, ex )
      def signum : E             = Signum.make( ex )
      def squared : E            = Squared.make( ex )
      def cubed : E              = Cubed.make( ex )

      import BinaryOp._
      def min( b: E ) : E        = Min.make( ex, b )
      def max( b: E ) : E        = Max.make( ex, b )
      def absdif( b: E ) : E     = Absdif.make( ex, b )
//      def clip2( b: E ) : E      = Clip2.make( ex, b )
//      def fold2( b: E ) : E      = Fold2.make( ex, b )
//      def wrap2( b: E ) : E      = Wrap2.make( ex, b )
   }
}