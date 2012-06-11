/*
 *  Booleans.scala
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
import de.sciss.lucre.event.Targets
import de.sciss.lucre.expr.{Expr, Span}

// typeIDs : 0 = byte, 1 = short, 2 = int, 3 = long, 4 = float, 5 = double, 6 = boolean, 7 = char,
//           8 = string, 9 = spanlike, 10 = span
object Spans extends BiTypeImpl[ Span ] {
   private val typeID = 10

   /* protected */ def readValue( in: DataInput ) : Span = Span.read( in )
   /* protected */ def writeValue( value: Span, out: DataOutput ) { value.write( out )}

   final class Ops[ S <: Sys[ S ]]( ex: Ex[ S ])( implicit tx: S#Tx ) {
      // ---- unary ----
      def start  : Expr[ S, Long ] = UnaryOp.Start.make(  ex )
      def stop   : Expr[ S, Long ] = UnaryOp.Stop.make(   ex )
      def length : Expr[ S, Long ] = UnaryOp.Length.make( ex )
      // ---- binary ----
      def shift( delta: Expr[ S, Long ]) : Ex[ S ] = BinaryOp.Shift.make( ex, delta )
   }

   // ---- protected ----

   def readTuple[ S <: Sys[ S ]]( cookie: Int, in: DataInput, access: S#Acc, targets: Targets[ S ])( implicit tx: S#Tx ) : Ex[ S ] =
//   case 3 =>
//      readCursor[ S ]( in, access, targets )
      sys.error( "Invalid cookie " + cookie )

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

      sealed abstract class LongOp( val id: Int ) extends Longs.UnaryOp.Op[ Span ] {
         final def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc, targets: Targets[ S ])
                                       ( implicit tx: S#Tx ) : Longs.Tuple1[ S, Span ] = {
            val _1 = readExpr( in, access )
            new Longs.Tuple1( typeID, this, targets, _1 )
         }

//         final def make[ S <: Sys[ S ]]( a: Ex[ S ])( implicit tx: S#Tx ) : Expr[ S, Long ] = {
//            new Longs.Tuple1( typeID, this, Targets.partial[ S ], a )
//         }
      }

      case object Start extends LongOp( 0x1000 ) {
         def value( a: Span ) : Long = a.start
      }

      case object Stop extends LongOp( 0x1001 ) {
         def value( a: Span ) : Long = a.stop
      }

      case object Length extends LongOp( 0x1002 ) {
         def value( a: Span ) : Long = a.length
      }
   }

   private object BinaryOp {
      sealed trait OpLike[ T1, T2 ] {
         def toString[ S <: Sys[ S ]]( _1: Expr[ S, T1 ], _2: Expr[ S, T2 ]) : String = _1.toString + "." + name + "(" + _2 + ")"

         def name: String = { val cn = getClass.getName
            val sz   = cn.length
            val i    = cn.indexOf( '$' ) + 1
            "" + cn.charAt( i ).toLower + cn.substring( i + 1, if( cn.charAt( sz - 1 ) == '$' ) sz - 1 else sz )
         }
      }

      sealed abstract class LongSpanOp( val id: Int ) extends Tuple2Op[ Span, Long ] with OpLike[ Span, Long ] {
         final def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc, targets: Targets[ S ])
                                       ( implicit tx: S#Tx ) : Tuple2[ S, Span, Long ] = {
            val _1 = readExpr( in, access )
            val _2 = Longs.readExpr( in, access )
            new Tuple2( typeID, this, targets, _1, _2 )
         }

         final def make[ S <: Sys[ S ]]( a: Ex[ S ], b: Expr[ S, Long ])( implicit tx: S#Tx ) : Ex[ S ] = {
            new Tuple2( typeID, this, Targets.partial[ S ], a, b )
         }
      }

      case object Shift extends LongSpanOp( 0x2000 ) {
         def value( a: Span, b: Long ) : Span = a.shift( b )
      }
   }
}