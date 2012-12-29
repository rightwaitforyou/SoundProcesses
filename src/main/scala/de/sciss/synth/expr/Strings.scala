/*
 *  Strings.scala
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

import de.sciss.lucre.{stm, DataInput, DataOutput, event => evt}
import evt.{Targets, Sys}

object Strings extends BiTypeImpl[ String ] {
   private final val typeID = 8

   /* protected */ def readValue( in: DataInput ) : String = in.readString()
   /* protected */ def writeValue( value: String, out: DataOutput ) { out.writeString( value )}

   final class Ops[ S <: Sys[ S ]]( ex: Ex[ S ])( implicit tx: S#Tx ) {
      private type E = Ex[ S ]
      import BinaryOp._

      def ++( b: E ) : E = Append.make( ex, b )
   }

   private object BinaryOp {
      sealed abstract class Op( val id: Int ) extends Tuple2Op[ String, String ] {
         final def make[ S <: Sys[ S ]]( a: Ex[ S ], b: Ex[ S ])( implicit tx: S#Tx ) : Ex[ S ] = {
            new Tuple2( typeID, this, Targets.partial[ S ], a, b )
         }
         def value( a: String, b: String ) : String

         def toString[ S <: stm.Sys[ S ]]( _1: Ex[ S ], _2: Ex[ S ]) : String = _1.toString + "." + name + "(" + _2 + ")"

         def name: String = { val cn = getClass.getName
            val sz   = cn.length
            val i    = cn.indexOf( '$' ) + 1
            "" + cn.charAt( i ).toLower + cn.substring( i + 1, if( cn.charAt( sz - 1 ) == '$' ) sz - 1 else sz )
         }
      }

      case object Append extends Op( 0 ) {
         def value( a: String, b: String ) : String = a + b
      }
   }

   // ---- protected ----

   def readTuple[ S <: Sys[ S ]]( cookie: Int, in: DataInput, access: S#Acc, targets: Targets[ S ])
                                ( implicit tx: S#Tx ) : ExN[ S ] = {
      (cookie /* : @switch */) match {
//         case 1 =>
//            val tpe  = in.readInt()
//            require( tpe == typeID, "Invalid type id (found " + tpe + ", required " + typeID + ")" )
//            val opID = in.readInt()
//            import UnaryOp._
//            val op: Op = (opID: @switch) match {
//               case _  => sys.error( "Invalid operation id " + opID )
//            }
//            val _1 = readExpr( in, access )
//            new Tuple1( typeID, op, targets, _1 )

         case 2 =>
            val tpe = in.readInt()
            require( tpe == typeID, "Invalid type id (found " + tpe + ", required " + typeID + ")" )
            val opID = in.readInt()
            import BinaryOp._
            val op: Op = (opID /*: @switch */) match {
               case 0 => Append
            }
            val _1 = readExpr( in, access )
            val _2 = readExpr( in, access )
            new Tuple2( typeID, op, targets, _1, _2 )

//         case 3 =>
//            readProjection[ S ]( in, access, targets )

         case _ => sys.error( "Invalid cookie " + cookie )
      }
   }
}
