/*
 *  BiType.scala
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

package de.sciss.lucre
package bitemp

import stm.{ImmutableSerializer, Sys}
import de.sciss.lucre.{event => evt}
import evt.{Pull, Targets}
import expr.{Expr, Type}

/**
 * Extends `Type` with a an expression form which acts as a cursor on a bi-temporal object.
 */
trait BiType[ A ] extends Type[ A ] {
   implicit object ValueSer extends ImmutableSerializer[ A ] {
      def write( v: A, out: DataOutput ) { writeValue( v, out )}
      def read( in: DataInput ) : A = readValue( in )
   }

//   def newProjection[ S <: Sys[ S ]]( bi: BiPin[ S, A ])( implicit tx: S#Tx, time: Chronos[ S ]): Ex[ S ] = {
//      val targets = Targets.partial[ S ]
//      val init    = bi.value
//      val cache   = tx.newPartialVar[ A ]( targets.id, init )
//      new Projection[ S ]( targets, cache, bi, time )
//   }

   def longType : BiType[ Long ]
   def spanLikeType : BiType[ SpanLike ]

//   def readLongExpr[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Expr[ S, Long ]

   // assumes it was identified (cookie 3 read)
   protected def readProjection[ S <: Sys[ S ]]( in: DataInput, access: S#Acc, targets: Targets[ S ])
                                               ( implicit tx: S#Tx ) : Ex[ S ] = {
//      val bi   =
      val cache   = tx.readPartialVar[ A ]( targets.id, in )
      val bi      = BiPin.Expr.Modifiable.read[ S, A ]( in, access )( tx, this )
      val time    = longType.readExpr( in, access )
      new Projection[ S ]( targets, cache, bi, time )
   }

   private final class Projection[ S <: Sys[ S ]]( protected val targets: Targets[ S ], cache: S#Var[ A ],
                                                   bi: BiPin[ S, Expr[ S, A ], event.Change[ A ]], ts: Expr[ S, Long ])
      extends Expr.Node[ S, A ] {
      def reader: event.Reader[ S, Ex[ S ]] = serializer[ S ]

      protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( 3 )
         cache.write( out )
         bi.write( out )
//         ts.time.write( out )
         ts.write( out )
      }

//      def value( implicit tx: S#Tx ): A = bi.value( tx, ts )
      def value( implicit tx: S#Tx ): A = bi.at( ts.value ).value

      private[lucre] def connect()( implicit tx: S#Tx ) {
//println( "CONNECT CURSOR" )
         bi.changed   ---> this
//         ts.time.changed ---> this
         ts.changed ---> this
      }

      private[lucre] def disconnect()( implicit tx: S#Tx ) {
//println( "DISCONNECT CURSOR" )
         bi.changed   -/-> this
//         ts.time.changed -/-> this
         ts.changed -/-> this
      }

      private[lucre] def pullUpdate( pull: Pull[ S ])( implicit tx: S#Tx ): Option[ Change[ S ]] = {
sys.error( "TODO" )
//         val biChanged     = bi.changed
////         val timeChanged   = ts.time.changed
//         val timeChanged   = ts.changed
//
//         val biChange = if( biChanged.isSource( pull )) {
//            biChanged.pullUpdate( pull )
//         } else {
//            None
//         }
//         val timeChange = if( timeChanged.isSource( pull )) {
//            timeChanged.pullUpdate( pull )
//         } else {
//            None
//         }
//
//         // there are three cases
//         // - if the time value changes, we need to read the bi at the new
//         //   value (independent of biChanged)
//         // - if the time value didn't change, we see if biChange affects the
//         //   current time position
//         // - all other cases are dropped
//         val res = (biChange, timeChange) match {
//            case (Some( bch ), None) =>
////               val timeVal = ts.time.value
//               val timeVal = ts.value
//               bch.find( _.span.contains( timeVal )).flatMap { region =>
//                  val before  = cache.get
//                  val now     = region.value
//                  cache.set( now )
//                  change( before, now )
//               }
//            case (_, Some( tch )) =>
//               val before  = cache.get
////               val before  = bi.value( tch.before )
//               val now     = bi.at( tch.now )
////println( "CACHE WAS " + before + " NOW (AT " + tch.now + ") IS " + now )
//               cache.set( now )
//               change( before, now )
////            case (Some( bch ), Some( tch )) /* if bch._1.contains( tch.now ) */ =>
////               val before  = cache.get
////               val now     = bi.value( tch.now )
////               cache.set( now )
////               change( before, now )
//            case _ => None
//         }
////println( "CURSOR UPDATE. TIME = " + timeChange + ", BI = " + biChange + ", RES = " + res )
//         res
      }
   }
}
