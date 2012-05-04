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

package de.sciss.lucre.expr

import de.sciss.lucre.stm.{Serializer, Sys}
import de.sciss.lucre.{DataInput, event, DataOutput}
import event.{Pull, Targets}

/**
 * Extends `Type` with a an expression form which acts as a cursor on a bi-temporal object.
 */
trait BiType[ A ] extends Type[ A ] {
   private implicit object ValueSer extends Serializer[ A ] {
      def write( v: A, out: DataOutput ) { writeValue( v, out )}
      def read( in: DataInput ) : A = readValue( in )
   }

   def newCursor[ S <: Sys[ S ]]( bi: Bi[ S, A ], time: Expr[ S, Long ])( implicit tx: S#Tx ): Ex[ S ] = {
      val targets = Targets.partial[ S ]
      val init    = bi.value( time.value )
      val cache   = tx.newPartialVar[ A ]( targets.id, init )
      new Cursor[ S ]( targets, cache, bi, time )
   }

   protected def longType : BiType[ Long ]

//   def readLongExpr[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Expr[ S, Long ]

   // assumes it was identified (cookie 3 read)
   protected def readCursor[ S <: Sys[ S ]]( in: DataInput, access: S#Acc, targets: Targets[ S ])
                                           ( implicit tx: S#Tx ) : Ex[ S ] = {
//      val bi   =
      val cache   = tx.readPartialVar[ A ]( targets.id, in )
      val bi      = Bi.readVar[ S, A ]( in, access )( tx, this )
      val time    = longType.readExpr( in, access )
      new Cursor[ S ]( targets, cache, bi, time )
   }

   private final class Cursor[ S <: Sys[ S ]]( protected val targets: Targets[ S ], cache: S#Var[ A ],
                                               bi: Bi[ S, A ], time: Expr[ S, Long ])
      extends Expr.Node[ S, A ] {
      def reader: event.Reader[ S, Ex[ S ]] = serializer[ S ]

      protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( 3 )
         cache.write( out )
         bi.write( out )
         time.write( out )
      }

      def value( implicit tx: S#Tx ): A = bi.get( time.value ).value

      private[lucre] def connect()( implicit tx: S#Tx ) {
         bi.changed   ---> this
         time.changed ---> this
      }

      private[lucre] def disconnect()( implicit tx: S#Tx ) {
         bi.changed   -/-> this
         time.changed -/-> this
      }

      private[lucre] def pullUpdate( pull: Pull[ S ])( implicit tx: S#Tx ): Option[ Change[ S ]] = {
         val biChanged     = bi.changed
         val timeChanged   = time.changed

         val biChange = if( biChanged.isSource( pull )) {
            biChanged.pullUpdate( pull )
         } else {
            None
         }
         val timeChange = if( timeChanged.isSource( pull )) {
            timeChanged.pullUpdate( pull )
         } else {
            None
         }

         (biChange, timeChange) match {
            case (Some( bch ), None) if bch._1.contains( time.value ) =>
               val before  = cache.get
               val now     = bch._2
               cache.set( now )
               change( before, now )
            case (_, Some( tch )) =>
               val before  = cache.get
//               val before  = bi.value( tch.before )
               val now     = bi.value( tch.now )
               cache.set( now )
               change( before, now )
//            case (Some( bch ), Some( tch )) /* if bch._1.contains( tch.now ) */ =>
//               val before  = cache.get
//               val now     = bi.value( tch.now )
//               cache.set( now )
//               change( before, now )
            case _ => None
         }
      }
   }
}
