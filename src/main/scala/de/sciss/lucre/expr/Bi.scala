/*
 *  Bi.scala
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

import de.sciss.lucre.event.Event
import de.sciss.lucre.stm.{Writer, Sys}
import de.sciss.lucre.{DataInput, DataOutput}
import de.sciss.collection.txn.{SkipList, Ordered, HASkipList}

object Bi {
   def newVar[ S <: Sys[ S ], A ]( init: Expr[ S, A ])( implicit tx: S#Tx,
                                                        peerType: BiType[ A ]) : Var[ S, A ] = {
      val ordered = {
         implicit val _peerSer   = peerType.serializer[ S ]
         implicit val ord        = Ordering.by[ (Long, Expr[ S, A ]), Long ]( _._1 )
         HASkipList.empty[ S, (Long, Expr[ S, A ])]
      }
      new Impl[ S, A ]( ordered, peerType )
   }

   def readVar[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc )
                                  ( implicit tx: S#Tx, peerType: BiType[ A ]) : Var[ S, A ] = {
      sys.error( "TODO" )
//      new Impl[ S, A ]( ordered, peerType )
   }

   trait Var[ S <: Sys[ S ], A ] extends Bi[ S, A ] {
      def set( time: Expr[ S, Long ], value: Expr[ S, A ]) : Unit
   }

   private final class Impl[ S <: Sys[ S ], A ]( ordered: SkipList[ S, (Long, Expr[ S, A ])], peerType: BiType[ A ])
   extends Var[ S, A ] {
//      private val ordered = {
//         implicit val tx         = tx0
//         implicit val _peerSer   = peerType.serializer[ S ]
//         implicit val ord        = Ordering.by[ (Long, Expr[ S, A ]), Long ]( _._1 )
//         HASkipList.empty[ S, (Long, Expr[ S, A ])]
//      }

      def get( time: Long )( implicit tx: S#Tx ) : Expr[ S, A ] = {
         // XXX TODO should be an efficient method in skiplist itself
         ordered.isomorphicQuery( new Ordered[ S#Tx, (Long, Expr[ S, A ])] {
            def compare( that: (Long, Expr[ S, A ]))( implicit tx: S#Tx ) = {
               val t = that._1
               if( t < time ) -1 else if( t > time ) 1 else 0
            }
         })._1._2
      }

      def value( time: Long )( implicit tx: S#Tx ) : A = get( time ).value

      def write( out: DataOutput ) {
         ordered.write( out )
      }

      def at( time: Expr[ S, Long ])( implicit tx: S#Tx ) : Expr[ S, A ] = peerType.newCursor[ S ]( this, time )

      def changed : Event[ S, (Span, A), Bi[ S, A ]] = sys.error( "TODO" )

      def set( time: Expr[ S, Long ], value: Expr[ S, A ]) {
         sys.error( "TODO" )
      }
   }
}
trait Bi[ S <: Sys[ S ], A ] extends Writer {
   def get( time: Long )( implicit tx: S#Tx ) : Expr[ S, A ]
   def value( time: Long )( implicit tx: S#Tx ) : A
   def at( time: Expr[ S, Long ])( implicit tx: S#Tx ) : Expr[ S, A ]
   def changed : Event[ S, (Span, A), Bi[ S, A ]]
}
