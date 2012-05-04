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

import de.sciss.collection.txn.{SkipList, Ordered, HASkipList}
import de.sciss.lucre.event.{Targets, Root, Trigger, StandaloneLike, Event}
import de.sciss.lucre.stm.{TxnSerializer, Writer, Sys}
import de.sciss.lucre.{event, DataInput, DataOutput}

object Bi {
   type Change[ A ] = (Span, A)

   def newVar[ S <: Sys[ S ], A ]( init: Expr[ S, A ])( implicit tx: S#Tx,
                                                        peerType: BiType[ A ]) : Var[ S, A ] = {
      val targets = Targets.partial[ S ]
      val ordered = {
         implicit val _peerSer   = peerType.serializer[ S ]
         implicit val ord        = Ordering.by[ (Long, Expr[ S, A ]), Long ]( _._1 )
         HASkipList.empty[ S, (Long, Expr[ S, A ])]
      }
      ordered.add( (0L, init) )
      new Impl[ S, A ]( targets, ordered )
   }

   def readVar[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc )
                                  ( implicit tx: S#Tx, peerType: BiType[ A ]) : Var[ S, A ] = {
      val targets = Targets.read[ S ]( in, access )
      val ordered = {
         implicit val _peerSer   = peerType.serializer[ S ]
         implicit val ord        = Ordering.by[ (Long, Expr[ S, A ]), Long ]( _._1 )
         HASkipList.read[ S, (Long, Expr[ S, A ])]( in, access )
      }
      new Impl[ S, A ]( targets, ordered )
   }

   implicit def serializer[ S <: Sys[ S ], A ]( implicit peerType: BiType[ A ]) :
      event.Reader[ S, Bi[ S, A ]] with TxnSerializer[ S#Tx, S#Acc, Bi[ S, A ]] = new Ser[ S, A ]

   implicit def varSerializer[ S <: Sys[ S ], A ]( implicit peerType: BiType[ A ]) :
      event.Reader[ S, Var[ S, A ]] with TxnSerializer[ S#Tx, S#Acc, Var[ S, A ]] = new VarSer[ S, A ]

   private final class Ser[ S <: Sys[ S ], A ]( implicit peerType: BiType[ A ])
   extends event.Reader[ S, Bi[ S, A ]] with TxnSerializer[ S#Tx, S#Acc, Bi[ S, A ]] {
      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Bi[ S, A ] = {
         read( in, access, Targets.read[ S ]( in, access ))
      }
      def read( in: DataInput, access: S#Acc, targets: Targets[ S ])( implicit tx: S#Tx ) : Bi[ S, A ] = {
         val ordered = {
            implicit val _peerSer   = peerType.serializer[ S ]
            implicit val ord        = Ordering.by[ (Long, Expr[ S, A ]), Long ]( _._1 )
            HASkipList.read[ S, (Long, Expr[ S, A ])]( in, access )
         }
         new Impl[ S, A ]( targets, ordered )
      }
      def write( v: Bi[ S, A ], out: DataOutput ) { v.write( out )}
   }

   private final class VarSer[ S <: Sys[ S ], A ]( implicit peerType: BiType[ A ])
   extends event.Reader[ S, Var[ S, A ]] with TxnSerializer[ S#Tx, S#Acc, Var[ S, A ]] {
      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Var[ S, A ] = {
         read( in, access, Targets.read[ S ]( in, access ))
      }
      def read( in: DataInput, access: S#Acc, targets: Targets[ S ])( implicit tx: S#Tx ) : Var[ S, A ] = {
         val ordered = {
            implicit val _peerSer   = peerType.serializer[ S ]
            implicit val ord        = Ordering.by[ (Long, Expr[ S, A ]), Long ]( _._1 )
            HASkipList.read[ S, (Long, Expr[ S, A ])]( in, access )
         }
         new Impl[ S, A ]( targets, ordered )
      }
      def write( v: Var[ S, A ], out: DataOutput ) { v.write( out )}
   }

   trait Var[ S <: Sys[ S ], A ] extends Bi[ S, A ] {
      def set( time: Expr[ S, Long ], value: Expr[ S, A ])( implicit tx: S#Tx ) : Unit
   }

   private final class Impl[ S <: Sys[ S ], A ]( protected val targets: Targets[ S ],
                                                 ordered: SkipList[ S, (Long, Expr[ S, A ])])
                                               ( implicit peerType: BiType[ A ])
   extends Var[ S, A ]
   with Trigger.Impl[ S, Change[ A ], Change[ A ], Bi[ S, A ]]
   with StandaloneLike[ S, Change[ A ], Bi[ S, A ]]
   with Root[ S, Change[ A ]] {
      protected def reader  = serializer[ S, A ]

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

      protected def writeData( out: DataOutput ) {
         ordered.write( out )
      }

      protected def disposeData()( implicit tx: S#Tx ) {
         ordered.dispose()
      }

      def at( time: Expr[ S, Long ])( implicit tx: S#Tx ) : Expr[ S, A ] = peerType.newCursor[ S ]( this, time )

      def changed : Event[ S, Change[ A ], Bi[ S, A ]] = this

      def set( time: Expr[ S, Long ], value: Expr[ S, A ])( implicit tx: S#Tx ) {
         val tv   = time.value
         ordered.add( (tv, value) )
         val span = Span( tv, tv )  // XXX TODO determine stop time
         fire( span -> value.value )
      }
   }
}
trait Bi[ S <: Sys[ S ], A ] extends Writer {
   def get( time: Long )( implicit tx: S#Tx ) : Expr[ S, A ]
   def value( time: Long )( implicit tx: S#Tx ) : A
   def at( time: Expr[ S, Long ])( implicit tx: S#Tx ) : Expr[ S, A ]
   def changed : Event[ S, Bi.Change[ A ], Bi[ S, A ]]
}
