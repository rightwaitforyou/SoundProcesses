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

import de.sciss.collection.txn
import txn.{SkipList, Ordered, HASkipList}
import de.sciss.lucre.{event, DataInput, DataOutput}
import event.{Pull, Targets, Trigger, StandaloneLike, Event}
import de.sciss.lucre.stm.{InMemory, TxnSerializer, Writer, Sys}

object Bi {
   type Change[ A ] = (Span, A)

   def newVar[ S <: Sys[ S ], A ]( init: Expr[ S, A ])( implicit tx: S#Tx,
                                                        peerType: BiType[ A ]) : Var[ S, A ] = {
      val targets = Targets.partial[ S ]
      val ordered = {
         implicit val ser     = Entry.serializer[ S, A ] // peerType.serializer[ S ]
         implicit val ord     = Ordering.by[ (Long, Expr[ S, A ]), Long ]( _._1 )
         HASkipList.empty[ S, Entry[ S, A ]]
      }
      ordered.add( Entry( 0L, peerType.longType.newConst( 0L ), init ))
      new Impl[ S, A ]( targets, ordered )
   }

   def readVar[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc )
                                  ( implicit tx: S#Tx, peerType: BiType[ A ]) : Var[ S, A ] = {
      val targets = Targets.read[ S ]( in, access )
      val ordered = {
         implicit val ser     = Entry.serializer[ S, A ] // peerType.serializer[ S ]
         implicit val ord     = Ordering.by[ (Long, Expr[ S, A ]), Long ]( _._1 )
         HASkipList.read[ S, Entry[ S, A ]]( in, access )
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
            implicit val ser     = Entry.serializer[ S, A ] // peerType.serializer[ S ]
            implicit val ord     = Ordering.by[ (Long, Expr[ S, A ]), Long ]( _._1 )
            HASkipList.read[ S, Entry[ S, A ]]( in, access )
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
            implicit val ser        = Entry.serializer[ S, A ] // peerType.serializer[ S ]
            implicit val ord        = Ordering.by[ (Long, Expr[ S, A ]), Long ]( _._1 )
            HASkipList.read[ S, Entry[ S, A ]]( in, access )
         }
         new Impl[ S, A ]( targets, ordered )
      }
      def write( v: Var[ S, A ], out: DataOutput ) { v.write( out )}
   }

   trait Var[ S <: Sys[ S ], A ] extends Bi[ S, A ] {
      def set( time: Expr[ S, Long ], value: Expr[ S, A ])( implicit tx: S#Tx ) : Unit
   }

   private object Entry {
      def serializer[ S <: Sys[ S ], A ]( implicit peer: BiType[ A ]) : TxnSerializer[ S#Tx, S#Acc, Entry[ S, A ]] =
         new Ser[ S, A ]

      private final class Ser[ S <: Sys[ S ], A ]( implicit peer: BiType[ A ])
      extends TxnSerializer[ S#Tx, S#Acc, Entry[ S, A ]] {
         def write( e: Entry[ S, A ], out: DataOutput ) {
            out.writeLong( e.timeVal )
            e.time.write( out )
            e.value.write( out )
         }

         def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Entry[ S, A ] = {
            val timeVal = in.readLong()
            val time    = peer.longType.readExpr[ S ]( in, access )
            val value   = peer.readExpr[ S ]( in, access )
            Entry( timeVal, time, value )
         }
      }

      implicit def ordering[ S <: Sys[ S ], A ] : txn.Ordering[ S#Tx, Entry[ S, A ]] =
         Ord.asInstanceOf[ txn.Ordering[ Any, Entry[ S, A ]]]

      private object Ord extends txn.Ordering[ Any, Entry[ InMemory, _ ]] {
         def compare( a: Entry[ InMemory, _ ], b: Entry[ InMemory, _ ])( implicit tx: Any ) : Int = {
            if( a.timeVal < b.timeVal ) -1 else if( a.timeVal > b.timeVal ) 1 else 0
         }
      }
   }
   private final case class Entry[ S <: Sys[ S ], A ]( timeVal: Long, time: Expr[ S, Long ], value: Expr[ S, A ])

   private final class Impl[ S <: Sys[ S ], A ]( protected val targets: Targets[ S ],
                                                 ordered: SkipList[ S, Entry[ S, A ]])
                                               ( implicit peerType: BiType[ A ])
   extends Var[ S, A ]
   with Trigger.Impl[ S, Change[ A ], Change[ A ], Bi[ S, A ]]
   with StandaloneLike[ S, Change[ A ], Bi[ S, A ]]
   /* with Root[ S, Change[ A ]] */ {
      protected def reader = serializer[ S, A ]

      private[lucre] def connect()( implicit tx: S#Tx ) {
//         getEntry(0)._1._2.changed.--->(this)
      }
      private[lucre] def disconnect()( implicit tx: S#Tx ) {

      }

      private[lucre] def pullUpdate( pull: Pull[ S ])( implicit tx: S#Tx ) : Option[ Change[ A ]] = {
         pull.resolve[ Change[ A ]]
      }

      private def getEntry( time: Long )( implicit tx: S#Tx ) : (Entry[ S, A ], Int) = {
         // XXX TODO should be an efficient method in skiplist itself
         ordered.isomorphicQuery( new Ordered[ S#Tx, Entry[ S, A ]] {
            def compare( that: Entry[ S, A ])( implicit tx: S#Tx ) = {
               val t = that.timeVal
               if( time < t ) -1 else if( time > t ) 1 else 0
            }
         })
      }

      def debugList()( implicit tx: S#Tx ) : List[ (Long, A)] = ordered.toList.map( e => (e.timeVal, e.value.value) )

      def get( time: Long )( implicit tx: S#Tx ) : Expr[ S, A ] = {
//         val ((succ, _), cmp) = getEntry( time )._1._2
//         if( cmp > 0 ) ???
         ordered.toList.takeWhile( _.timeVal <= time ).last.value // XXX TODO ouch... we do need a pred method for skiplist
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
         val start                        = time.value
         val (Entry( stop0, _, _), cmp)   = getEntry( start + 1 )
//println( "set " + tv + " -> succ = " + succ + ", cmp = " + cmp )
         ordered.add( Entry( start, time, value ))
         val stop                = if( cmp <= 0 ) stop0 else 0x4000000000000000L  // XXX TODO should have special version of Span
         val span = Span( start, stop )
         fire( span -> value.value )
      }
   }
}
sealed trait Bi[ S <: Sys[ S ], A ] extends Writer {
   def get( time: Long )( implicit tx: S#Tx ) : Expr[ S, A ]
   def value( time: Long )( implicit tx: S#Tx ) : A
   def at( time: Expr[ S, Long ])( implicit tx: S#Tx ) : Expr[ S, A ]
   def changed : Event[ S, Bi.Change[ A ], Bi[ S, A ]]

   def debugList()( implicit tx: S#Tx ) : List[ (Long, A)]
}
