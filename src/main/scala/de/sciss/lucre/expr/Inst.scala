/*
 *  BiExpr.scala
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
import collection.immutable.{IndexedSeq => IIdxSeq}
import event.{Node, Intruder, EventLikeSerializer, Change, Reader, Constant, Dummy, EventLike, Pull, Targets, Trigger, StandaloneLike, Event}
import de.sciss.lucre.stm.{Disposable, Serializer, InMemory, TxnSerializer, Writer, Sys}

object Inst {
   type Update[ A ] = IIdxSeq[ Region[ A ]]

   def newVar[ S <: Sys[ S ], A ]( init: Expr[ S, A ])( implicit tx: S#Tx,
                                                        peerType: BiType[ A ]) : Var[ S, A ] = {
      val targets = Targets.partial[ S ]
      newVar( targets, init )
   }

   def newConfluentVar[ S <: Sys[ S ], A ]( init: Expr[ S, A ])( implicit tx: S#Tx,
                                                                 peerType: BiType[ A ]) : Var[ S, A ] = {
      val targets = Targets[ S ]
      newVar( targets, init )
   }

   private def newVar[ S <: Sys[ S ], A ]( targets: Targets[ S ], init: Expr[ S, A ])( implicit tx: S#Tx,
                                                        peerType: BiType[ A ]) : Var[ S, A ] = {
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
      event.Reader[ S, Inst[ S, A ]] with TxnSerializer[ S#Tx, S#Acc, Inst[ S, A ]] = new Ser[ S, A ]

   implicit def varSerializer[ S <: Sys[ S ], A ]( implicit peerType: BiType[ A ]) :
      event.Reader[ S, Var[ S, A ]] with TxnSerializer[ S#Tx, S#Acc, Var[ S, A ]] = new VarSer[ S, A ]

   private final class Ser[ S <: Sys[ S ], A ]( implicit peerType: BiType[ A ])
   extends event.Reader[ S, Inst[ S, A ]] with TxnSerializer[ S#Tx, S#Acc, Inst[ S, A ]] {
      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Inst[ S, A ] = {
         read( in, access, Targets.read[ S ]( in, access ))
      }
      def read( in: DataInput, access: S#Acc, targets: Targets[ S ])( implicit tx: S#Tx ) : Inst[ S, A ] = {
         val ordered = {
            implicit val ser     = Entry.serializer[ S, A ] // peerType.serializer[ S ]
            implicit val ord     = Ordering.by[ (Long, Expr[ S, A ]), Long ]( _._1 )
            HASkipList.read[ S, Entry[ S, A ]]( in, access )
         }
         new Impl[ S, A ]( targets, ordered )
      }
      def write( v: Inst[ S, A ], out: DataOutput ) { v.write( out )}
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

//   trait Source[ S <: Sys[ S ], +A ] extends Writer with Disposable[ S#Tx ] {
//      def get( implicit time: Chronos[ S ]) : Expr[ S, A ]
//   }
//
//   sealed trait Sink[ S <: Sys[ S ], -A ] {
//      def set( value: Expr[ S, A ])( implicit time: Chronos[ S ]) : Unit
//   }

   trait Var[ S <: Sys[ S ], A ] extends Inst[ S, A ] with Disposable[ S#Tx ] /* with Source[ S, A ] with Sink[ S, A ] */ {
//      def set( time: Expr[ S, Long ], value: Expr[ S, A ])( implicit tx: S#Tx ) : Unit
//      def get( time: Long )( implicit tx: S#Tx ) : Expr[ S, A ]
      def get( implicit tx: S#Tx, time: Chronos[ S ]) : Expr[ S, A ]
      def getAt( time: Long )( implicit tx: S#Tx ) : Expr[ S, A ]
      def set( value: Expr[ S, A ])( implicit tx: S#Tx ) : Unit
//      def setAt( time: Expr[ S, Long ], value: Expr[ S, A ])( implicit tx: S#Tx ) : Unit
//      def setFrom( time: Expr[ S, Long ])
      def add( time: Expr[ S, Long ], value: Expr[ S, A ])( implicit tx: S#Tx ) : Boolean
      def remove( time: Expr[ S, Long ])( implicit tx: S#Tx ) : Boolean
      def removeAt( time: Long )( implicit tx: S#Tx ) : Option[ Expr[ S, Long ]]
      def removeAll( span: SpanLike )( implicit tx: S#Tx ) : Unit
   }

   private object Entry {
      def serializer[ S <: Sys[ S ], A ]( implicit peer: BiType[ A ]) : TxnSerializer[ S#Tx, S#Acc, Entry[ S, A ]] =
         new Ser[ S, A ]

      private def readFull[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc, targets: Targets[ S ])
                                              ( implicit tx: S#Tx, peer: BiType[ A ]) : FullImpl[ S, A ] = {
//         import peer.ValueSer
         implicit val cacheSer   = TxnSerializer.tuple2[ S#Tx, S#Acc, Long, A ]( Serializer.Long, peer.ValueSer )
         val cacheVar            = tx.readVar[ (Long, A) ]( targets.id, in )
         val time                = peer.longType.readExpr[ S ]( in, access )
         val value               = peer.readExpr[ S ]( in, access )
         new FullImpl[ S, A ]( targets, cacheVar, time, value )
      }

      private final class Ser[ S <: Sys[ S ], A ]( implicit peer: BiType[ A ])
      extends EventLikeSerializer[ S, Entry[ S, A ]] {
         def read( in: DataInput, access: S#Acc, targets: Targets[ S ])( implicit tx: S#Tx ) : Entry[ S, A ] = {
            readFull( in, access, targets )
         }

         def readConstant( in: DataInput )( implicit tx: S#Tx ) : Entry[ S, A ] = {
            val timeVal       = in.readLong()
            val valueVal      = peer.readValue( in )
            new DummyImpl[ S, A ]( timeVal, valueVal )
         }
      }

      def nodeReader[ S <: Sys[ S ], A ]( implicit peer: BiType[ A ]) : Reader[ S, Node[ S ]] =
         new NodeReader[ S, A ]

      private final class NodeReader[ S <: Sys[ S ], A ]( implicit peer: BiType[ A ])
      extends Reader[ S, Node[ S ]] {
         def read( in: DataInput, access: S#Acc, targets: Targets[ S ])( implicit tx: S#Tx ) : Node[ S ] = {
            readFull( in, access, targets )
         }
      }

      implicit def ordering[ S <: Sys[ S ], A ] : txn.Ordering[ S#Tx, Entry[ S, A ]] =
         Ord.asInstanceOf[ txn.Ordering[ S#Tx, Entry[ S, A ]]]

      private object Ord extends txn.Ordering[ InMemory#Tx, Entry[ InMemory, _ ]] {
         def compare( a: Entry[ InMemory, _ ], b: Entry[ InMemory, _ ])( implicit tx: InMemory#Tx ) : Int = {
            val at = a.timeCache
            val bt = b.timeCache
            if( at < bt ) -1 else if( at > bt ) 1 else 0
         }
      }

      def apply[ S <: Sys[ S ], A ]( timeVal: Long, time: Expr[ S, Long ], value: Expr[ S, A ])
                                   ( implicit tx: S#Tx, peerType: BiType[ A ]) : Entry[ S, A ] = {
         val valueVal = value.value
         if( time.isInstanceOf[ Expr.Const[ _, _ ]] && value.isInstanceOf[ Expr.Const[ _, _ ]]) {
            new DummyImpl[ S, A ]( timeVal, valueVal )
         } else {
            val targets             = Targets[ S ]
            implicit val cacheSer   = TxnSerializer.tuple2[ S#Tx, S#Acc, Long, A ]( Serializer.Long, peerType.ValueSer )
            val cacheVar            = tx.newVar( targets.id, (timeVal, valueVal) )
            new FullImpl[ S, A ]( targets, cacheVar, time, value )
         }
      }

//      private sealed trait Impl[ S <: Sys[ S ], A ] extends Entry[ S, A ] {
//      }

      private final class DummyImpl[ S <: Sys[ S ], A ]( timeVal: Long, val valueVal: A )( implicit peerType: BiType[ A ])
      extends Entry[ S, A ] with Dummy[ S, Change[ (Long, A) ], Entry[ S, A ]] with Constant[ S ] {
         protected def writeData( out: DataOutput ) {
            out.writeLong( timeVal )
            peerType.writeValue( valueVal, out )
         }

         def timeCache( implicit tx: S#Tx ) : Long = timeVal
         def valueCache( implicit tx: S#Tx ) : A = valueVal

         def time  : Expr[ S, Long ]   = peerType.longType.newConst[ S ]( timeVal )
         def value : Expr[ S, A ]      = peerType.newConst[ S ]( valueVal )

         def updateCache()( implicit tx: S#Tx ) {
            sys.error( "Illegal state -- a constant region should not change its time value : " + this )
         }

         def isDummy = true
      }

      final class FullImpl[ S <: Sys[ S ], A ]( protected val targets: Targets[ S ], cacheVar: S#Var[ (Long, A) ],
                                                val time: Expr[ S, Long ], val value: Expr[ S, A ])
      extends Entry[ S, A ] with StandaloneLike[ S, Change[ (Long, A) ], Entry[ S, A ]] {
         def timeCache(  implicit tx: S#Tx ) : Long = cacheVar.get._1
         def valueCache( implicit tx: S#Tx ) : A    = cacheVar.get._2

         // LucreSTM issue #7 !!!
         override def toString() = "Entry(time = " + time + ", value =" + value + ")"

         protected def writeData( out: DataOutput ) {
            cacheVar.write( out )
            time.write( out )
            value.write( out )
         }

         protected def disposeData()( implicit tx: S#Tx ) {
            cacheVar.dispose()
         }

         private[lucre] def connect()( implicit tx: S#Tx ) {
            time.changed  ---> this
            value.changed ---> this
         }

         private[lucre] def disconnect()( implicit tx: S#Tx ) {
            time.changed  -/-> this
            value.changed -/-> this
         }

         protected def reader : Reader[ S, Entry[ S, A ]] = {
            sys.error( "TODO" )
         }

         @inline private def change[ B ]( before: B, now: B ) : Option[ Change[ B ]] = Change( before, now ).toOption

         private[lucre] def pullUpdate( pull: Pull[ S ])( implicit tx: S#Tx ) : Option[ Change[ (Long, A) ]] = {
            val timeChanged   = time.changed
            val valueChanged  = value.changed

            val timeChange = if( timeChanged.isSource( pull )) {
               timeChanged.pullUpdate( pull )
            } else {
               None
            }
            val valueChange = if( valueChanged.isSource( pull )) {
               valueChanged.pullUpdate( pull )
            } else {
               None
            }

            (timeChange, valueChange) match {
               case (Some( tch ), None) =>
                  val vv   = value.value
                  change( (tch.before, vv), (tch.now, vv) )
               case (None, Some( vch )) =>
                  val tv = timeCache
                  change( (tv, vch.before), (tv, vch.now) )
               case (Some( tch ), Some( vch )) =>
                  change( (tch.before, vch.before), (tch.now, vch.now) )
               case _ => None
            }
         }

         def updateCache()( implicit tx: S#Tx ) {
            cacheVar.set( (time.value, value.value) )
         }

         def isDummy = false
      }
   }
   private sealed trait Entry[ S <: Sys[ S ], A ] extends EventLike[ S, Change[ (Long, A) ], Entry[ S, A ]] with Writer {
      def timeCache(  implicit tx: S#Tx ) : Long
      def valueCache( implicit tx: S#Tx ) : A
      def time: Expr[ S, Long ]
      def value: Expr[ S, A ]
      def updateCache()( implicit tx: S#Tx ) : Unit
      def isDummy: Boolean
   }

   final case class Region[ A ]( span: SpanLike, value: A )

   private final class Impl[ S <: Sys[ S ], A ]( protected val targets: Targets[ S ],
                                                 ordered: SkipList[ S, Entry[ S, A ]])
                                               ( implicit peerType: BiType[ A ])
   extends Var[ S, A ]
   with Trigger.Impl[ S, Update[ A ], Update[ A ], Inst[ S, A ]]
   with StandaloneLike[ S, Update[ A ], Inst[ S, A ]] {
      protected def reader = serializer[ S, A ]

      private[lucre] def connect()( implicit tx: S#Tx ) {
         var dirty   = IIdxSeq.empty[ Entry[ S, A ]]

         ordered.iterator.foreach { e =>
            if( !e.isDummy ) {
               val tNow = e.time.value
               if( e.timeCache != tNow ) {
                  dirty :+= e
               } else {
                  e ---> this
               }
            }
         }

         dirty.foreach { e =>
            ordered -= e
            e.updateCache()
            ordered += e
            e ---> this
         }
      }
      private[lucre] def disconnect()( implicit tx: S#Tx ) {
         ordered.iterator.foreach( _ -/-> this )
      }

      private[lucre] def pullUpdate( pull: Pull[ S ])( implicit tx: S#Tx ) : Option[ Update[ A ]] = {
         val p = pull.parents( this )
         if( p.isEmpty ) {
            pull.resolve[ Update[ A ]]
         } else {
            val reader  = Entry.nodeReader[ S, A ]
            var regions = IIdxSeq.empty[ Region[ A ]]
            p.foreach { sel =>
// need to change package private modifier from `event` to `lucre`
//               n.devirtualize()
               val e = Intruder.devirtualize( sel, reader ).asInstanceOf[ Entry[ S, A ]]
               e.pullUpdate( pull ).foreach {
                  case Change( (tOld, vOld), (tNew, vNew) ) =>
                     if( tOld == tNew ) { // time didn't change -- only one region changed
                        val (succ, cmp) = getGeq( tOld + 1 )
                        val span = if( cmp <= 0 ) Span( tOld, succ.timeCache ) else Span.from( tOld )
                        regions :+= Region( span, vNew )
                     } else {             // time did change -- two changed regions, and need to re-insert entries
                        val (succ1, cmp1) = getGeq( tOld + 1 )
                        val span1   = if( cmp1 <= 0 ) Span( tOld, succ1.timeCache ) else Span.from( tOld )
                        val r1      = Region( span1, valueCache( tOld ))
                        regions :+= r1
                        val (succ2, cmp2) = getGeq( tNew + 1 )
                        val span2   = if( cmp2 <= 0 ) Span( tNew, succ2.timeCache ) else Span.from( tNew )
                        val r2      = Region( span2, vNew )
                        regions :+= r2

                        ordered -= e
                        e.updateCache()
                        ordered += e
                     }
               }
            }
            Some( regions )
         }
      }

      private def getGeq( time: Long )( implicit tx: S#Tx ) : (Entry[ S, A ], Int) = {
         // XXX TODO should be an efficient method in skiplist itself
         ordered.isomorphicQuery( new Ordered[ S#Tx, Entry[ S, A ]] {
            def compare( that: Entry[ S, A ])( implicit tx: S#Tx ) = {
               val t = that.timeCache
               if( time < t ) -1 else if( time > t ) 1 else 0
            }
         })
      }

      def debugList()( implicit tx: S#Tx ) : List[ (Long, A)] = ordered.toList.map( e => (e.time.value, e.value.value) )

      private def getLeq( time: Long )( implicit tx: S#Tx ) : Entry[ S, A ] = {
//         val ((succ, _), cmp) = getEntry( time )._1._2
//         if( cmp > 0 ) ???
         ordered.toList.takeWhile( _.timeCache <= time ).last // XXX TODO ouch... we do need a pred method for skiplist
      }

      def getAt( time: Long )( implicit tx: S#Tx ) : Expr[ S, A ] = getLeq( time ).value
      def get( implicit tx: S#Tx, chr: Chronos[ S ]) : Expr[ S, A ] = getAt( chr.time.value )

      def valueAt( time: Long )( implicit tx: S#Tx ) : A = getLeq( time ).value.value
      def value( implicit tx: S#Tx, chr: Chronos[ S ]) : A = valueAt( chr.time.value )

      def add( time: Expr[ S, Long ], value: Expr[ S, A ])( implicit tx: S#Tx ) : Boolean = {
         val start         = time.value
         val (succ, cmp)   = getGeq( start + 1 )
//println( "set " + tv + " -> succ = " + succ + ", cmp = " + cmp )
         val newEntry      = Entry( start, time, value )
         val con           = targets.nonEmpty
         if( con && cmp == 0 ) {  // overwriting entry!
            succ -/-> this
         }
         ordered.add( newEntry )
         if( con ) {
            if( con ) newEntry ---> this
            val span = if( cmp <= 0 ) Span( start, succ.timeCache ) else Span.from( start )
            fire( IIdxSeq( Region( span, value.value )))
         }
         sys.error( "TODO" )
      }
      def set( value: Expr[ S, A ])( implicit tx: S#Tx ) {
//         setAt( chr.time, value )
         ordered.clear()
         sys.error( "TODO" )
      }
      def removeAll( span: SpanLike )( implicit tx: S#Tx ) {
         sys.error( "TODO" )
      }
      def remove( time: Expr[ S, Long ])( implicit tx: S#Tx ) : Boolean = {
         sys.error( "TODO" )
      }
      def removeAt( time: Long )( implicit tx: S#Tx ) : Option[ Expr[ S, Long ]] = {
         sys.error( "TODO" )
      }

      private def valueCache( time: Long )( implicit tx: S#Tx ) : A = getLeq( time ).valueCache

      protected def writeData( out: DataOutput ) {
         ordered.write( out )
      }

      protected def disposeData()( implicit tx: S#Tx ) {
         ordered.dispose()
      }

      def projection( implicit tx: S#Tx, time: Chronos[ S ]) : Expr[ S, A ] =
         peerType.newProjection[ S ]( this )

      def changed : Event[ S, Update[ A ], Inst[ S, A ]] = this
   }
}
sealed trait Inst[ S <: Sys[ S ], A ] extends /* BiSource[ S#Tx, Chronos[ S ], Expr[ S, A ]] with */ Writer {
   def value( implicit tx: S#Tx, time: Chronos[ S ]) : A
   def valueAt( time: Long )( implicit tx: S#Tx ) : A
   def projection( implicit tx: S#Tx, time: Chronos[ S ]) : Expr[ S, A ]

   def changed : Event[ S, Inst.Update[ A ], Inst[ S, A ]]

   def debugList()( implicit tx: S#Tx ) : List[ (Long, A)]
}
