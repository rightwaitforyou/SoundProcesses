/*
 *  BiPin.scala
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
import txn.{SkipList, HASkipList}
import de.sciss.lucre.{event, DataInput, DataOutput}
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.lucre.stm.{Disposable, Serializer, TxnSerializer, Writer, Sys}
import event.{Constant, Node, Intruder, EventLikeSerializer, Change, Reader, Dummy, EventLike, Pull, Targets, Trigger, StandaloneLike, Event}

/**
 * TODO: Entry.Dynamic should not require its own targets. It would be much better to realise a group event
 * in the var itself.
 */
object BiPin {
   private val MIN_TIME = Long.MinValue

   type Update[ A ] = IIdxSeq[ Region[ A ]]

   trait Var[ S <: Sys[ S ], A ] extends BiPin[ S, A ] with Disposable[ S#Tx ] /* with Source[ S, A ] with Sink[ S, A ] */ {
      def get( implicit tx: S#Tx, time: Chronos[ S ]) : Expr[ S, A ]
      def getAt( time: Long )( implicit tx: S#Tx ) : Expr[ S, A ]
      def set( value: Expr[ S, A ])( implicit tx: S#Tx ) : Unit
      def add( time: Expr[ S, Long ], value: Expr[ S, A ])( implicit tx: S#Tx ) : Option[ Expr[ S, A ]]
      def remove( time: Expr[ S, Long ])( implicit tx: S#Tx ) : Boolean
      def removeAt( time: Long )( implicit tx: S#Tx ) : Option[ Expr[ S, Long ]]
      def removeAll( span: SpanLike )( implicit tx: S#Tx ) : Unit
   }

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

   def readVar[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc )
                                  ( implicit tx: S#Tx, peerType: BiType[ A ]) : Var[ S, A ] = {
      val targets = Targets.read[ S ]( in, access )
      val ordered = {
         implicit val ser     = Entry.serializer[ S, A ] // peerType.serializer[ S ]
         implicit val ord     = Ordering.by[ (Long, Expr[ S, A ]), Long ]( _._1 )
         HASkipList.Map.read[ S, Long, Entry[ S, A ]]( in, access )
      }
      new Impl[ S, A ]( targets, ordered )
   }

   implicit def serializer[ S <: Sys[ S ], A ]( implicit peerType: BiType[ A ]) :
      event.Reader[ S, BiPin[ S, A ]] with TxnSerializer[ S#Tx, S#Acc, BiPin[ S, A ]] = new Ser[ S, A ]

   implicit def varSerializer[ S <: Sys[ S ], A ]( implicit peerType: BiType[ A ]) :
      event.Reader[ S, Var[ S, A ]] with TxnSerializer[ S#Tx, S#Acc, Var[ S, A ]] = new VarSer[ S, A ]

   private def newVar[ S <: Sys[ S ], A ]( targets: Targets[ S ], init: Expr[ S, A ])( implicit tx: S#Tx,
                                                        peerType: BiType[ A ]) : Var[ S, A ] = {
      val ordered = {
         implicit val ser     = Entry.serializer[ S, A ] // peerType.serializer[ S ]
         implicit val ord     = Ordering.by[ (Long, Expr[ S, A ]), Long ]( _._1 )
         HASkipList.Map.empty[ S, Long, Entry[ S, A ]]
      }
      ordered.add( MIN_TIME -> Entry( MIN_TIME, peerType.longType.newConst( MIN_TIME ), init ))
      new Impl[ S, A ]( targets, ordered )
   }

   private final class Ser[ S <: Sys[ S ], A ]( implicit peerType: BiType[ A ])
   extends event.Reader[ S, BiPin[ S, A ]] with TxnSerializer[ S#Tx, S#Acc, BiPin[ S, A ]] {
      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : BiPin[ S, A ] = {
         read( in, access, Targets.read[ S ]( in, access ))
      }
      def read( in: DataInput, access: S#Acc, targets: Targets[ S ])( implicit tx: S#Tx ) : BiPin[ S, A ] = {
         val ordered = {
            implicit val ser     = Entry.serializer[ S, A ] // peerType.serializer[ S ]
            implicit val ord     = Ordering.by[ (Long, Expr[ S, A ]), Long ]( _._1 )
            HASkipList.Map.read[ S, Long, Entry[ S, A ]]( in, access )
         }
         new Impl[ S, A ]( targets, ordered )
      }
      def write( v: BiPin[ S, A ], out: DataOutput ) { v.write( out )}
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
            HASkipList.Map.read[ S, Long, Entry[ S, A ]]( in, access )
         }
         new Impl[ S, A ]( targets, ordered )
      }
      def write( v: Var[ S, A ], out: DataOutput ) { v.write( out )}
   }

   private object Entry {
      def serializer[ S <: Sys[ S ], A ]( implicit peer: BiType[ A ]) : TxnSerializer[ S#Tx, S#Acc, Entry[ S, A ]] =
         new Ser[ S, A ]

      private def readFull[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc, targets: Targets[ S ])
                                              ( implicit tx: S#Tx, peer: BiType[ A ]) : Dynamic[ S, A ] = {
         implicit val cacheSer   = TxnSerializer.tuple2[ S#Tx, S#Acc, Long, A ]( Serializer.Long, peer.ValueSer )
         val cacheVar            = tx.readVar[ (Long, A) ]( targets.id, in )
         val time                = peer.longType.readExpr[ S ]( in, access )
         val value               = peer.readExpr[ S ]( in, access )
         new Dynamic[ S, A ]( targets, cacheVar, time, value )
      }

      private final class Ser[ S <: Sys[ S ], A ]( implicit peer: BiType[ A ])
      extends EventLikeSerializer[ S, Entry[ S, A ]] {
         def read( in: DataInput, access: S#Acc, targets: Targets[ S ])( implicit tx: S#Tx ) : Entry[ S, A ] = {
            readFull( in, access, targets )
         }

         def readConstant( in: DataInput )( implicit tx: S#Tx ) : Entry[ S, A ] = {
            val valueVal = peer.readValue( in )
            new Static[ S, A ]( valueVal )
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

      def apply[ S <: Sys[ S ], A ]( timeVal: Long, time: Expr[ S, Long ], value: Expr[ S, A ])
                                   ( implicit tx: S#Tx, peerType: BiType[ A ]) : Entry[ S, A ] = {
         val valueVal = value.value
         if( time.isInstanceOf[ Expr.Const[ _, _ ]] && value.isInstanceOf[ Expr.Const[ _, _ ]]) {
            new Static[ S, A ]( valueVal )
         } else {
            val targets             = Targets[ S ]
            implicit val cacheSer   = TxnSerializer.tuple2[ S#Tx, S#Acc, Long, A ]( Serializer.Long, peerType.ValueSer )
            val cacheVar            = tx.newVar( targets.id, (timeVal, valueVal) )
            new Dynamic[ S, A ]( targets, cacheVar, time, value )
         }
      }

      final private[BiPin] case class Static[ S <: Sys[ S ], A ]( valueVal: A )( implicit peerType: BiType[ A ])
      extends Entry[ S, A ] with Dummy[ S, Change[ (Long, A) ], Entry[ S, A ]] with Constant[ S ] {
         protected def writeData( out: DataOutput ) {
            peerType.writeValue( valueVal, out )
         }

         def valueCache( implicit tx: S#Tx ) : A = valueVal
         def value : Expr[ S, A ]      = peerType.newConst[ S ]( valueVal )
      }

      final private[BiPin] case class Dynamic[ S <: Sys[ S ], A ]( targets: Targets[ S ], cacheVar: S#Var[ (Long, A) ],
                                                                  time: Expr[ S, Long ], value: Expr[ S, A ])
      extends Entry[ S, A ] with StandaloneLike[ S, Change[ (Long, A) ], Entry[ S, A ]] {
         private def timeCache(  implicit tx: S#Tx ) : Long = cacheVar.get._1
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

         def updateCache()( implicit tx: S#Tx ) : Long = {
            val newTime = time.value
            cacheVar.set( (newTime, value.value) )
            newTime
         }
      }
   }
   private sealed trait Entry[ S <: Sys[ S ], A ] extends EventLike[ S, Change[ (Long, A) ], Entry[ S, A ]] with Writer {
      def valueCache( implicit tx: S#Tx ) : A
      def value: Expr[ S, A ]
   }

   final case class Region[ A ]( span: SpanLike, value: A )

   private final class Impl[ S <: Sys[ S ], A ]( protected val targets: Targets[ S ],
                                                 ordered: SkipList.Map[ S, Long, Entry[ S, A ]])
                                               ( implicit peerType: BiType[ A ])
   extends Var[ S, A ]
   with Trigger.Impl[ S, Update[ A ], Update[ A ], BiPin[ S, A ]]
   with StandaloneLike[ S, Update[ A ], BiPin[ S, A ]] {
      protected def reader = serializer[ S, A ]

      private[lucre] def connect()( implicit tx: S#Tx ) {
         var dirty   = IIdxSeq.empty[ (Long, Entry.Dynamic[ S, A ])]

         ordered.iterator.foreach {
            case (timeCache, e @ Entry.Dynamic( _, _, timeVar, _ )) =>
               val tNow = timeVar.value
               if( timeCache != tNow ) {
                  dirty :+= (timeCache, e)
               } else {
                  e ---> this
               }

            case _ =>
         }

         dirty.foreach { case (timeCache, e) =>
            ordered -= timeCache
            val newTime = e.updateCache()
            ordered += newTime -> e
            e ---> this
         }
      }
      private[lucre] def disconnect()( implicit tx: S#Tx ) {
         ordered.valuesIterator.foreach( _ -/-> this )
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
               val e = Intruder.devirtualize( sel, reader ).asInstanceOf[ Entry.Dynamic[ S, A ]]
               e.pullUpdate( pull ).foreach {
                  case Change( (tOld, vOld), (tNew, vNew) ) =>
                     if( tOld == tNew ) { // time didn't change -- only one region changed
                        val span = ordered.ceil( tOld + 1 ) match {
                           case Some( (tSucc, _) ) => Span( tOld, tSucc )
                           case None               => Span.from( tOld )
                        }
                        regions :+= Region( span, vNew )
                     } else {             // time did change -- two changed regions, and need to re-insert entries
                        val span1 = ordered.ceil( tOld + 1 ) match {
                           case Some( (tSucc, _) ) => Span( tOld, tSucc )
                           case None               => Span.from( tOld )
                        }
                        val r1      = Region( span1, valueCache( tOld ))
                        regions :+= r1
                        val span2 = ordered.ceil( tNew + 1 ) match {
                           case Some( (tSucc, _) ) => Span( tNew, tSucc )
                           case None               => Span.from( tNew )
                        }
                        val r2      = Region( span2, vNew )
                        regions :+= r2

                        ordered -= tOld
                        val tNew2 = e.updateCache()
                        assert( tNew2 == tNew )
                        ordered += tNew -> e
                     }
               }
            }
            Some( regions )
         }
      }

      def debugList()( implicit tx: S#Tx ) : List[ (Long, A)] =
         ordered.iterator.map( tup => (tup._1, tup._2.value.value) ).toList

      def getAt( time: Long )( implicit tx: S#Tx )   : Expr[ S, A ] = ordered.floor( time ).get._2.value
      def get( implicit tx: S#Tx, chr: Chronos[ S ]) : Expr[ S, A ] = getAt( chr.time.value )

      def valueAt( time: Long )( implicit tx: S#Tx )   : A  = getAt( time ).value
      def value( implicit tx: S#Tx, chr: Chronos[ S ]) : A  = valueAt( chr.time.value )

      private def isConnected( implicit tx: S#Tx ) : Boolean = targets.nonEmpty

      def add( time: Expr[ S, Long ], value: Expr[ S, A ])( implicit tx: S#Tx ) : Option[ Expr[ S, A ]] = {
         val start         = time.value
         val newEntry      = Entry( start, time, value )
         val con           = isConnected
         val succ          = ordered.ceil( start + 1 )

         val oldOption = ordered.add( start -> newEntry )
         if( con ) {
            oldOption.foreach( _ -/-> this )
            newEntry ---> this
            val span = succ match {
               case Some( (tSucc, _) ) => Span( start, tSucc )
               case None               => Span.from( start )
            }
            fire( IIdxSeq( Region( span, value.value )))
         }
         oldOption.map( _.value )
      }
      def set( value: Expr[ S, A ])( implicit tx: S#Tx ) {
         ordered.clear()
         ordered.add( MIN_TIME -> Entry( MIN_TIME, peerType.longType.newConst( MIN_TIME ), value ))
         if( isConnected ) {
            fire( IIdxSeq( Region( Span.All, value.value )))
         }
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

      private def valueCache( time: Long )( implicit tx: S#Tx ) : A = ordered.floor( time ).get._2.valueCache

      protected def writeData( out: DataOutput ) {
         ordered.write( out )
      }

      protected def disposeData()( implicit tx: S#Tx ) {
         ordered.dispose()
      }

      def projection( implicit tx: S#Tx, time: Chronos[ S ]) : Expr[ S, A ] =
         peerType.newProjection[ S ]( this )

      def changed : Event[ S, Update[ A ], BiPin[ S, A ]] = this
   }
}
sealed trait BiPin[ S <: Sys[ S ], A ] extends /* BiSource[ S#Tx, Chronos[ S ], Expr[ S, A ]] with */ Writer {
   def value( implicit tx: S#Tx, time: Chronos[ S ]) : A
   def valueAt( time: Long )( implicit tx: S#Tx ) : A
   def projection( implicit tx: S#Tx, time: Chronos[ S ]) : Expr[ S, A ]

   def changed : Event[ S, BiPin.Update[ A ], BiPin[ S, A ]]

   def debugList()( implicit tx: S#Tx ) : List[ (Long, A)]
}
