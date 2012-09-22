/*
 *  TransportImpl.scala
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

package de.sciss.synth.proc
package impl

import de.sciss.lucre.{stm, expr, data, bitemp, event => evt, DataInput, DataOutput}
import stm.{Cursor, Sys}
import bitemp.{BiGroup, SpanLike, Span}
import expr.Expr
import evt.Event
import data.Iterator
import de.sciss.synth.expr.Booleans
import collection.immutable.{IndexedSeq => IIdxSeq}
import java.util.concurrent.{TimeUnit, ScheduledExecutorService, Executors}
import concurrent.stm.{Txn => STMTxn, TxnLocal => STMTxnLocal}

object TransportImpl {
   private val VERBOSE = false

   def apply[ S <: Sys[ S ]]( group: ProcGroup[ S ], sampleRate: Double )
                            ( implicit tx: S#Tx, cursor: Cursor[ S ]) : ProcTransport[ S ] = {
      val targets    = evt.Targets[ S ]
      val id         = targets.id
      val playingVar = Booleans.newVar[ S ]( Booleans.newConst( false ))
      val validVar   = tx.newIntVar( id, 0 )
      val lastTime   = tx.newLongVar( id, 0L )
      val csrPos     = cursor.position
      new Impl( targets, group, sampleRate, playingVar, validVar, lastTime, csrPos )
   }

   implicit def serializer[ S <: Sys[ S ]]( implicit cursor: Cursor[ S ]) : evt.NodeSerializer[ S, ProcTransport[ S ]] =
      new Ser[ S ]

   private final class Ser[ S <: Sys[ S ]]( implicit cursor: Cursor[ S ])
   extends evt.NodeSerializer[ S, ProcTransport[ S ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : ProcTransport[ S ] = {
         val id         = targets.id
         val group      = ProcGroup_.read( in, access )
         val sampleRate = in.readDouble()
         val playingVar = Booleans.readVar( in, access )
         val validVar   = tx.readIntVar( id, in )
         val lastTime   = tx.readLongVar( id, in )
         val csrPos     = cursor.position
         new Impl( targets, group, sampleRate, playingVar, validVar, lastTime, csrPos )
      }
   }

   private lazy val pool : ScheduledExecutorService = {        // system wide scheduler
      val res = Executors.newScheduledThreadPool( 1 )
      sys.addShutdownHook( shutdownScheduler() )
      res
   }
   private val cpuTime = STMTxnLocal( System.nanoTime()/1000 ) // system wide wall clock in microseconds

   def shutdownScheduler() {
     println( "Shutting down scheduler thread pool" )
     pool.shutdown()
   }

//   private def flatSpans[ S <: Sys[ S ]]( in: (SpanLike, IIdxSeq[ (Expr[ S, SpanLike ], Proc[ S ])])) : IIdxSeq[ (SpanLike, Proc[ S ])] = {
//      val span = in._1
//      in._2.map { case (_, proc) => (span, proc) }
//   }

   private def flatSpans[ S <: Sys[ S ]]( in: (SpanLike, IIdxSeq[ TimedProc[ S ]])) : IIdxSeq[ (SpanLike, TimedProc[ S ])] = {
      val span = in._1
      in._2.map { span -> _ }
   }

   private type ProcTransportUpd[ S <: Sys[ S ]] = Transport.Update[ S, Proc[ S ], Proc.Update[ S ]]

   private final class Impl[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ],
                                             val group: ProcGroup[ S ],
                                             val sampleRate: Double, playingVar: Expr.Var[ S, Boolean ],
                                             validVar: S#Var[ Int ], lastTime: S#Var[ Long ],
                                             csrPos: S#Acc )
                                           ( implicit cursor: Cursor[ S ])
   extends Transport[ S, Proc[ S ], Proc.Update[ S ]]
   with evt.Trigger.Impl[ S, ProcTransportUpd[ S ], ProcTransport[ S ]]
   with evt.StandaloneLike[ S, ProcTransportUpd[ S ], ProcTransport[ S ]]
//   with evt.Root[ S, Transport.Update[ S, Proc[ S ]]]
   {
      me =>

      private val microsPerSample = 1000000 / sampleRate

      private type Elem = Proc[ S ]

      override def toString() = "Transport" + id

      protected def writeData( out: DataOutput ) {
         group.write( out )
         out.writeDouble( sampleRate )
         playingVar.write( out )
         validVar.write( out )
         lastTime.write( out )
      }

      protected def disposeData()( implicit tx: S#Tx ) {
         playingVar.dispose()
         validVar.dispose()
         lastTime.dispose()
      }

      // ---- event ----

//      private object TransportEvent
//      extends evt.Trigger.Impl[ S, BiGroup.Collection[ S, Elem, U ], BiGroup.Collection[ S, Elem, U ], BiGroup[ S, Elem, U ]]
//      with evt.EventImpl[ S, BiGroup.Collection[ S, Elem, U ], BiGroup.Collection[ S, Elem, U ], BiGroup[ S, Elem, U ]]
//      with evt.InvariantEvent[ S, BiGroup.Collection[ S, Elem, U ], BiGroup[ S, Elem, U ]]
//      with evt.Root[ S, BiGroup.Collection[ S, Elem, U ]]
//      {
//         protected def reader : evt.Reader[ S, BiGroup[ S, Elem, U ]] = serializer( eventView )
//         def slot: Int = 1
//         def node: evt.Node[ S ] = group
//      }

      def connect()( implicit tx: S#Tx ) {
         group.changed ---> this
      }

      def disconnect()( implicit tx: S#Tx ) {
         group.changed -/-> this
      }

      def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ ProcTransportUpd[ S ]] = {
         if( pull.parents( this ).isEmpty ) {
            pull.resolve[ ProcTransportUpd[ S ]]
         } else {
            /*
               XXX TODO: if the transport is running
               - cancel the future
               - calculate interpolated current time t_now
               - if added/removed and span ends before t_now: ignore
               - if t_now contains span: fire
               - recalculate next event time _if necessary_
               - create new future
             */
            group.changed.pullUpdate( pull ).flatMap { gu: BiGroup.Update[ S, Proc[ S ], Proc.Update[ S ]] =>
               val tim = time // XXX TODO
               gu match {
                  case BiGroup.Added( gr, span, elem ) =>
                     if( span.contains( tim )) {

                        None
                     } else if( span.overlaps( Span.from( tim ))) {

                        None
                     } else {
                        None
                     }
                  case BiGroup.Removed( gr, span, elem ) =>
                     None
                  case BiGroup.Element( gr, changes ) =>
                     None
               }
            }
         }
      }

      def iterator( implicit tx: S#Tx ) : Iterator[ S#Tx, (SpanLike, TimedProc[ S ])] =
         group.intersect( time ).flatMap( flatSpans )

      def seek( time: Long )( implicit tx: S#Tx ) {
         advance( playing = false, lastTime.get, time, true, true )
      }

      private def advance( playing: Boolean, oldFrame: Long, newFrame: Long,
                           hasProcEvent: Boolean, hasParEvent: Boolean )( implicit tx: S#Tx ) {
if( VERBOSE ) println( "::: advance(" + playing + ", " + oldFrame + ", " + newFrame + ")" )
         if( newFrame == oldFrame ) return
         lastTime.set( newFrame )

         val (removed, added) = if( hasProcEvent ) {
            val (remStart, remStop, addStart, addStop) = if( newFrame > oldFrame ) {
               // ... those which end in the interval (LRP, t] && begin <= LRP must be removed ...
               // ... those which begin in the interval (LRP, t] && end > t must be added ...
               val skipInt = Span( oldFrame + 1, newFrame + 1 )
               (Span.until( oldFrame + 1 ), skipInt, skipInt, Span.from( newFrame + 1 ))
            } else {
               // ... those which begin in the interval (t, LRP] && end > LRP must be removed ...
               // ... those which end in the interval (t, LRP] && begin <=t must be added ...
               val skipInt = Span( newFrame + 1, oldFrame + 1 )
               (skipInt, Span.from( oldFrame + 1 ), Span.until( newFrame + 1 ), skipInt)
            }

            (group.rangeSearch( remStart, remStop ).flatMap( flatSpans ).toIndexedSeq,
             group.rangeSearch( addStart, addStop ).flatMap( flatSpans ).toIndexedSeq)
         } else (IIdxSeq.empty, IIdxSeq.empty)

         val params = if( hasParEvent  ) {
            var res = IIdxSeq.empty[ (SpanLike, TimedProc[ S ], Map[ String, Param ])]
            group.intersect( newFrame ).foreach { case (span, entries) =>
               entries.foreach { case timed =>
// XXX TODO
//                  val par = timed.value.par
                  var map = Map.empty[ String, Param ]
//                  par.keys.foreach { key =>
//                     par.get( key ).foreach { bi =>
//                        val oldExO = bi.at( oldFrame )
//                        val newExO = bi.at( newFrame )
//                        newExO.foreach { newEx =>
//                           if( oldExO != newExO ) {
//                              map += key -> newEx.value
//                           }
//                        }
//                     }
//                  }
                  if( map.nonEmpty ) res :+= (span, timed, map)
               }
            }
            res
         } else IIdxSeq.empty

//         if( removed.nonEmpty || added.nonEmpty || params.nonEmpty ) {
            fire( Transport.Advance( this, playing, newFrame, added, removed, params ))
//         }
      }

      def playing( implicit tx: S#Tx ) : Expr[ S, Boolean ] = playingVar.get

      def playing_=( expr: Expr[ S, Boolean ])( implicit tx: S#Tx ) {
         val wasPlaying = playingVar.get.value
         val isPlaying  = expr.value
         playingVar.set( expr )
         if( wasPlaying != isPlaying ) {
            if( isPlaying ) play() else stop()
         }
      }

      private def play()( implicit tx: S#Tx ) {
         scheduleNext()
         fire( Transport.Play( this ))
      }

      private def stop()( implicit tx: S#Tx ) {
         validVar.transform( _ + 1 )
         fire( Transport.Stop( this ))
      }

      private def scheduleNext()( implicit tx: S#Tx ) {
         val oldFrame      = lastTime.get
         val searchStart   = oldFrame + 1
         val procMin       = group.nearestEventAfter( searchStart ).getOrElse( Long.MaxValue )
         val hasProcEvent  = procMin != Long.MaxValue
//         val innerSpan     = if( hasProcEvent ) {
//            Span( searchStart, procMin )
//         } else {
//            Span.from( searchStart )
//         }
         val /* var */ parMin = Long.MaxValue
// XXX TODO
//         group.intersect( innerSpan ).foreach { case (span, entries) =>
//            entries.foreach { case timed =>
//               val par = timed.value.par
//               par.keys.foreach { key =>
//                  par.get( key ).foreach { bi =>
//                     bi.nearestEventAfter( searchStart ) match {
//                        case Some( time ) if (time < parMin) && (span.compareStart( time ) < 0) && (span.compareStop( time ) > 0) => parMin = time
//                        case _ =>
//                     }
//                  }
//               }
//            }
//         }
         val hasParEvent = parMin != Long.MaxValue

         if( hasProcEvent || hasParEvent ) {
            val newFrame   = math.min( procMin, parMin )
            val delay      = ((newFrame - oldFrame) * microsPerSample).toLong
            val v          = validVar.get
            val logical    = cpuTime.get( tx.peer )
            val jitter     = System.nanoTime()/1000 - logical
            val effective  = math.max( 0L, delay - jitter )
if( VERBOSE ) println( "::: scheduled: delay = " + delay + ", effective = " + effective + ", new frame = " + newFrame )
            STMTxn.afterCommit( _ => {
               pool.schedule( new Runnable {
                  def run() { cursor.step { implicit tx =>
                     val self = tx.refresh[ ProcTransport[ S ]]( csrPos, me )
                     self.eventReached( v, logical + delay, oldFrame, newFrame, hasProcEvent, hasParEvent )
                  }}
               }, effective, TimeUnit.MICROSECONDS )
            })( tx.peer )
         }
      }

      def eventReached( valid: Int, newLogical: Long, oldFrame: Long, newFrame: Long,
                        hasProcEvent: Boolean, hasParEvent: Boolean )( implicit tx: S#Tx ) {
         // if the transport had been stopped between scheduling and actual
         // execution of the scheduled Runnable, then we will find a
         // discrepancy in validVar. In that case, just don't do anything.
         if( valid == validVar.get ) {
            cpuTime.set( newLogical )( tx.peer )
            advance( playing = true, oldFrame = oldFrame, newFrame = newFrame, hasProcEvent, hasParEvent )
            scheduleNext()
         }
      }

      def time( implicit tx: S#Tx ) : Long = lastTime.get   // XXX // Expr[ S, Long ] = timeExpr

      // ---- event stuff ----

      def changed : Event[ S, ProcTransportUpd[ S ], ProcTransport[ S ]] = this

      protected def reader: evt.Reader[ S, ProcTransport[ S ]] = serializer[ S ]
   }
}
