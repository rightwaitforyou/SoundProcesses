package de.sciss.synth
package proc
package impl

import de.sciss.lucre.{stm, bitemp, expr, data}
import stm.{IdentifierMap, Sys, Cursor}
import bitemp.{SpanLike, Span}
import data.SkipList
import expr.Expr
import collection.immutable.{IndexedSeq => IIdxSeq}
import concurrent.stm.{Txn, TxnLocal}
import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}

object TransportViewImpl {
   def apply[ S <: Sys[ S ]]( transport: ProcTransport[ S ])( implicit tx: S#Tx, cursor: Cursor[ S ]) /* : TransportView[ S ] */ = {
      val time    = transport.time
      val flags   = tx.newInMemoryIDMap[ Flag[ S ]]
      val view: Impl[ S ] = ??? //    = new Impl( transport, ???, flags )
      transport.group.intersect( time ).foreach {
         case (Span.HasStart( span ), seq) =>
            seq.foreach { timed => view.add( span, timed )}
         case _ =>
      }
      view
   }

   private sealed trait Flag[ +S ] {
      def time: Long
   }
   private final case class GroupFlag( time: Long ) extends Flag[ Nothing ]
   private final case class ProcFlag[ S <: Sys[ S ]]( id: S#ID, time: Long ) extends Flag[ S ]

   private final case class Info[ S <: Sys[ S ]]( cpuTime: Long, frame: Long, state: State,
                                                  nextProcTime: Long, nextGraphemeTime: Long,
                                                  graphemes: IIdxSeq[ GraphemeInfo[ S#ID ]]) {
      def isRunning = state.isRunning
//      def touches( time: Long ) = frame <= time && state.targetFrame >= time
   }

   private sealed trait State {
      /**
       * For scheduled, the frame at which the scheduled event will happen, otherwise `Long.MaxValue`
       */
      def targetFrame: Long
      def isRunning: Boolean
   }
   private sealed trait NonScheduled extends State {
      final def targetFrame = Long.MaxValue
   }
   private case object Stopped extends NonScheduled { def isRunning = false }
   private case object FreeWheel extends NonScheduled { def isRunning = true }
   private case class Scheduled( targetFrame: Long ) extends State { def isRunning = true }

//   private sealed trait Running[ +S ] extends State[ S ] { final def isRunning = true }
//   private final case class Stopped( cpuTime: Long, frame: Long ) extends NonScheduled { def isRunning = false }
//   private final case class FreeWheel( cpuTime: Long, frame: Long ) extends NonScheduled with Running[ Nothing ]
//   private final case class Scheduled[ S <: Sys[ S ]]( cpuTime: Long, frame: Long, targetFrame: Long,
//                                                       nextProcTime: Long, nextGraphemeTime: Long,
//                                                       graphemes: IIdxSeq[ GraphemeInfo[ S#ID ]])
//   extends Running[ S ] {
//      def nextTime   = math.min( nextProcTime, nextGraphemeTime )
////      def isValid    = nextTime != Long.MaxValue
//   }

   private trait GraphemeInfo[ +ID ] {
      def id: ID
      def key: String
   }

   private lazy val pool : ScheduledExecutorService = {        // system wide scheduler
      val res = Executors.newScheduledThreadPool( 1 )
      sys.addShutdownHook( shutdownScheduler() )
      res
   }

   private val cpuTime = TxnLocal( System.nanoTime()/1000 )    // system wide wall clock in microseconds

   var VERBOSE = true

   private def shutdownScheduler() {
     if( VERBOSE )println( "Shutting down scheduler thread pool" )
     pool.shutdown()
   }

   private def flatSpans[ S <: Sys[ S ]]( in: (SpanLike, IIdxSeq[ TimedProc[ S ]])) : IIdxSeq[ (SpanLike, TimedProc[ S ])] = {
      val span = in._1
      in._2.map { span -> _ }
   }

//   private val emptySeq = IIdxSeq.empty

   private val itNoProcs = (data.Iterator.empty, data.Iterator.empty)

   final class Impl[ S <: Sys[ S ]]( groupStale: ProcGroup[ S ],val sampleRate: Double,
                                     playingVar: Expr.Var[ S, Boolean ], validVar: I#Var[ Int ],
                                     infoVar: I#Var[ Info[ S ]],
                                     csrPos: S#Acc,
                                     flags: IdentifierMap[ S#ID, S#Tx, Flag[ S ]])
                                   ( implicit cursor: Cursor[ S ]) {

//      private val prevTime: I#Var[ Long ] = ???
//      private val nextTime: I#Var[ Long ] = ???

      private implicit val procGroupSer = ProcGroup_.serializer[ S ]

      private val microsPerSample = 1000000 / sampleRate

      private val graphemePrio: SkipList.Map[ I, Long, IIdxSeq[ GraphemeInfo[ S#ID ]]] = ???

      private def play()( implicit tx: S#Tx ) {
         if( isPlaying ) return
//         scheduleNext( ??? )
      }

//      private def setCurrentFrame( time: Long )( implicit tx: S#Tx ) {
//
//      }

      private def calcCurrentTime( info: Info[ S ])( implicit tx: S#Tx ) : Long = {
         val startFrame = info.frame
         val state      = info.state
         if( state.isRunning ) {
            val logicalNow    = cpuTime.get( tx.peer )
            val logicalDelay  = logicalNow - info.cpuTime
            val stopFrame     = state.targetFrame
            math.min( stopFrame - 1, startFrame + (logicalDelay / microsPerSample).toLong )
         } else startFrame
      }

      def time( implicit tx: S#Tx ) : Long = calcCurrentTime( infoVar.get( tx.inMemory ))

      private def fire( evt: Transport.Update[ S, Proc[ S ], Proc.Update[ S ]])( implicit tx: S#Tx ) {
         ???
      }

      private def invalidateScheduled()( implicit tx: S#Tx ) {
         validVar.transform( _ + 1 )( tx.inMemory )   // invalidate scheduled tasks
      }

      def stop()( implicit tx: S#Tx ) {
         implicit val itx = tx.inMemory
         val oldInfo = infoVar.get
         if( !oldInfo.isRunning ) return

         invalidateScheduled()
         val newInfo = oldInfo.copy( cpuTime = cpuTime.get( tx.peer ), frame = calcCurrentTime( oldInfo ), state = Stopped )
         infoVar.set( newInfo )
         ??? // fire( newState )
      }

//      private def processScheduled( evt: Scheduled[ S ])( implicit tx: S#Tx ) {
//         ???
//      }

      def group( implicit tx: S#Tx ) : ProcGroup[ S ] =  tx.refresh( csrPos, groupStale )

      def isPlaying( implicit tx: S#Tx ) : Boolean = infoVar.get( tx.inMemory ).state.isRunning // playingVar.value

      def seek( time: Long )( implicit tx: S#Tx ) {
         implicit val itx = tx.inMemory
         val oldInfo = infoVar.get
         if( oldInfo.isRunning ) {
//            val t = calcCurrentTime( oldState )
            invalidateScheduled()
            performSeek(  oldInfo = oldInfo, newFrame = time )
            scheduleNext( oldInfo = oldInfo, newFrame = time )
         } else {
            val newInfo = oldInfo.copy( cpuTime = cpuTime.get( tx.peer ), frame = time )
            infoVar.set( newInfo )
         }
         ??? // fire
      }

      private def performSeek( oldInfo: Info[ S ], newFrame: Long )( implicit tx: S#Tx ) {
if( VERBOSE ) println( "::: performSeek(oldInfo = " + oldInfo + ", newFrame = " + newFrame + ")" )
         if( newFrame == oldInfo.frame ) return
//         lastTime.set( newFrame )

         val g = group
         val oldFrame = oldInfo.frame
         val (itAdded, itRemoved) = if( oldFrame > newFrame || oldInfo.nextProcTime < newFrame ) {
            // the new time frame lies outside the range for the known next proc event.
            // therefore we need to fire proc additions and removals (if there are any)
            // and recalculate the next proc event time after the new time frame
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

            (g.rangeSearch( addStart, addStop ), g.rangeSearch( remStart, remStop ))

//            val nextProcTime  = g.nearestEventAfter( newFrame + 1 )

         } else if( newFrame == oldInfo.nextProcTime ) {
            // we went exactly till a known event spot
            g.eventsAt( newFrame )

         } else {
            itNoProcs
         }

         if( itAdded.nonEmpty || itRemoved.nonEmpty ) {
            val procAdded   = itAdded.flatMap(   flatSpans ).toIndexedSeq
            val procRemoved = itRemoved.flatMap( flatSpans ).toIndexedSeq

            ??? // fire
         }

         if( oldFrame > newFrame || oldInfo.nextGraphemeTime < newFrame ) {
            // the new time frame lies outside the range for the known next grapheme event.
            // therefore we need to fire grapheme changes (if there are any)
            // and recalculate the next grapheme event time after the new time frame


         }
//         if( removed.nonEmpty || added.nonEmpty || params.nonEmpty ) {
//            fire( Transport.Advance( this, playing, newFrame, added, removed, params ))
//         }
      }

      private def scheduleNext( oldInfo: Info[ S ], newFrame: Long )( implicit tx: S#Tx ) {
         implicit val itx  = tx.inMemory
         val searchStart   = newFrame + 1

         ???
//         val newEvt0       = if( oldEvt.nextProcTime <= oldFrame ) {       // have to search for the next group event
//            oldEvt.copy( nextProcTime = group.nearestEventAfter( searchStart ).getOrElse( Long.MaxValue ))
//         } else oldEvt
//
//         val newEvt1       = if( oldEvt.nextGraphemeTime <= oldFrame ) {   // have to search for the next grapheme event
//            val (gTime, gInfos) = graphemePrio.ceil( searchStart ).getOrElse( Long.MaxValue -> IIdxSeq.empty )
//            newEvt0.copy( nextGraphemeTime = gTime, graphemes = gInfos )
//         } else newEvt0
//
//         val newFrame   = newEvt1.nextTime
//         if( newFrame == Long.MaxValue ) return
//
//         val logicalDelay = ((newFrame - oldFrame) * microsPerSample).toLong
//         val v          = validVar.get
//         val logicalNow = cpuTime.get( tx.peer )
//         val newEvt     = newEvt1.copy( cpuTime = logicalNow )
//         state.set( newEvt )
//         val jitter     = System.nanoTime()/1000 - logicalNow
//         val actualDelay = math.max( 0L, logicalDelay - jitter )
//if( VERBOSE ) println( "::: scheduled: logicalDelay = " + logicalDelay + ", actualDelay = " + actualDelay + ", new frame = " + newFrame )
//         Txn.afterCommit( _ => {
//            pool.schedule( new Runnable {
//               def run() { cursor.step { implicit tx =>
//                  // if the transport had been stopped between scheduling and actual
//                  // execution of the scheduled Runnable, then we will find a
//                  // discrepancy in validVar. In that case, just don't do anything.
//                  implicit val itx: I#Tx = tx.inMemory
//                  if( v == validVar.get ) {
//                     eventReached( logicalNow + logicalDelay, oldFrame, newFrame, newEvt )
//                  }
//               }}
//            }, actualDelay, TimeUnit.MICROSECONDS )
//         })( tx.peer )
      }

      private def eventReached( newLogical: Long, oldFrame: Long, newFrame: Long,
                                info: Info[ S ])( implicit tx: S#Tx ) {
//         cpuTime.set( newLogical )( tx.peer )
//         processScheduled( evt )
////         advance( playing = true, oldFrame = oldFrame, newFrame = newFrame, hasProcEvent, hasParEvent )
////         scheduleNext()
         ???
      }

      def add( span: Span.HasStart, timed: TimedProc[ S ])( implicit tx: S#Tx ) {
         ???
//         implicit val itx: I#Tx = ???
//         val w = waitSpan.get
//         if( w.touches( span )) {
//            // interrupt scheduled task
//
////            val p = timed.value
////            p.scans.iterator.foreach {
////               case (key, scan) => scan.source match {
////                  case Some( Scan.Link.Grapheme( grapheme )) =>
////                     grapheme.nearestEventAfter( span.start )   // XXX
////                  case _ =>
////               }
////            }
//         } else {
//            // see if it appears earlier than the next region
//         }
      }
   }
}
