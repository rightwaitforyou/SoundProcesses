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
   private case object Stopped   extends NonScheduled { def isRunning = false }
   private case object FreeWheel extends NonScheduled { def isRunning = true  }
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

      // the three structures maintained for the update algorithm
      // (1) for each observed timed proc, store information about all scans whose source is a grapheme.
      //     an observed proc is one whose time span overlaps the current span in the transport info.
      //     the value found in the map is a tuple. the tuple's first element is the _stale_ ID which
      //     corresponds to the ID when the underlying grapheme value was stored in structure (2), thereby
      //     allowing to find that value in (2). the tuple's second element is a map from the scan keys
      //     to a tuple consisting of a time value and the grapheme value. the time value is the value at
      //     which the grapheme value was stored in (2)
      // (2) a skiplist is used as priority queue for the next interesting point in time at which the
      //     transport needs to emit an advancement message. the value is a map from stale timed-proc ID's
      //     to a map from scan keys to grapheme values, i.e. for scans whose source is a grapheme.
      // (3) a refreshment map for the timed procs
      private val gMap: IdentifierMap[ S#ID, S#Tx, (S#ID, Map[ String, (Long, Grapheme.Value) ])]  = ??? // (1)
      private val gPrio: SkipList.Map[ I, Long, Map[ S#ID, Map[ String, Grapheme.Value ]]]         = ??? // (2)
      private val timedMap: IdentifierMap[ S#ID, S#Tx, TimedProc[ S ]]                             = ??? // (3)

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

      // [A]
      // algorithm: given that the transport arrived at exactly info.nextProcTime, update the structures in
      // anticipation of the next scheduling, and collect event dispatch information.
      // - find the procs to remove and add via group.eventsAt.
      // - for the removed procs, remove the corresponding entries in (1), (2), and (3)
      // - for the added procs, find all sinks whose source connects to a grapheme. calculate the values
      //   of those graphemes at the current transport time. this goes with the Transport.Update so that
      //   listeners do not need to perform that step themselves. Also this information is useful because it
      //   yields the ceil time ct; if no value is found at the current transport time, find the ceiling time ct
      //   explicitly; for ct, evaluate the grapheme value and store it in (1) and (2) accordingly.

      // [B]
      // algorithm: given that that the transport arrived at a time which was unobserved by the structures
      // (e.g. before the info's start frame, or after the info's nextProcTime).
      // - find the procs with to remove and add via range searches
      // - proceed as in [A]

      // [C]
      // algorithm: given that the transport arrived at exactly info.nextGraphemeTime, update the
      // structures in anticipation of the next scheduling, and collect event dispatch information.
      // - in (2) find and remove the map for the given time frame
      // - for each scan (S#ID, String) collect the grapheme values so that they be dispatched
      //   in the advancement message
      // - and for each of these scans, look up the timed proc through (3) and gather the new next grapheme
      //   values, store (replace) them in (1) and (2), and calculate the new nextGraphemeTime.

      // [D]
      // algorithm: given that the transport arrived at a time which was unobserved by the structures
      // (e.g. before the info's start frame, or after the info's nextGraphemeTime).
      // - assume that interesting procs have already been removed and added (algorithm [A] or [B])
      // - because iterator is not yet working for IdentifierMap, make a point intersection of the group
      //   at the new time, yielding all timed procs active at that point
      // - for each timed proc, look up the entries in (1). if the time value stored there is still valid,
      //   ignore this entry. a point is still valid, if the new transport time is >= info.frame and <= the
      //   value stored here in (1). otherwise, determine the ceil time for that grapheme. if this time is
      //   the same as was stored in (1), ignore this entry. otherwise, remove the old entry and replace with
      //   the new time and the new grapheme value; perform the corresponding update in (2).
      // - for the changed entries, collect those which overlap the current transport time, so that they
      //   will go into the advancement message
      // - retrieve the new nextGraphemeTime by looking at the head element in (2).

      private def performSeek( oldInfo: Info[ S ], newFrame: Long )( implicit tx: S#Tx ) {
if( VERBOSE ) println( "::: performSeek(oldInfo = " + oldInfo + ", newFrame = " + newFrame + ")" )
         if( newFrame == oldInfo.frame ) return
//         lastTime.set( newFrame )

         implicit val itx     = tx.inMemory
         val g                = group
         val oldFrame         = oldInfo.frame
         val needsNewProcTime = newFrame < oldFrame || newFrame >= oldInfo.nextProcTime
         val newFrameP        = newFrame + 1

         // algorithm [A] or [B]
         if( needsNewProcTime ) {
            // algorithm [A]
            val (itAdded, itRemoved) = if( newFrame == oldInfo.nextProcTime ) {
               // we went exactly till a known event spot
               // - find the procs to remove and add via group.eventsAt.
               g.eventsAt( newFrame )

            // algorithm [B]
            } else {
               // the new time frame lies outside the range for the known next proc event.
               // therefore we need to fire proc additions and removals (if there are any)
               // and recalculate the next proc event time after the new time frame

               // - find the procs with to remove and add via range searches

               val (remStart, remStop, addStart, addStop) = if( newFrame > oldFrame ) {
                  // ... those which end in the interval (LRP, t] && begin <= LRP must be removed ...
                  // ... those which begin in the interval (LRP, t] && end > t must be added ...
                  val skipInt = Span( oldFrame + 1, newFrameP )
                  (Span.until( oldFrame + 1 ), skipInt, skipInt, Span.from( newFrameP ))
               } else {
                  // ... those which begin in the interval (t, LRP] && end > LRP must be removed ...
                  // ... those which end in the interval (t, LRP] && begin <=t must be added ...
                  val skipInt = Span( newFrameP, oldFrame + 1 )
                  (skipInt, Span.from( oldFrame + 1 ), Span.until( newFrameP ), skipInt)
               }

               (g.rangeSearch( addStart, addStop ), g.rangeSearch( remStart, remStop ))
            }

            // - for the removed procs, remove the corresponding entries in (1), (2), and (3)
            val procRemoved = itRemoved.flatMap( flatSpans ).toIndexedSeq
            procRemoved.foreach {
               case (_, timed) =>
                  val id = timed.id
                  timedMap.remove( id )                              // in (3)
                  val entries = gMap.get( id )
                  gMap.remove( id )                                  // in (1)
                  entries.foreach {
                     case (staleID, scanMap) =>
                        scanMap.foreach {
                           case (_, (time, _)) =>
                              gPrio.get( time ).foreach { staleMap =>
                                 val newStaleMap = staleMap - staleID
                                 gPrio.add( time -> newStaleMap )    // in (2)
                              }
                        }
                  }
            }

            // - for the added procs, find all sinks whose source connects to a grapheme. calculate the values
            //   of those graphemes at the current transport time. (NOT YET: this goes with the Transport.Update so that
            //   listeners do not need to perform that step themselves. Also this information is useful because it
            //   yields the ceil time ct); if no value is found at the current transport time, find the ceiling time ct
            //   explicitly; for ct, evaluate the grapheme value and store it in (1) and (2) accordingly.
            //   store procs in (3)
            val procAdded = itAdded.flatMap( flatSpans ).toIndexedSeq
            procAdded.foreach {
               case (span, timed) =>
                  val id   = timed.id
                  val p    = timed.value
//                  var procVals = Map.empty[ String, Scan.Link ]
                  var scanMap    = Map.empty[ String, (Long, Grapheme.Value) ]
                  var skipMap    = Map.empty[ Long, Map[ String, Grapheme.Value ]]
                  p.scans.iterator.foreach {
                     case (key, scan) =>
                        scan.source match {
                           case Some( link @ Scan.Link.Grapheme( peer )) =>
//                              procVals += ...
                              peer.nearestEventAfter( newFrameP ).foreach { ceilTime =>
                                 peer.valueAt( ceilTime ).foreach { ceilValue =>
                                    scanMap   += key -> (ceilTime, ceilValue)
                                    val newMap = skipMap.getOrElse( ceilTime, Map.empty ) + (key -> ceilValue)
                                    skipMap   += ceilTime -> newMap
                                 }
                              }
//                           case Some( link @ Scan.Link.Scan( peer )) =>
//                              procVals += ...
                           case _ => None
                        }
                  }
                  gMap.put( id, id -> scanMap )       // in (1)
                  skipMap.foreach {
                     case (time, keyMap) =>
                        val newMap = gPrio.get( time ).getOrElse( Map.empty ) + (id -> keyMap)
                        gPrio.add( time -> newMap )   // in (2)
                  }
                  timedMap.put( id, timed )           // in (3)
            }

//            val nextProcTime  = g.nearestEventAfter( newFrame + 1 )
         }

         // continue algorithm [A] with removed and added procs


//         if( itAdded.nonEmpty || itRemoved.nonEmpty ) {
//            val procAdded   = itAdded.flatMap(   flatSpans ).toIndexedSeq
//            val procRemoved = itRemoved.flatMap( flatSpans ).toIndexedSeq
//
//            ??? // fire
//         }

         val needsNewGraphemeTime = oldFrame > newFrame || oldInfo.nextGraphemeTime < newFrame
         if( needsNewGraphemeTime ) {
            // the new time frame lies outside the range for the known next grapheme event.
            // therefore we need to fire grapheme changes (if there are any)
            // and recalculate the next grapheme event time after the new time frame


         } else if( newFrame == oldInfo.nextGraphemeTime ) {
            // we went exactly till a known event spot
//            graphemePrio.remove( newFrame ).flatMap { infoSeq =>
//               ???
//            }
         }

         val ggg: Grapheme[ S ] = ???
         ggg.valueAt( time )



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
