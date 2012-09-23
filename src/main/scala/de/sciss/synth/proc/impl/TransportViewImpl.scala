package de.sciss.synth
package proc
package impl

import de.sciss.lucre.{event => evt, DataOutput, DataInput, stm, bitemp, expr, data}
import stm.{IdentifierMap, Sys, Cursor}
import bitemp.{SpanLike, Span}
import data.SkipList
import expr.Expr
import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import concurrent.stm.{Txn, TxnLocal}
import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}
import de.sciss.synth.expr.Booleans

object TransportViewImpl {
   var VERBOSE = true

   def apply[ S <: Sys[ S ]]( group: ProcGroup[ S ], sampleRate: Double )( implicit tx: S#Tx, cursor: Cursor[ S ]) /* : TransportView[ S ] */ = {
      val targets    = evt.Targets[ S ]   // XXX TODO: partial?
//      val id         = targets.id
      val playingVar = Booleans.newVar[ S ]( Booleans.newConst( false ))
      implicit val itx = tx.inMemory
      val iid        = itx.newID()
      implicit val infoSer = dummySerializer[ Info ]
      val infoVar    = itx.newVar( iid, Info.init )
      val gMap       = tx.newInMemoryIDMap[ (S#ID, Map[ String, (Long, Grapheme.Value) ])]      // (1)
      implicit val skipSer = dummySerializer[ Map[ S#ID, Map[ String, Grapheme.Value ]]]
      val gPrio      = SkipList.Map.empty[ I, Long, Map[ S#ID, Map[ String, Grapheme.Value ]]]  // (2)
      val timedMap   = tx.newInMemoryIDMap[ TimedProc[ S ]]                                     // (3)
      val view       = new Impl[ S ]( targets, group, sampleRate, playingVar, infoVar,
                                      gMap, gPrio, timedMap, cursor.position )
//      group.intersect( time ).foreach {
//         case (Span.HasStart( span ), seq) =>
//            seq.foreach { timed => view.add( span, timed )}
//         case _ =>
//      }
      view
   }

   private def sysMicros() = System.nanoTime()/1000

   private object Info {
      val init: Info = apply( 0L, 0L, Stopped, Long.MaxValue, Long.MaxValue, 0 )
   }
   /**
    * Information about the current situation of the transport.
    *
    * @param cpuTime             the CPU time in microseconds at which the info was last updated
    * @param frame               the frame corresponding to the transport position at the time the info was updated
    * @param state               the current state of the transport scheduling (stopped, scheduled, free-wheeling).
    *                            when scheduled, the state contains the target frame aimed at in the subsequent
    *                            scheduler invocation.
    * @param nextProcTime        the next frame greater than `frame` at which a significant event happens in terms
    *                            of processes starting or stopping in the transport's group
    * @param nextGraphemeTime    the next frame greater than `frame` at which
    * @param valid               a counter which is automatically incremented by the `copy` method, used to determine
    *                            whether a scheduled runnable is still valid. the scheduler needs to read this value
    *                            before scheduling the runnable, then after the runnable is invoked, the current
    *                            info must be retrieved and its valid counter compared to the previously extracted
    *                            valid value. if both are equal, the runnable should go on executing, otherwise it
    *                            is meant to silently abort.
    */
   private final case class Info private( cpuTime: Long, frame: Long, state: State, nextProcTime: Long,
                                          nextGraphemeTime: Long, valid: Int ) {
      require( nextProcTime > frame && nextGraphemeTime > frame )

      def copy( cpuTime: Long = cpuTime, frame: Long = frame, state: State = state,
                nextProcTime: Long = nextProcTime, nextGraphemeTime: Long = nextGraphemeTime ) : Info =
         Info( cpuTime = cpuTime, frame = frame, state = state, nextProcTime = nextProcTime,
               nextGraphemeTime = nextGraphemeTime, valid = valid + 1 )

      def isRunning  = state.isRunning
      def nextTime   = math.min( nextProcTime, nextGraphemeTime )
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

   private trait GraphemeInfo[ +ID ] {
      def id: ID
      def key: String
   }

   private lazy val pool : ScheduledExecutorService = {        // system wide scheduler
      val res = Executors.newScheduledThreadPool( 1 )
      sys.addShutdownHook( shutdownScheduler() )
      res
   }

   private val cpuTime = TxnLocal( sysMicros() )    // system wide wall clock in microseconds

   private def shutdownScheduler() {
     if( VERBOSE )println( "Shutting down scheduler thread pool" )
     pool.shutdown()
   }

   private def flatSpans[ S <: Sys[ S ]]( in: (SpanLike, IIdxSeq[ TimedProc[ S ]])) : IIdxSeq[ (SpanLike, TimedProc[ S ])] = {
      val span = in._1
      in._2.map { span -> _ }
   }

   private val anyEmptySeq = IIdxSeq.empty[ Nothing ]
   @inline private def emptySeq[ A ] = anyEmptySeq.asInstanceOf[ IIdxSeq[ A ]]

   private def dummySerializer[ A ] : stm.Serializer[ I#Tx, I#Acc, A ] =
      DummySerializer.asInstanceOf[ stm.Serializer[ I#Tx, I#Acc, A ]]

   private object DummySerializer extends stm.Serializer[ I#Tx, I#Acc, Nothing ] {
      def write( v: Nothing, out: DataOutput) {}
      def read( in: DataInput, access: I#Acc )( implicit tx: I#Tx ) : Nothing = sys.error( "Operation not supported" )
   }

   final class Impl[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ],
                                     groupStale: ProcGroup[ S ],val sampleRate: Double,
                                     playingVar: Expr.Var[ S, Boolean ], /* validVar: I#Var[ Int ], */
                                     infoVar: I#Var[ Info ],
                                     gMap: IdentifierMap[ S#ID, S#Tx, (S#ID, Map[ String, (Long, Grapheme.Value) ])],
                                     gPrio: SkipList.Map[ I, Long, Map[ S#ID, Map[ String, Grapheme.Value ]]],
                                     timedMap: IdentifierMap[ S#ID, S#Tx, TimedProc[ S ]],
                                     csrPos: S#Acc )
                                   ( implicit cursor: Cursor[ S ]) {

      private implicit val procGroupSer      = ProcGroup_.serializer[ S ]
      private val          microsPerSample   = 1000000 / sampleRate

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
//      private val gMap: IdentifierMap[ S#ID, S#Tx, (S#ID, Map[ String, (Long, Grapheme.Value) ])]   // (1)
//      private val gPrio: SkipList.Map[ I, Long, Map[ S#ID, Map[ String, Grapheme.Value ]]]          // (2)
//      private val timedMap: IdentifierMap[ S#ID, S#Tx, TimedProc[ S ]]                              // (3)

      private def calcCurrentTime( info: Info )( implicit tx: S#Tx ) : Long = {
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

//      private def fire( evt: Transport.Update[ S, Proc[ S ], Proc.Update[ S ]])( implicit tx: S#Tx ) {
//         ???
//      }

//      private def invalidateScheduled()( implicit tx: S#Tx ) {
//         validVar.transform( _ + 1 )( tx.inMemory )   // invalidate scheduled tasks
//      }

      private def play()( implicit tx: S#Tx ) {
         if( isPlaying ) return
         ??? // fire( newState )
         scheduleNext()
      }

      private def stop()( implicit tx: S#Tx ) {
         implicit val itx = tx.inMemory
         val oldInfo = infoVar.get
         if( !oldInfo.isRunning ) return

         val newInfo = oldInfo.copy( cpuTime = cpuTime.get( tx.peer ),
                                     frame   = calcCurrentTime( oldInfo ),
                                     state   = Stopped )
         infoVar.set( newInfo )
         ??? // fire( newState )
      }

      def group( implicit tx: S#Tx ) : ProcGroup[ S ] =  tx.refresh( csrPos, groupStale )

      def isPlaying( implicit tx: S#Tx ) : Boolean = infoVar.get( tx.inMemory ).state.isRunning // playingVar.value

      def seek( time: Long )( implicit tx: S#Tx ) {
         advance( isSeek = true, newFrame = time )
         scheduleNext()
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
      //   ignore this entry. a point is still valid, if the new transport time is >= info.frame and < the
      //   value stored here in (1). otherwise, determine the ceil time for that grapheme. if this time is
      //   the same as was stored in (1), ignore this entry. otherwise, remove the old entry and replace with
      //   the new time and the new grapheme value; perform the corresponding update in (2).
      // - for the changed entries, collect those which overlap the current transport time, so that they
      //   will go into the advancement message
      // - retrieve the new nextGraphemeTime by looking at the head element in (2).

      /**
       * The core method: based on the previous info and the reached target frame, update the structures
       * accordingly (invalidate obsolete information, gather new information regarding the next
       * advancement), assemble and fire the corresponding events, and update the info to be ready for
       * a subsequent call to `scheduleNext`.
       *
       * @param isSeek     whether the advancement is due to a hard seek (`true`) or a regular passing of time (`false`).
       *                   the information is carried in the fired event.
       * @param newFrame   the frame which has been reached
       */
      private def advance( isSeek: Boolean, newFrame: Long )( implicit tx: S#Tx ) {
         implicit val itx     = tx.inMemory
         val oldInfo          = infoVar.get
if( VERBOSE ) println( "::: advance(isSeek = " + isSeek + "; newFrame = " + newFrame + "); oldInfo = " + oldInfo )
         if( newFrame == oldInfo.frame ) return

         val g                = group
         val oldFrame         = oldInfo.frame
         val needsNewProcTime = newFrame < oldFrame || newFrame >= oldInfo.nextProcTime
         val newFrameP        = newFrame + 1

         var procAdded        = emptySeq[ (SpanLike, TimedProc[ S ])]
         var procRemoved      = emptySeq[ (SpanLike, TimedProc[ S ])]

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

            // continue algorithm [A] with removed and added procs

            // - for the removed procs, remove the corresponding entries in (1), (2), and (3)
            procRemoved = itRemoved.flatMap( flatSpans ).toIndexedSeq
            procRemoved.foreach {
               case (_, timed) =>
                  val id = timed.id
                  timedMap.remove( id )                              // in (3)
                  val entries = gMap.get( id )
                  gMap.remove( id )                                  // in (1)
                  entries.foreach {
                     case (staleID, scanMap) =>
                        scanMap.valuesIterator.foreach {
                           case (time, _) =>
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
            procAdded = itAdded.flatMap( flatSpans ).toIndexedSeq
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
         }

         val needsNewGraphemeTime = newFrame < oldFrame || newFrame >= oldInfo.nextGraphemeTime

         // algorithm [C] or [D]
         if( needsNewGraphemeTime ) {
            // [C]
            if( newFrame == oldInfo.nextGraphemeTime ) {
               // we went exactly till a known event spot

               // - in (2) find and remove the map for the given time frame
               // - for each scan (S#ID, String) collect the grapheme values so that they be dispatched
               //   in the advancement message
               // - and for each of these scans, look up the timed proc through (3) and gather the new next grapheme
               //   values, store (replace) them in (1) and (2), and calculate the new nextGraphemeTime.

               val scanMap = gPrio.remove( newFrame ) match {
                  case Some( staleMap ) => staleMap.flatMap {
                     case (staleID, keyMap) =>
                        timedMap.get( staleID ).map( _ -> keyMap )

                     case _ => None
                  }
                  case _ => Map.empty
               }
               scanMap.foreach {
                  case (timed, removeKeyMap) =>
                     val id = timed.id
                     var (staleID, keyMap) = gMap.get( id ).getOrElse( id -> Map.empty[ String, (Long, Grapheme.Value) ])
                     removeKeyMap.keysIterator.foreach { key =>
                        val p = timed.value
                        val valueOption = p.scans.get( key ).flatMap { scan =>
                           scan.source.flatMap {
                              case Scan.Link.Grapheme( peer ) =>
                                 peer.nearestEventAfter( newFrameP ).flatMap { ceilTime =>
                                    peer.valueAt( ceilTime ).map( ceilTime -> _ )
                                 }
                              case _ => None
                           }
                        }
                        valueOption match {
                           case Some( tup @ (time, value) ) =>
                              keyMap        += key -> tup
                              val staleMap   = gPrio.get( time ).getOrElse( Map.empty )
                              val keyMap2    = staleMap.get( staleID ).getOrElse( Map.empty ) + (key -> value)
                              val newMap     = staleMap + (staleID -> keyMap2)
                              gPrio.add( time -> newMap )

                           case _ =>
                              keyMap -= key
                        }
                     }
                     gMap.put( id, staleID -> keyMap )
               }

            // [D]
            } else {
               // the new time frame lies outside the range for the known next grapheme event.
               // therefore we need to fire grapheme changes (if there are any)
               // and recalculate the next grapheme event time after the new time frame

               // - assume that interesting procs have already been removed and added (algorithm [A] or [B])
               // - because iterator is not yet working for IdentifierMap, make a point intersection of the group
               //   at the new time, yielding all timed procs active at that point
               // - for each timed proc, look up the entries in (1). if the time value stored there is still valid,
               //   ignore this entry. a point is still valid, if the new transport time is >= info.frame and < the
               //   value stored here in (1). otherwise, determine the ceil time for that grapheme. if this time is
               //   the same as was stored in (1), ignore this entry. otherwise, remove the old entry and replace with
               //   the new time and the new grapheme value; perform the corresponding update in (2).
               // - for the changed entries, collect those which overlap the current transport time, so that they
               //   will go into the advancement message

               val newProcs: Set[ TimedProc[ S ]] = procAdded.map( _._2 )( breakOut )

               // filter because new procs have already build up their scan maps
               g.intersect( newFrame ).flatMap( _._2.filterNot( newProcs.contains )).foreach { timed =>
                  val id   = timed.id
                  val p    = timed.value
                  var (staleID, keyMap) = gMap.get( id ).getOrElse( id -> Map.empty[ String, (Long, Grapheme.Value) ])
                  keyMap.foreach {
                     case (key, tup @ (time, value)) =>
                        val (timeVerify, newValue) = if( newFrame < oldFrame || newFrame >= time ) { // need to verify next time point
                           val opt: Option[ (Long, Grapheme.Value) ] = p.scans.get( key ).flatMap( scan => {
                              scan.source.flatMap {
                                 case Scan.Link.Grapheme( peer ) =>
                                    peer.nearestEventAfter( newFrameP ).flatMap { ceilTime =>
                                       peer.valueAt( ceilTime ).map( ceilTime -> _ )
                                    }
                                 case _ => None
                              }
                           })
                           opt.getOrElse( Long.MaxValue -> value )
                        } else tup

                        if( timeVerify != time ) {
                           // remove old entry from gPrio
                           gPrio.get( time ).foreach { staleMap =>
                              staleMap.get( staleID ).foreach { scanMap =>
                                 val newScanMap = scanMap - key
                                 val newStaleMap = if( newScanMap.isEmpty ) {
                                    staleMap - staleID
                                 } else {
                                    staleMap + (staleID -> newScanMap)
                                 }
                                 if( newStaleMap.isEmpty ) {
                                    gPrio.remove( time )
                                 } else {
                                    gPrio.add( time -> newStaleMap )
                                 }
                              }
                           }
                           // check if there is a new entry
                           if( timeVerify != Long.MaxValue ) {
                              // ...yes... store the new entry
                              keyMap += key -> (timeVerify -> newValue)
                              val staleMap   = gPrio.get( timeVerify ).getOrElse( Map.empty )
                              val scanMap    = staleMap.getOrElse( staleID, Map.empty ) + (key -> newValue)
                              val newStaleMap= staleMap + (staleID -> scanMap)
                              gPrio.add( timeVerify, newStaleMap )

                           } else { // no event after newFrame
                              keyMap -= key
                           }
                        }
                  }

                  // - for the changed entries, collect those which overlap the current transport time, so that they
                  //   will go into the advancement message
                  ???

               }
            }

//            Transport.Proc.GraphemesChanged( m: Map[ String, Grapheme.Value ])
         }

         val nextProcTime = if( needsNewProcTime ) {
            g.nearestEventAfter( newFrameP ).getOrElse( Long.MaxValue )
         } else {
            oldInfo.nextProcTime
         }

         val nextGraphemeTime = if( needsNewGraphemeTime ) {
            ??? // gPrio.headOption.map( _._1 ).getOrElse( Long.MaxValue )
         } else {
            oldInfo.nextGraphemeTime
         }

         val newState: State = ???
         val newInfo = oldInfo.copy( cpuTime          = cpuTime.get( tx.peer ),
                                     frame            = newFrame,
                                     state            = newState,
                                     nextProcTime     = nextProcTime,
                                     nextGraphemeTime = nextGraphemeTime )
         infoVar.set( newInfo )

//         if( procAdded.nonEmpty || procRemoved.nonEmpty || procUpdated ) {
//            val upd = Transport.Advanced( this, playing?, newFrame, procAdded, procRemoved, procUpdated )
//            Changed( upd )
            ???
//         }
      }

      private def scheduleNext()( implicit tx: S#Tx ) {
         implicit val itx  = tx.inMemory
         val info          = infoVar.get
         val targetFrame   = info.nextTime

         if( !info.isRunning || targetFrame == Long.MaxValue ) return

         val logicalDelay  = ((targetFrame - info.frame) * microsPerSample).toLong
         val logicalNow    = info.cpuTime
         val schedValid    = info.valid
         val jitter        = sysMicros() - logicalNow
         val actualDelay   = math.max( 0L, logicalDelay - jitter )
if( VERBOSE ) println( "::: scheduled: logicalDelay = " + logicalDelay + ", actualDelay = " + actualDelay + ", targetFrame = " + targetFrame )
         Txn.afterCommit( _ => {
            pool.schedule( new Runnable {
               def run() {
                  cursor.step { implicit tx =>
                     eventReached( schedValid, logicalNow + logicalDelay )
                  }
               }
            }, actualDelay, TimeUnit.MICROSECONDS )
         })( tx.peer )
      }

      private def eventReached( expectedValid: Int, newLogical: Long )( implicit tx: S#Tx ) {
         implicit val itx = tx.inMemory
         val info = infoVar.get
         if( info.valid != expectedValid ) return  // the scheduled task was invalidated by an intermediate stop or seek command

         // this is crucial to eliminate drift: since we reached the scheduled event, do not
         // let the cpuTime txn-local determine a new free wheeling time, but set it to the
         // time we targetted at; then in the next scheduleNext call, the jitter is properly
         // calculated.
         cpuTime.set( newLogical )( tx.peer )
         advance( isSeek = false, newFrame = info.nextTime )
         scheduleNext()
      }

//      def add( span: Span.HasStart, timed: TimedProc[ S ])( implicit tx: S#Tx ) {
//         ???
////         implicit val itx: I#Tx = ???
////         val w = waitSpan.get
////         if( w.touches( span )) {
////            // interrupt scheduled task
////
//////            val p = timed.value
//////            p.scans.iterator.foreach {
//////               case (key, scan) => scan.source match {
//////                  case Some( Scan.Link.Grapheme( grapheme )) =>
//////                     grapheme.nearestEventAfter( span.start )   // XXX
//////                  case _ =>
//////               }
//////            }
////         } else {
////            // see if it appears earlier than the next region
////         }
//      }
   }
}
