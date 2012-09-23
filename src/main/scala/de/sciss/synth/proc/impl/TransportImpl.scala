package de.sciss.synth
package proc
package impl

import de.sciss.lucre.{event => evt, DataOutput, DataInput, stm, bitemp, expr, data}
import stm.{Disposable, IdentifierMap, Sys, Cursor}
import bitemp.{SpanLike, Span}
import data.SkipList
import expr.Expr
import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import concurrent.stm.{Txn, TxnLocal}
import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}
import de.sciss.synth.expr.Booleans

object TransportImpl {
   var VERBOSE = true

   def apply[ S <: Sys[ S ]]( group: ProcGroup[ S ], sampleRate: Double )( implicit tx: S#Tx, cursor: Cursor[ S ]) : ProcTransport[ S ] = {
//      val targets    = evt.Targets[ S ]   // XXX TODO: partial?
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
      implicit val obsSer = dummySerializer[ IIdxSeq[ Observation[ S ]]]
      val obsVar     = itx.newVar( iid, IIdxSeq.empty[ Observation[ S ]])
      val view       = new Impl[ S ]( /* targets, */ group, sampleRate, playingVar, infoVar,
                                      gMap, gPrio, timedMap, obsVar, cursor.position )
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

      def isRunning  = state == Playing
      def nextTime   = math.min( nextProcTime, nextGraphemeTime )
   }

   private sealed trait State
   private case object Stopped extends State
   private case object Playing extends State

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

//   private def flatSpans[ S <: Sys[ S ]]( in: (SpanLike, IIdxSeq[ TimedProc[ S ]])) : IIdxSeq[ TimedProc[ S ]] = in._2

   private val anyEmptySeq = IIdxSeq.empty[ Nothing ]
   @inline private def emptySeq[ A ] = anyEmptySeq.asInstanceOf[ IIdxSeq[ A ]]

   private def dummySerializer[ A ] : stm.Serializer[ I#Tx, I#Acc, A ] =
      DummySerializer.asInstanceOf[ stm.Serializer[ I#Tx, I#Acc, A ]]

   private object DummySerializer extends stm.Serializer[ I#Tx, I#Acc, Nothing ] {
      def write( v: Nothing, out: DataOutput) {}
      def read( in: DataInput, access: I#Acc )( implicit tx: I#Tx ) : Nothing = sys.error( "Operation not supported" )
   }

   private type Update[ S <: Sys[ S ]] = Transport.Update[ S, Proc[ S ], Transport.Proc.Update[ S ]]

   private final class Observation[ S <: Sys[ S ]]( impl: Impl[ S ], fun: S#Tx => Update[ S ] => Unit )
   extends Disposable[ S#Tx ] {
      override def toString = impl.toString + ".react@" + hashCode().toHexString

      def dispose()( implicit tx: S#Tx ) {
         impl.removeObservation( this )
      }
   }

   private final class Impl[ S <: Sys[ S ]]( /* protected val targets: evt.Targets[ S ], */
                                     groupStale:            ProcGroup[ S ],
                                     val sampleRate:        Double,
                                     playingVar:            Expr.Var[ S, Boolean ],
                                     infoVar:               I#Var[ Info ],
                                     gMap:                  IdentifierMap[ S#ID, S#Tx, (S#ID, Map[ String, (Long, Grapheme.Value) ])],
                                     gPrio:                 SkipList.Map[ I, Long, Map[ S#ID, Map[ String, Grapheme.Value ]]],
                                     timedMap:              IdentifierMap[ S#ID, S#Tx, TimedProc[ S ]],
                                     obsVar:                I#Var[ IIdxSeq[ Observation[ S ]]],
                                     csrPos:                S#Acc )
                                   ( implicit cursor: Cursor[ S ])
   extends Transport[ S, Proc[ S ], Transport.Proc.Update[ S ]] /* with evt.Node[ S ] */ {
      impl =>

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

      def dispose()( implicit tx: S#Tx ) {
         disposeData()
      }

      // ---- evt.Node ----

      protected def disposeData()( implicit tx: S#Tx ) {
         implicit val itx = tx.inMemory
         playingVar.dispose()
         infoVar.set( Info.init )   // if there is pending scheduled tasks, they should abort gracefully
//         infoVar.dispose()
         gMap.dispose()
         gPrio.dispose()
         timedMap.dispose()
      }

//      protected def writeData( out: DataOutput ) {}   // there is no serialization for this class
//
//      def select( slot: Int, invariant: Boolean ) : Event[ S, Any, Any ] = {
//         require( slot == 1 )
//         ChangeEvent
//      }
//
//      def changed: Event[ S, Transport.Update[ S, Proc[ S ], Transport.Proc.Update[ S ]], ProcTransport[ S ]] =
//         ChangeEvent

      def iterator( implicit tx: S#Tx ) : data.Iterator[ S#Tx, (SpanLike, TimedProc[ S ])] =
         group.intersect( time ).flatMap( flatSpans )

      private def calcCurrentTime( info: Info )( implicit tx: S#Tx ) : Long = {
         val startFrame = info.frame
         if( info.isRunning ) {
            val logicalNow    = cpuTime.get( tx.peer )
            val logicalDelay  = logicalNow - info.cpuTime
            val stopFrame     = info.nextTime
            math.min( stopFrame - 1, startFrame + (logicalDelay / microsPerSample).toLong )
         } else startFrame
      }

      def time( implicit tx: S#Tx ) : Long = calcCurrentTime( infoVar.get( tx.inMemory ))

      def playing( implicit tx: S#Tx ) : Expr[ S, Boolean ] = playingVar.get
      def playing_=( expr: Expr[ S, Boolean ])( implicit tx: S#Tx ) { playingVar.set( expr )}

      private def play()( implicit tx: S#Tx ) {
         implicit val itx = tx.inMemory
         val oldInfo = infoVar.get
         if( oldInfo.isRunning ) return
         val newInfo = oldInfo.copy( cpuTime = cpuTime.get( tx.peer ),
                                     state   = Playing )
         infoVar.set( newInfo )
         fire( Transport.Play( impl, newInfo.frame ))
         scheduleNext( newInfo )
      }

      private def stop()( implicit tx: S#Tx ) {
         implicit val itx = tx.inMemory
         val oldInfo = infoVar.get
         if( !oldInfo.isRunning ) return

         val newInfo = oldInfo.copy( cpuTime = cpuTime.get( tx.peer ),
                                     frame   = calcCurrentTime( oldInfo ),
                                     state   = Stopped )
         infoVar.set( newInfo )
         fire( Transport.Stop( impl, newInfo.frame ))
      }

      def group( implicit tx: S#Tx ) : ProcGroup[ S ] =  tx.refresh( csrPos, groupStale )

      def isPlaying( implicit tx: S#Tx ) : Boolean = infoVar.get( tx.inMemory ).isRunning // playingVar.value

      def seek( time: Long )( implicit tx: S#Tx ) {
         advance( isSeek = true, startPlay = false, newFrame = time )
      }

      private def fire( update: Update[ S ])( implicit tx: S#Tx ) {
         ???
      }

      def removeObservation( obs: Observation[ S ])( implicit tx: S#Tx ) {
         obsVar.transform( _.filterNot( _ == obs ))( tx.inMemory )
      }

      def react( fun: Update[ S ] => Unit )( implicit tx: S#Tx ) : Disposable[ S#Tx ] =
         reactTx( _ => fun )

      def reactTx( fun: S#Tx => Update[ S ] => Unit )( implicit tx: S#Tx ) : Disposable[ S#Tx ] = {
         val obs = new Observation( this, fun )
         obsVar.transform( _ :+ obs )( tx.inMemory )
         obs
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
      private def advance( isSeek: Boolean, startPlay: Boolean, newFrame: Long )( implicit tx: S#Tx ) {
         implicit val itx     = tx.inMemory
         val oldInfo          = infoVar.get
         val oldFrame         = oldInfo.frame
if( VERBOSE ) println( "::: advance(isSeek = " + isSeek + "; newFrame = " + newFrame + "); oldInfo = " + oldInfo )
         if( newFrame == oldFrame ) return

         val g                = group
         val needsNewProcTime = newFrame < oldFrame || newFrame >= oldInfo.nextProcTime
         val newFrameP        = newFrame + 1

         var procAdded        = emptySeq[  TimedProc[ S ]]
         var procRemoved      = emptySeq[  TimedProc[ S ]]
         var procUpdated      = emptySeq[ (TimedProc[ S ], Transport.Proc.Update[ S ])]

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
            procRemoved = itRemoved.flatMap( _._2 ).toIndexedSeq
            procRemoved.foreach { timed =>
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
            procAdded = itAdded.flatMap( _._2 ).toIndexedSeq
            procAdded.foreach { timed =>
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
            val updMap: IIdxSeq[ (TimedProc[ S ], Map[ String, Grapheme.Value ])] = if( newFrame == oldInfo.nextGraphemeTime ) {
               // we went exactly till a known event spot

               // - in (2) find and remove the map for the given time frame
               // - for each scan (S#ID, String) collect the grapheme values so that they be dispatched
               //   in the advancement message
               // - and for each of these scans, look up the timed proc through (3) and gather the new next grapheme
               //   values, store (replace) them in (1) and (2), and calculate the new nextGraphemeTime.

               val scanMap: IIdxSeq[ (TimedProc[ S ], Map[ String, Grapheme.Value ])] = gPrio.remove( newFrame ) match {
                  case Some( staleMap ) => staleMap.flatMap({
                        case (staleID, keyMap) =>
                           timedMap.get( staleID ).map( _ -> keyMap )

                        case _ => None
                     })( breakOut )
                  case _ => IIdxSeq.empty // Map.empty
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
               scanMap

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

               val newProcs: Set[ TimedProc[ S ]] = procAdded.toSet // .map( _._2 )( breakOut )

               // filter because new procs have already build up their scan maps
               val oldProcs = g.intersect( newFrame ).flatMap( _._2.filterNot( newProcs.contains ))
               val itMap = oldProcs.map { timed =>
                  val id   = timed.id
                  val p    = timed.value
                  var (staleID, keyMap) = gMap.get( id ).getOrElse( id -> Map.empty[ String, (Long, Grapheme.Value) ])

                  var scanMap = Map.empty[ String, Grapheme.Value ]

                  // check again all scan's which are linked to a grapheme source,
                  // because we may have new entries for which no data existed before
                  p.scans.iterator.foreach {
                     case (key, scan) =>
                        scan.source match {
                           case Some( Scan.Link.Grapheme( peer )) =>

                              def addNewEntry( time: Long, value: Grapheme.Value ) {
                                 keyMap += key -> (time -> value)
                                 val staleMap      = gPrio.get( time ).getOrElse( Map.empty )
                                 val m             = staleMap.getOrElse( staleID, Map.empty ) + (key -> value)
                                 val newStaleMap   = staleMap + (staleID -> m)
                                 gPrio.add( time, newStaleMap )
                              }

                              // deal with scanMap (fired update)
                              // - for the changed entries, collect those which overlap the current
                              //   transport time, so that they will go into the advancement message
                              def addToScanMap( maybeNowValue: Option[ Grapheme.Value ]) {
                                 maybeNowValue match {
                                    case Some( nextValue ) if nextValue.span.contains( newFrame ) =>
                                       // next value is valid also for current time, so re-use it
                                       scanMap += key -> nextValue

                                    case _ =>
                                       // either no value, or ceilTime is larger than now, need to find value explicitly
                                       peer.valueAt( newFrame ).foreach { nowValue =>
                                          // only add it if it did not cover the previous transport position
                                          // (because if it did, it would not constitute a change)
                                          if( !nowValue.span.contains( oldFrame )) {
                                             scanMap += key -> nowValue
                                          }
                                       }
                                 }
                              }

                              keyMap.get( key ) match {
                                 // first case: there was an entry in the previous info
                                 case Some( tup @ (time, value) ) =>
                                    val (newTime, newValue) = if( newFrame < oldFrame || newFrame >= time ) { // need to verify next time point
                                       val opt = peer.nearestEventAfter( newFrameP ).flatMap { ceilTime =>
                                          peer.valueAt( ceilTime ).map( ceilTime -> _ )
                                       }
                                       opt.getOrElse( Long.MaxValue -> value )
                                    } else tup

                                    if( newTime != time ) {
                                       // remove old entry from gPrio
                                       gPrio.get( time ).foreach { staleMap =>
                                          staleMap.get( staleID ).foreach { mOld =>
                                             val m = mOld - key
                                             val newStaleMap = if( m.isEmpty ) {
                                                staleMap - staleID
                                             } else {
                                                staleMap + (staleID -> m)
                                             }
                                             if( newStaleMap.isEmpty ) {
                                                gPrio.remove( time )
                                             } else {
                                                gPrio.add( time -> newStaleMap )
                                             }
                                          }
                                       }
                                       // check if there is a new entry
                                       if( newTime != Long.MaxValue ) {
                                          // ...yes... store the new entry
                                          addNewEntry( newTime, newValue )
                                          if( !newValue.span.contains( oldFrame )) addToScanMap( Some( newValue ))

                                       } else { // no event after newFrame
                                          keyMap -= key
                                          addToScanMap( None )
                                       }
                                    } else {
                                       // no change in next value (because the next time did not change);
                                       // however if we went backwards in time, the current value might
                                       // have changed!
                                       if( newFrame < oldFrame ) addToScanMap( None ) // None triggers explicit search
                                    }

                                 // second case: there was not entry in the previous info.
                                 // if we advanced in time, there might be new data.
                                 // if we went back in time, it means there can't be any
                                 // previous data
                                 case _ =>
                                    if( newFrame > oldFrame ) {
                                       val maybeNowValue = peer.nearestEventAfter( newFrameP ).flatMap { ceilTime =>
                                          peer.valueAt( ceilTime ).map { ceilValue =>
                                             // a new entry
                                             addNewEntry( ceilTime, ceilValue )
                                             ceilValue
                                          }
                                       }
                                       addToScanMap( maybeNowValue )
                                    }
                              }

                           case _ =>
                        }
                  }
                  timed -> scanMap
               }
               itMap.toIndexedSeq
            }

            procUpdated = updMap.map { case (timed, map) => timed -> Transport.Proc.GraphemesChanged( map )}
         }

         val nextProcTime = if( needsNewProcTime ) {
            g.nearestEventAfter( newFrameP ).getOrElse( Long.MaxValue )
         } else {
            oldInfo.nextProcTime
         }

         val nextGraphemeTime = if( needsNewGraphemeTime ) {
            val headOption = gPrio.ceil( Long.MinValue ) // headOption method missing
            headOption.map( _._1 ).getOrElse( Long.MaxValue )
         } else {
            oldInfo.nextGraphemeTime
         }

         val newState: State = if( startPlay ) Playing else oldInfo.state
         val newInfo = oldInfo.copy( cpuTime          = cpuTime.get( tx.peer ),
                                     frame            = newFrame,
                                     state            = newState,
                                     nextProcTime     = nextProcTime,
                                     nextGraphemeTime = nextGraphemeTime )
         infoVar.set( newInfo )

         if( procAdded.nonEmpty || procRemoved.nonEmpty || procUpdated.nonEmpty ) {
            val upd = Transport.Advance( transport = impl, time = newFrame,
                                         isSeek = isSeek, isPlaying = newInfo.isRunning,
                                         added = procAdded, removed = procRemoved, changes = procUpdated )
            fire( upd )
         }

         if( newState == Playing ) scheduleNext( newInfo )
      }

      private def scheduleNext( info: Info )( implicit tx: S#Tx ) {
//         implicit val itx  = tx.inMemory
//         val info          = infoVar.get
         val targetFrame   = info.nextTime

         if( /* !info.isRunning || */ targetFrame == Long.MaxValue ) return

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
         advance( isSeek = false, startPlay = false, newFrame = info.nextTime )
      }

//      def add( span: Span.HasStart, timed: TimedProc[ S ])( implicit tx: S#Tx ) {
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
