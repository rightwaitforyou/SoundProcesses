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

package de.sciss.synth
package proc
package impl

import de.sciss.lucre.{DataOutput, DataInput, stm, bitemp, data, event => evt}
import stm.{Disposable, IdentifierMap, Cursor}
import evt.Sys
import bitemp.{BiGroup, SpanLike, Span}
import data.SkipList
import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import concurrent.stm.{Ref, Txn, TxnLocal}
import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}
import java.text.SimpleDateFormat

object TransportImpl {
   var VERBOSE = false
   
   import Grapheme.Segment
   import Segment.{Defined => DefSeg}

   def apply[ S <: Sys[ S ], I <: stm.Sys[ I ]]( group: ProcGroup[ S ], sampleRate: Double )(
      implicit tx: S#Tx, cursor: Cursor[ S ], bridge: S#Tx => I#Tx ) : ProcTransport[ S ] = {

      val (groupH, infoVar, gMap, gPrio, timedMap, obsVar) = prepare[ S, I ]( group )
      val t = new Realtime[ S, I ]( groupH, sampleRate, infoVar, gMap, gPrio, timedMap, obsVar )
      t.init()
      t
   }

   def offline[ S <: Sys[ S ], I <: stm.Sys[ I ]]( group: ProcGroup[ S ], sampleRate: Double )(
      implicit tx: S#Tx, bridge: S#Tx => I#Tx ) : Transport.Offline[ S, Proc[ S ], Transport.Proc.Update[ S ]] = {

      val (groupH, infoVar, gMap, gPrio, timedMap, obsVar) = prepare[ S, I ]( group )
      val t = new Offline[ S, I ]( groupH, sampleRate, infoVar, gMap, gPrio, timedMap, obsVar )
      t.init()
      t
   }

   private def prepare[ S <: Sys[ S ], I <: stm.Sys[ I ]]( group: ProcGroup[ S ])
                                                         ( implicit tx: S#Tx, bridge: S#Tx => I#Tx ) :
      (stm.Source[ S#Tx, ProcGroup[ S ]],
       I#Var[ Info ],
       IdentifierMap[ S#ID, S#Tx, (S#ID, Map[ String, DefSeg ])],
       SkipList.Map[ I, Long, Map[ S#ID, Map[ String, DefSeg ]]],
       IdentifierMap[ S#ID, S#Tx, TimedProc[ S ]],
       I#Var[ IIdxSeq[ Observation[ S, I ]]]) = {

      implicit val itx: I#Tx = tx
      val iid        = itx.newID()
      implicit val infoSer = dummySerializer[ Info, I ]
      val infoVar    = itx.newVar( iid, Info.init ) // ( dummySerializer )
      val gMap       = tx.newInMemoryIDMap[ (S#ID, Map[ String, DefSeg ])]             // (1)
      implicit val skipSer = dummySerializer[ Map[ S#ID, Map[ String, DefSeg ]], I ]
      val gPrio      = SkipList.Map.empty[ I, Long, Map[ S#ID, Map[ String, DefSeg ]]] // (2)
      val timedMap   = tx.newInMemoryIDMap[ TimedProc[ S ]]                            // (3)
      implicit val obsSer = dummySerializer[ IIdxSeq[ Observation[ S, I ]], I ]
      val obsVar     = itx.newVar( iid, IIdxSeq.empty[ Observation[ S, I ]])
      val groupH     = tx.newHandle( group )( ProcGroup_.serializer )

      (groupH, infoVar, gMap, gPrio, timedMap, obsVar)
   }

   private def sysMicros() = System.nanoTime()/1000

   private lazy val df = new SimpleDateFormat( "HH:mm:ss.SSS" )

   private object Info {
      // the initial info is at minimum possible frame. that way, calling seek(0L) which initialise
      // the structures properly
      final val init: Info = apply( cpuTime = 0L, frame = Long.MinValue, state = Stopped,
                                    nextProcTime     = Long.MinValue + 1,
                                    nextGraphemeTime = Long.MinValue + 1, valid = -1 )
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

      private def smartLong( n: Long ) : String = n match {
         case Long.MinValue => "-inf"
         case Long.MaxValue => "inf"
         case _ => n.toString
      }

      private def smartMicros( n: Long ) : String = {
         val msDelta = (n - sysMicros()) / 1000
         val d = new java.util.Date( System.currentTimeMillis() + msDelta )
         df.format( d )
      }

      override def toString = "Info(cpuTime = " + smartMicros( cpuTime ) + "; frame = " + smartLong( frame ) + ", state = " + state +
         ", nextProcTime = " + smartLong( nextProcTime ) + ", nextGraphemeTime = " + smartLong( nextGraphemeTime ) +
         ", valid = " + valid + ")"
   }

   private sealed trait State
   private case object Stopped extends State
   private case object Playing extends State

   private lazy val pool : ScheduledExecutorService = {        // system wide scheduler
      val res = Executors.newScheduledThreadPool( 1 )
      sys.addShutdownHook( shutdownScheduler() )
      res
   }

   private def shutdownScheduler() {
     if( VERBOSE )println( "Shutting down scheduler thread pool" )
     pool.shutdown()
   }

   private def flatSpans[ S <: Sys[ S ]]( in: (SpanLike, IIdxSeq[ TimedProc[ S ]])) : IIdxSeq[ (SpanLike, TimedProc[ S ])] = {
      val span = in._1
      in._2.map { span -> _ }
   }

//   private def flatSpans[ S <: Sys[ S ]]( in: (SpanLike, IIdxSeq[ TimedProc[ S ]])) : IIdxSeq[ TimedProc[ S ]] = in._2

//   private val anyEmptySeq = IIdxSeq.empty[ Nothing ]
//   @inline private def emptySeq[ A ] = anyEmptySeq.asInstanceOf[ IIdxSeq[ A ]]

   private val emptySeq = IIdxSeq.empty[ Nothing ]

   private def dummySerializer[ A, I <: stm.Sys[ I ]] : stm.Serializer[ I#Tx, I#Acc, A ] =
      DummySerializer.asInstanceOf[ stm.Serializer[ I#Tx, I#Acc, A ]]

   private object DummySerializer extends stm.Serializer[ stm.InMemory#Tx, stm.InMemory#Acc, Nothing ] {
      def write( v: Nothing, out: DataOutput) {}
      def read( in: DataInput, access: stm.InMemory#Acc )( implicit tx: stm.InMemory#Tx ) : Nothing = sys.error( "Operation not supported" )
   }

   private type Update[ S <: Sys[ S ]] = Transport.Update[ S, Proc[ S ], Transport.Proc.Update[ S ]]

   private final class Observation[ S <: Sys[ S ], I <: stm.Sys[ I ]]( impl: Impl[ S, I ], val fun: S#Tx => Update[ S ] => Unit )
   extends Disposable[ S#Tx ] {
      override def toString = impl.toString + ".react@" + hashCode().toHexString

      def dispose()( implicit tx: S#Tx ) {
         impl.removeObservation( this )
      }
   }

   private final val dummySegment = Segment.Undefined( Span.from( Long.MaxValue ))

   private final class Offline[ S <: Sys[ S ], I <: stm.Sys[ I ]](
                      protected val groupHandle:   stm.Source[ S#Tx, ProcGroup[ S ]],
                      val sampleRate:              Double,
                      protected val infoVar:       I#Var[ Info ],
                      protected val gMap:          IdentifierMap[ S#ID, S#Tx, (S#ID, Map[ String, DefSeg ])],
                      protected val gPrio:         SkipList.Map[ I, Long, Map[ S#ID, Map[ String, DefSeg ]]],
                      protected val timedMap:      IdentifierMap[ S#ID, S#Tx, TimedProc[ S ]],
                      protected val obsVar:        I#Var[ IIdxSeq[ Observation[ S, I ]]])
                    ( implicit protected val trans: S#Tx => I#Tx )
   extends Impl[ S, I ] with Transport.Offline[ S, Proc[ S ], Transport.Proc.Update[ S ]] {
      private val submitRef   = Ref( (0L, 0L, -1) )
      private val timeRef     = Ref( 0L )

      protected def logicalTime()( implicit tx: S#Tx ) : Long = timeRef.get( tx.peer )
      protected def logicalTime_=( value: Long )( implicit tx: S#Tx ) {
         timeRef.set( value )( tx.peer )
      }

      protected def submit( logicalNow: Long, logicalDelay: Long, schedValid: Int )( implicit tx: S#Tx ) {
if( VERBOSE ) println( "::: scheduled: logicalDelay = " + logicalDelay ) // + ", targetFrame = " + targetFrame )
         submitRef.set( (logicalNow, logicalDelay, schedValid) )( tx.peer )
      }

      def step()( implicit tx: S#Tx ) {
         val (logicalNow, logicalDelay, schedValid) = submitRef.swap( (0L, 0L, -1) )( tx.peer )
         if( schedValid >= 0 ) {
            eventReached( logicalNow = logicalNow, logicalDelay = logicalDelay, expectedValid = schedValid )
         }
      }

      def elapse( seconds: Double )( implicit tx: S#Tx ) {
         val micros = (seconds * 1e6).toLong
         timeRef.transform( _ + micros )( tx.peer )
      }
   }

   private final val rt_cpuTime = TxnLocal( sysMicros() )    // system wide wall clock in microseconds

   private final class Realtime[ S <: Sys[ S ], I <: stm.Sys[ I ]](
                      protected val groupHandle:   stm.Source[ S#Tx, ProcGroup[ S ]],
                      val sampleRate:              Double,
                      protected val infoVar:       I#Var[ Info ],
                      protected val gMap:          IdentifierMap[ S#ID, S#Tx, (S#ID, Map[ String, DefSeg ])],
                      protected val gPrio:         SkipList.Map[ I, Long, Map[ S#ID, Map[ String, DefSeg ]]],
                      protected val timedMap:      IdentifierMap[ S#ID, S#Tx, TimedProc[ S ]],
                      protected val obsVar:        I#Var[ IIdxSeq[ Observation[ S, I ]]])
                    ( implicit cursor: Cursor[ S ], protected val trans: S#Tx => I#Tx )
   extends Impl[ S, I ] {

      protected def logicalTime()( implicit tx: S#Tx ) : Long = rt_cpuTime.get( tx.peer )
      protected def logicalTime_=( value: Long )( implicit tx: S#Tx ) {
         rt_cpuTime.set( value )( tx.peer )
      }

      protected def submit( logicalNow: Long, logicalDelay: Long, schedValid: Int )( implicit tx: S#Tx ) {
         val jitter        = sysMicros() - logicalNow
         val actualDelay   = math.max( 0L, logicalDelay - jitter )
if( VERBOSE ) println( "::: scheduled: logicalDelay = " + logicalDelay + ", actualDelay = " + actualDelay ) // + ", targetFrame = " + targetFrame )
         Txn.afterCommit( _ => {
            pool.schedule( new Runnable {
               def run() {
                  cursor.step { implicit tx =>
                     eventReached( logicalNow = logicalNow, logicalDelay = logicalDelay, expectedValid = schedValid )
                  }
               }
            }, actualDelay, TimeUnit.MICROSECONDS )
         })( tx.peer )
      }
   }

   private sealed trait Impl[ S <: Sys[ S ], I <: stm.Sys[ I ]]
   extends Transport[ S, Proc[ S ], Transport.Proc.Update[ S ]] {
      impl =>

      private implicit final val procGroupSer      = ProcGroup_.serializer[ S ]
      private final val          microsPerSample   = 1000000 / sampleRate

      // the three structures maintained for the update algorithm
      // (1) for each observed timed proc, store information about all scans whose source is a grapheme.
      //     an observed proc is one whose time span overlaps the current span in the transport info.
      //     the value found in the map is a tuple. the tuple's first element is the _stale_ ID which
      //     corresponds to the ID when the underlying grapheme value was stored in structure (2), thereby
      //     allowing to find that value in (2). the tuple's second element is a map from the scan keys
      //     to a grapheme segment. the segment span's start time value is the value at
      //     which the grapheme value was stored in (2)
      // (2) a skiplist is used as priority queue for the next interesting point in time at which the
      //     transport needs to emit an advancement message. the value is a map from stale timed-proc ID's
      //     to a map from scan keys to grapheme values, i.e. for scans whose source is a grapheme.
      // (3) a refreshment map for the timed procs
      protected def gMap:        IdentifierMap[ S#ID, S#Tx, (S#ID, Map[ String, DefSeg ])]   // (1)
      protected def gPrio:       SkipList.Map[ I, Long, Map[ S#ID, Map[ String, DefSeg ]]]   // (2)
      protected def timedMap:    IdentifierMap[ S#ID, S#Tx, TimedProc[ S ]]                  // (3)

      protected def groupHandle: stm.Source[ S#Tx, ProcGroup[ S ]]
      protected def infoVar:     I#Var[ Info ]
      protected def obsVar:      I#Var[ IIdxSeq[ Observation[ S, I ]]]

      protected implicit def trans: S#Tx => I#Tx

      private final var groupObs : Disposable[ S#Tx ] = _

      /**
       * Invoked to submit a schedule step either to a realtime scheduler or other mechanism.
       * When the step is performed, execution should be handed over to `eventReached`, passing
       * over the same three arguments.
       *
       * @param logicalNow       the logical now time at the time the event was scheduled
       * @param logicalDelay     the logical delay corresponding with the delay of the scheduled event
       * @param schedValid       the valid counter at the time of scheduling
       */
      protected def submit( logicalNow: Long, logicalDelay: Long, schedValid: Int )( implicit tx: S#Tx ) : Unit

      /**
       * Invoked from the `submit` body after the scheduled event is reached.
       *
       * @param logicalNow       the logical now time at the time the event was scheduled
       * @param logicalDelay     the logical delay corresponding with the delay of the scheduled event
       * @param expectedValid    the valid counter at the time of scheduling
       */
      final protected def eventReached( logicalNow: Long, logicalDelay: Long, expectedValid: Int )( implicit tx: S#Tx ) {
         implicit val itx: I#Tx = tx
         val info = infoVar.get
         if( info.valid != expectedValid ) return  // the scheduled task was invalidated by an intermediate stop or seek command

         // this is crucial to eliminate drift: since we reached the scheduled event, do not
         // let the cpuTime txn-local determine a new free wheeling time, but set it to the
         // time we targetted at; then in the next scheduleNext call, the jitter is properly
         // calculated.
         val newLogical = logicalNow + logicalDelay
         logicalTime_=( newLogical )
         advance( newFrame = info.nextTime )
      }

      protected def logicalTime()( implicit tx: S#Tx ) : Long
      protected def logicalTime_=( value: Long )( implicit tx: S#Tx ) : Unit

      // the following illustrates what actions need to be done with respect
      // to the overlap/touch or not between the current info span and the change span
      // (the `v` indicates the result of calcCurrentTime; ! is a non-important point, wheras | is an important point)
      //
      // info:              |....v.......|
      // proc: (a)    !........| --> proc ends before `v` = NO ACTION
      //       (b)            !,,,,,,| --> proc contains `v` = ADD/REMOVE and calc NEW NEXT
      //       (c)                 |.........! --> proc starts after info began but before info ends = JUST NEW NEXT
      //       (d)                       |.......! --> proc starts no earlier than info ends = NO ACTION
      //
      // (Note that case (a) can only be addition; removal would mean that the stop time
      //  existed after info.frame but before info.nextTime which is in contraction to
      //  the definition of info.nextTime)
      //
      // So in short: - (1) if update span contains `v`, perform add/remove and recalc new next
      //              - (2) if info span contains update span's start, recalc new next
      private def addRemoveProcs( g: ProcGroup[ S ], span: SpanLike, procAdded: IIdxSeq[ TimedProc[ S ]],
                                  procRemoved: IIdxSeq[ TimedProc[ S ]])( implicit tx: S#Tx ) {
         implicit val itx: I#Tx = tx
         val oldInfo    = infoVar.get
         val newFrame   = calcCurrentTime( oldInfo )
         val isPly      = oldInfo.isRunning

         val perceived        = span.contains( newFrame )   // (1)
         val needsNewProcTime = perceived || (span match {  // (2)
            case hs: Span.HasStart => oldInfo.frame <= hs.start && oldInfo.nextProcTime > hs.start
            case _ => false
         })

         if( !(perceived || needsNewProcTime) ) return   // keep scheduled task running, don't overwrite infoVar

         if( perceived ) {
            procRemoved.foreach { timed => removeProc( timed )}
            procAdded.foreach   { timed => addProc( newFrame, timed )}
         }

         val nextProcTime = if( needsNewProcTime ) {
            g.nearestEventAfter( newFrame + 1 ).getOrElse( Long.MaxValue )
         } else {
            oldInfo.nextProcTime
         }
         val nextGraphemeTime = if( perceived ) {
            val headOption = gPrio.ceil( Long.MinValue ) // headOption method missing
            headOption.map( _._1 ).getOrElse( Long.MaxValue )
         } else {
            oldInfo.nextGraphemeTime
         }
         val newInfo = oldInfo.copy( cpuTime          = logicalTime(),
                                     frame            = newFrame,
                                     nextProcTime     = nextProcTime,
                                     nextGraphemeTime = nextGraphemeTime )
         infoVar.set( newInfo )

         if( perceived ) {
            fire( Transport.Advance( transport = this, time = newFrame, isSeek = false, isPlaying = isPly,
                                     added = procAdded, removed = procRemoved, changes = emptySeq ))
         }

         if( isPly ) scheduleNext( newInfo )
      }

      private def procMoved( g: ProcGroup[ S ], timed: TimedProc[ S ], oldSpan: SpanLike, newSpan: SpanLike )( implicit tx: S#Tx ) {
         // ... possible situations
         //     (1) old span contains current time frame `v`, but new span not --> treat as removal
         //     (2) old span does not contain `v`, but new span does --> treat as addition
         //     (3) neither old nor new span contain `v`
         //         --> info.span contains start point of either span? then recalc nextProcTime.
         //         (indeed we could call addRemoveProcs with empty lists for the procs, and a
         //          second span argument, which would be just Span.Void in the normal add/remove calls)
         //     (4) both old and new span contain `v`
         //         --> remove map entries (gMap -> gPrio), and rebuild them, then calc new next times

         implicit val itx: I#Tx = tx
         val oldInfo    = infoVar.get
         val newFrame   = calcCurrentTime( oldInfo )

         val oldPercv   = oldSpan.contains( newFrame )
         val newPercv   = newSpan.contains( newFrame )

         (oldPercv, newPercv) match {
            case (true,  false)  => // case (1)
               addRemoveProcs( g, oldSpan, procAdded = emptySeq, procRemoved = IIdxSeq( timed ))
            case (false, true)   => // case (2)
               addRemoveProcs( g, newSpan, procAdded = IIdxSeq( timed ), procRemoved = emptySeq )
            case (false, false)  => // case (3)
               ???
            case (true,  true)   => // case (4)
               ???
         }
      }

      final def init()( implicit tx: S#Tx ) {
         // we can use groupStale because init is called straight away after instantiating Impl
         groupObs = group.changed.reactTx[ ProcGroup_.Update[ S ]] { implicit tx => {
            case BiGroup.Added( g, span, timed ) =>
               addRemoveProcs( g, span, procAdded = IIdxSeq( timed ), procRemoved = emptySeq )
            case BiGroup.Removed( g, span, timed ) =>
               addRemoveProcs( g, span, procAdded = emptySeq, procRemoved = IIdxSeq( timed ))

            case BiGroup.Element( g, changes ) =>
               // changes: IIdxSeq[ (TimedProc[ S ], BiGroup.ElementUpdate[ U ])]
               // ElementUpdate is either of Moved( change: evt.Change[ SpanLike ])
               //                         or Mutated[ U ]( change: U ) ; U = Proc.Update[ S ]
               // Mutated:
               // ... only process procs which are observed (found in timedMap)
               // ... first of all, the update is passed on as such. additional processing:
               // ... where each Proc.Update can be one of
               //     StateChange
               //        AssociativeChange : we need to track the addition and removal of scans.
               //                            filter only those AssociativeKeys which are ScanKeys.
               //                            track appearance or disappearence of graphemes as sources
               //                            of these scans, and update structures
               //        other StateChange : -
               //     ScanChange (carrying a Map[ String, Scan.Update[ S ]])
               //        ...where a Scan.Update is either of:
               //        SinkAdded, SinkRemoved : -
               //        SourceUpdate (passing on changes in a grapheme source) :
               //          - grapheme changes must then be tracked, structures updated
               //        SourceChanged : if it means a grapheme is connected or disconnect, update structures
               //     GraphemeChange : - (the grapheme is not interested, we only see them as sources of scans)
               //
               // Moved:
               // ... possible situations
               //     (1) old span contains current time frame `v`, but new span not --> treat as removal
               //     (2) old span does not contain `v`, but new span does --> treat as addition
               //     (3) neither old nor new span contain `v`
               //         --> info.span contains start point of either span? then recalc nextProcTime.
               //         (indeed we could call addRemoveProcs with empty lists for the procs, and a
               //          second span argument, which would be just Span.Void in the normal add/remove calls)
               //     (4) both old and new span contain `v`
               //         --> remove map entries (gMap -> gPrio), and rebuild them, then calc new next times

//               println( "WARNING: Transport observing BiGroup.Element not yet implemented" )

               changes.foreach {
                  case (timed, BiGroup.Mutated( procUpd )) => procUpd match {
                     case Proc.AssociativeChange( _, added, removed ) =>
                        ???
                     case Proc.ScanChange( _, scanChanges ) =>
                        scanChanges.foreach {
                           case (key, Scan.SourceUpdate( scan, graphUpd )) =>
                              ???
                           case (key, Scan.SourceChanged( scan, sourceOpt )) =>
                              ???
                           case _ =>   // ignore SinkAdded, SinkRemoved
                        }
                     case _ => // ignore StateChange other than AssociativeChange, and ignore GraphemeChange
                  }
                  case (timed, BiGroup.Moved( evt.Change( oldSpan, newSpan ))) =>
                     procMoved( g, timed, oldSpan, newSpan )
               }
         }}

         seek( 0L )
      }

      final def dispose()( implicit tx: S#Tx ) {
         implicit val itx: I#Tx = tx
         groupObs.dispose()
         infoVar.set( Info.init )   // if there is pending scheduled tasks, they should abort gracefully
//         infoVar.dispose()
         gMap.dispose()
         gPrio.dispose()
         timedMap.dispose()
         obsVar.dispose()
      }

      override def toString = "Transport(group=" + groupHandle + ")@" + hashCode.toHexString

      final def iterator( implicit tx: S#Tx ) : data.Iterator[ S#Tx, (SpanLike, TimedProc[ S ])] =
         group.intersect( time ).flatMap( flatSpans )

      private def calcCurrentTime( info: Info )( implicit tx: S#Tx ) : Long = {
         val startFrame = info.frame
         if( info.isRunning ) {
            val logicalNow    = logicalTime()
            val logicalDelay  = logicalNow - info.cpuTime
            val stopFrame     = info.nextTime
            // for this to work as expected it is crucial that the reported current time is
            // _less than_ the info's target time (<= stopFrame -1)
            math.min( stopFrame - 1, startFrame + (logicalDelay / microsPerSample).toLong )
         } else startFrame
      }

      final def time( implicit tx: S#Tx ) : Long = {
         implicit val itx: I#Tx = tx
         calcCurrentTime( infoVar.get )
      }

//      def playing( implicit tx: S#Tx ) : Expr[ S, Boolean ] = playingVar.get
//      def playing_=( expr: Expr[ S, Boolean ])( implicit tx: S#Tx ) { playingVar.set( expr )}

      final def play()( implicit tx: S#Tx ) {
         implicit val itx: I#Tx = tx
         val oldInfo = infoVar.get
         if( oldInfo.isRunning ) return
         val newInfo = oldInfo.copy( cpuTime = logicalTime(),
                                     state   = Playing )
         infoVar.set( newInfo )
         fire( Transport.Play( impl, newInfo.frame ))
         scheduleNext( newInfo )
      }

      final def stop()( implicit tx: S#Tx ) {
         implicit val itx: I#Tx = tx
         val oldInfo = infoVar.get
         if( !oldInfo.isRunning ) return

         val newInfo = oldInfo.copy( cpuTime = logicalTime(),
                                     frame   = calcCurrentTime( oldInfo ),
                                     state   = Stopped )
         infoVar.set( newInfo )
         fire( Transport.Stop( impl, newInfo.frame ))
      }

      final def seek( time: Long )( implicit tx: S#Tx ) {
         advance( isSeek = true, newFrame = time )
      }

      final def isPlaying( implicit tx: S#Tx ) : Boolean = {
         implicit val itx: I#Tx = tx
         infoVar.get.isRunning
      } // playingVar.value

      private def scheduleNext( info: Info )( implicit tx: S#Tx ) {
//         implicit val itx  = tx.inMemory
//         val info          = infoVar.get
         val targetFrame   = info.nextTime

         if( /* !info.isRunning || */ targetFrame == Long.MaxValue ) return

         val logicalDelay  = ((targetFrame - info.frame) * microsPerSample).toLong
         val logicalNow    = info.cpuTime
         val schedValid    = info.valid

         submit( logicalNow = logicalNow, logicalDelay = logicalDelay, schedValid = schedValid )
      }

      final def group( implicit tx: S#Tx ) : ProcGroup[ S ] = groupHandle.get // tx.refresh( csrPos, groupStale )

      private def fire( update: Update[ S ])( implicit tx: S#Tx ) {
         implicit val itx: I#Tx = tx
         val obs = obsVar.get
         obs.foreach( _.fun( tx )( update ))
      }

      final def removeObservation( obs: Observation[ S, I ])( implicit tx: S#Tx ) {
         implicit val itx: I#Tx = tx
         obsVar.transform( _.filterNot( _ == obs ))
      }

      final def react( fun: Update[ S ] => Unit )( implicit tx: S#Tx ) : Disposable[ S#Tx ] =
         reactTx( _ => fun )

      final def reactTx( fun: S#Tx => Update[ S ] => Unit )( implicit tx: S#Tx ) : Disposable[ S#Tx ] = {
         implicit val itx: I#Tx = tx
         val obs = new Observation[ S, I ]( this, fun )
         obsVar.transform( _ :+ obs )
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

      private def removeProc( timed: TimedProc[ S ])( implicit tx: S#Tx ) {
         implicit val itx: I#Tx = tx
         val id = timed.id
         timedMap.remove( id )                              // in (3)
         val entries = gMap.get( id )
         gMap.remove( id )                                  // in (1)
         entries.foreach {
            case (staleID, scanMap) =>
               scanMap.valuesIterator.foreach { segm =>
                  val time = segm.span.start
                  gPrio.get( time ).foreach { staleMap =>
                     val newStaleMap = staleMap - staleID
                     gPrio.add( time -> newStaleMap )       // in (2)
                  }
               }
         }
      }

      private def addProc( newFrame: Long, timed: TimedProc[ S ])( implicit tx: S#Tx ) {
         implicit val itx: I#Tx = tx
         val newFrameP  = newFrame + 1
         val id         = timed.id
         val p          = timed.value
         var scanMap    = Map.empty[ String, DefSeg ]
         var skipMap    = Map.empty[ Long, Map[ String, DefSeg ]]
         p.scans.iterator.foreach {
            case (key, scan) =>
               scan.source match {
                  case Some( link @ Scan.Link.Grapheme( peer )) =>
                     peer.nearestEventAfter( newFrameP ).foreach { ceilTime =>
                        peer.segment( ceilTime ).foreach { ceilSeg =>
                           scanMap   += key -> ceilSeg
                           assert( ceilSeg.span.start == ceilTime )
                           val newMap = skipMap.getOrElse( ceilTime, Map.empty ) + (key -> ceilSeg)
                           skipMap   += ceilTime -> newMap
                        }
                     }

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
      private def advance( newFrame: Long, isSeek: Boolean = false, startPlay: Boolean = false )( implicit tx: S#Tx ) {
         implicit val itx: I#Tx = tx
         val oldInfo          = infoVar.get
         val oldFrame         = oldInfo.frame
if( VERBOSE ) println( "::: advance(isSeek = " + isSeek + "; newFrame = " + newFrame + "); oldInfo = " + oldInfo )
         // do not short cut and return; because we may want to enforce play and call `scheduleNext`
//         if( newFrame == oldFrame ) return

         val g                = group
         val needsNewProcTime = newFrame < oldFrame || newFrame >= oldInfo.nextProcTime
         val newFrameP        = newFrame + 1

         var procAdded:   IIdxSeq[  TimedProc[ S ]] = emptySeq
         var procRemoved: IIdxSeq[  TimedProc[ S ]] = emptySeq
         var procUpdated: IIdxSeq[ (TimedProc[ S ], Transport.Proc.Update[ S ])] = emptySeq

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
            procRemoved.foreach( removeProc )

            // - for the added procs, find all sinks whose source connects to a grapheme. calculate the values
            //   of those graphemes at the current transport time. (NOT YET: this goes with the Transport.Update so that
            //   listeners do not need to perform that step themselves. Also this information is useful because it
            //   yields the ceil time ct); if no value is found at the current transport time, find the ceiling time ct
            //   explicitly; for ct, evaluate the grapheme value and store it in (1) and (2) accordingly.
            //   store procs in (3)
            procAdded = itAdded.flatMap( _._2 ).toIndexedSeq
            procAdded.foreach( timed => addProc( newFrame, timed ))
         }

         val needsNewGraphemeTime = newFrame < oldFrame || newFrame >= oldInfo.nextGraphemeTime

         // algorithm [C] or [D]
         if( needsNewGraphemeTime ) {
            // [C]
            val updMap: IIdxSeq[ (TimedProc[ S ], Map[ String, DefSeg ])] = if( newFrame == oldInfo.nextGraphemeTime ) {
               // we went exactly till a known event spot

               // - in (2) find and remove the map for the given time frame
               // - for each scan (S#ID, String) collect the grapheme values so that they be dispatched
               //   in the advancement message
               // - and for each of these scans, look up the timed proc through (3) and gather the new next grapheme
               //   values, store (replace) them in (1) and (2), and calculate the new nextGraphemeTime.

               val scanMap: IIdxSeq[ (TimedProc[ S ], Map[ String, DefSeg ])] = gPrio.remove( newFrame ) match {
                  case Some( staleMap ) => staleMap.flatMap({
                        case (staleID, keyMap) => timedMap.get( staleID ).map( _ -> keyMap )
//                        case _ => None
                     })( breakOut )
                  case _ => emptySeq // Map.empty
               }
               scanMap.foreach {
                  case (timed, removeKeyMap) =>
                     val id = timed.id
                     var (staleID, keyMap) = gMap.get( id ).getOrElse( id -> Map.empty[ String, DefSeg ])
                     removeKeyMap.keysIterator.foreach { key =>
                        val p = timed.value
                        val valueOption = p.scans.get( key ).flatMap { scan =>
                           scan.source.flatMap {
                              case Scan.Link.Grapheme( peer ) =>
                                 peer.nearestEventAfter( newFrameP ).flatMap { ceilTime =>
                                    peer.segment( ceilTime ) // .map( ceilTime -> _ )
                                 }
                              case _ => None
                           }
                        }
                        valueOption match {
                           case Some( segm ) =>
                              val time       = segm.span.start
                              keyMap        += key -> segm
                              val staleMap   = gPrio.get( time ).getOrElse( Map.empty )
                              val keyMap2    = staleMap.get( staleID ).getOrElse( Map.empty ) + (key -> segm)
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
                  var (staleID, keyMap) = gMap.get( id ).getOrElse( id -> Map.empty[ String, DefSeg ])

                  // we collect in here the scan grapheme source changes to fire as update
                  var scanMap = Map.empty[ String, DefSeg ]

                  // check again all scan's which are linked to a grapheme source,
                  // because we may have new entries for which no data existed before
                  p.scans.iterator.foreach {
                     case (key, scan) =>
                        scan.source match {
                           case Some( Scan.Link.Grapheme( peer )) =>

                              // store a new entry in the structure, and
                              // also make sure an update entry in scan map
                              // is produced (if it exists and is new)
                              def addNewEntry( segm: DefSeg ) {
                                 keyMap += key -> segm // (time -> value)
                                 val time          = segm.span.start
                                 val staleMap      = gPrio.get( time ).getOrElse( Map.empty )
                                 val m             = staleMap.getOrElse( staleID, Map.empty ) + (key -> segm)
                                 val newStaleMap   = staleMap + (staleID -> m)
                                 gPrio.add( time, newStaleMap )


                                 // deal with scanMap (fired update)
                                 // - for the changed entries, collect those which overlap the current
                                 //   transport time but not the previous one, so that they will go into
                                 //   the advancement message

                                 // if next segment is valid also for current time, re-use it
                                 if( segm.span.contains( newFrame )) {
                                    // only add it if it did not cover the previous transport position
                                    // (because if it did, it would not constitute a change)
                                    if( !segm.span.contains( oldFrame )) {
                                       scanMap += key -> segm
                                    }
                                 } else {
                                 // either no value, or ceilTime is larger than now, need to find value explicitly
                                    findAndAddToScanMap()
                                 }
                              }

                              // given that no potentially re-usable segment was found,
                              // find one for the new frame, and if one exists and is
                              // new (didn't overlap with the previous old frame), add it to the scan map
                              def findAndAddToScanMap() {
                                 peer.segment( newFrame ).foreach { nowSegm =>
                                    // only add it if it did not cover the previous transport position
                                    // (because if it did, it would not constitute a change)
                                    if( !nowSegm.span.contains( oldFrame )) {
                                       scanMap += key -> nowSegm
                                    }
                                 }
                              }

                              keyMap.get( key ) match {
                                 // first case: there was an entry in the previous info
                                 case Some( oldSegm ) =>
                                    val oldSegmSpan   = oldSegm.span
                                    val oldSegmTime   = oldSegmSpan.start
                                    // invalid if transport went backwards or past the previously stored segment's span
                                    val oldSegmInvalid = newFrame < oldFrame ||
                                                         (newFrame >= oldSegmTime && !oldSegmSpan.contains( newFrame ))
                                    val newSegm = if( oldSegmInvalid ) { // need to verify next time point
                                       val opt = peer.nearestEventAfter( newFrameP ).flatMap { ceilTime =>
                                          peer.segment( ceilTime )
                                       }
                                       opt.getOrElse( dummySegment )
                                    } else oldSegm

                                    if( newSegm != oldSegm ) {
                                       // remove old entry from gPrio
                                       gPrio.get( oldSegmTime ).foreach { staleMap =>
                                          staleMap.get( staleID ).foreach { mOld =>
                                             val m = mOld - key
                                             val newStaleMap = if( m.isEmpty ) {
                                                staleMap - staleID
                                             } else {
                                                staleMap + (staleID -> m)
                                             }
                                             if( newStaleMap.isEmpty ) {
                                                gPrio.remove( oldSegmTime )
                                             } else {
                                                gPrio.add( oldSegmTime -> newStaleMap )
                                             }
                                          }
                                       }
                                       // check if there is a new entry
                                       newSegm match {
                                          case segm: DefSeg =>
                                             // ...yes... store the new entry
                                             addNewEntry( segm )

                                          case _ =>
                                             // no event after newFrame
                                             keyMap -= key
                                             findAndAddToScanMap()
                                       }

                                    } else {
                                       // no change in next segment;
                                       // however if we went backwards in time, the current value might
                                       // have changed!
                                       if( newFrame < oldFrame ) findAndAddToScanMap()
                                    }

                                 // second case: there was not entry in the previous info.
                                 // if we advanced in time, there might be new data.
                                 // if we went back in time, it means there can't be any
                                 // previous data (so we won't need to search for an update segment)
                                 case _ =>
                                    if( newFrame > oldFrame ) {
                                       peer.nearestEventAfter( newFrameP ) match {
                                          case Some( ceilTime ) =>
                                             peer.segment( ceilTime ) match {
                                                case Some( ceilSegm ) =>
                                                   // a new entry
                                                   addNewEntry( ceilSegm )

                                                case _ => findAndAddToScanMap()
                                             }
                                          case _ => findAndAddToScanMap()
                                       }
                                    }
                              }

                           case _ =>   // for client timing, we are only interested in scans whose source is a grapheme
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
         val newInfo = oldInfo.copy( cpuTime          = logicalTime(),
                                     frame            = newFrame,
                                     state            = newState,
                                     nextProcTime     = nextProcTime,
                                     nextGraphemeTime = nextGraphemeTime )
         infoVar.set( newInfo )
if( VERBOSE ) println( "::: advance - newInfo = " + newInfo )

         if( procAdded.nonEmpty || procRemoved.nonEmpty || procUpdated.nonEmpty ) {
            val upd = Transport.Advance( transport = impl, time = newFrame,
                                         isSeek = isSeek, isPlaying = newInfo.isRunning,
                                         added = procAdded, removed = procRemoved, changes = procUpdated )
if( VERBOSE ) println( "::: advance - fire " + upd )
            fire( upd )
         }

         if( newState == Playing ) scheduleNext( newInfo )
      }
   }
}
