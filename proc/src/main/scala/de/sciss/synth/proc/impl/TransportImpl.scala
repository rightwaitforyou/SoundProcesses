/*
 *  TransportImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package proc
package impl

import de.sciss.lucre.{event => evt, synth, stm, bitemp, data}
import stm.{Disposable, IdentifierMap, Cursor}
import bitemp.BiGroup
import data.SkipList
import collection.immutable.{IndexedSeq => Vec}
import concurrent.stm.{Ref, Txn => ScalaTxn, TxnLocal}
import java.util.concurrent.TimeUnit
import java.text.SimpleDateFormat
import proc.{logTransport => log}
import de.sciss.span.{Span, SpanLike}
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.{model => m}
import de.sciss.lucre.synth.Sys

object TransportImpl {
  import Grapheme.Segment
  import Segment.{Defined => DefSeg}
  import Transport.Proc.{GraphemesChanged, Changed => ProcChanged}

  private type DefSegs = Vec[DefSeg]

  def apply[S <: Sys[S], I <: stm.Sys[I]](group: ProcGroup[S], sampleRate: Double)(
    implicit tx: S#Tx, cursor: Cursor[S], bridge: S#Tx => I#Tx): ProcTransport[S] = {

    val (groupH, infoVar, gMap, gPrio, timedMap, obsVar) = prepare[S, I](group)
    val t = new Realtime[S, I](groupH, sampleRate, infoVar, gMap, gPrio, timedMap, obsVar)
    t.init()
    t.seek(0L)
    t
  }

  def offline[S <: Sys[S], I <: stm.Sys[I]](group: ProcGroup[S], sampleRate: Double)(
    implicit tx: S#Tx, cursor: Cursor[S], bridge: S#Tx => I#Tx): Transport.Offline[S, Proc[S], Transport.Proc.Update[S]] = {

    val (groupH, infoVar, gMap, gPrio, timedMap, obsVar) = prepare[S, I](group)
    val t = new Offline[S, I](groupH, sampleRate, infoVar, gMap, gPrio, timedMap, obsVar)
    t.init()
    t
  }

  private def prepare[S <: Sys[S], I <: stm.Sys[I]](group: ProcGroup[S])
                                                   (implicit tx: S#Tx, bridge: S#Tx => I#Tx):
  (stm.Source[S#Tx, ProcGroup[S]],
    I#Var[Info],
    IdentifierMap[S#ID, S#Tx, ProcCache[S]],
    SkipList.Map[I, Long,  Set[S#ID]],
    IdentifierMap[S#ID, S#Tx, TimedProc[S]],
    I#Var[Vec[Observation[S, I]]]) = {

    implicit val itx: I#Tx  = tx
    val iid                 = itx.newID()
    implicit val infoSer    = dummySerializer[Info, I]
    val infoVar             = itx.newVar(iid, Info.init) // ( dummySerializer )
    val gMap                = tx.newInMemoryIDMap[ProcCache[S]] // (1)
    implicit val skipSer    = dummySerializer[Set[S#ID], I]
    val gPrio               = SkipList.Map.empty[I, Long, Set[S#ID]] // (2)
    val timedMap            = tx.newInMemoryIDMap[TimedProc[S]] // (3)
    implicit val obsSer     = dummySerializer[Vec[Observation[S, I]], I]
    val obsVar              = itx.newVar(iid, Vec.empty[Observation[S, I]])
    val groupH              = tx.newHandle(group)(ProcGroup.serializer)

    (groupH, infoVar, gMap, gPrio, timedMap, obsVar)
  }

  private def sysMicros() = System.nanoTime() / 1000

  private lazy val df = new SimpleDateFormat("HH:mm:ss.SSS")

  private object Info {
    // the initial info is at minimum possible frame. that way, calling seek(0L) which initialise
    // the structures properly
    final val init: Info = apply(
      cpuTime           = 0L,
      frame             = Long.MinValue,
      state             = Stopped,
      nextProcTime      = Long.MinValue + 1,
      nextGraphemeTime  = Long.MinValue + 1,
      valid             = -1
    )
  }

  /** Information about the current situation of the transport.
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
  private final case class Info private(cpuTime: Long, frame: Long, state: State, nextProcTime: Long,
                                        nextGraphemeTime: Long, valid: Int) {
    require(nextProcTime > frame && nextGraphemeTime > frame)

    def copy(cpuTime: Long = cpuTime, frame: Long = frame, state: State = state,
             nextProcTime: Long = nextProcTime, nextGraphemeTime: Long = nextGraphemeTime): Info =
      Info(cpuTime = cpuTime, frame = frame, state = state, nextProcTime = nextProcTime,
        nextGraphemeTime = nextGraphemeTime, valid = valid + 1)

    // does not increment valid
    def copy1(cpuTime: Long = cpuTime, frame: Long = frame, state: State = state,
              nextProcTime: Long = nextProcTime, nextGraphemeTime: Long = nextGraphemeTime): Info =
      Info(cpuTime = cpuTime, frame = frame, state = state, nextProcTime = nextProcTime,
        nextGraphemeTime = nextGraphemeTime, valid = valid)

    def incValid: Info = Info(cpuTime = cpuTime, frame = frame, state = state, nextProcTime = nextProcTime,
      nextGraphemeTime = nextGraphemeTime, valid = valid + 1)

    def isRunning = state == Playing

    def nextTime = math.min(nextProcTime, nextGraphemeTime)

    private def smartLong(n: Long): String = n match {
      case Long.MinValue => "-inf"
      case Long.MaxValue => "inf"
      case _ => n.toString
    }

    private def smartMicros(n: Long): String = {
      val msDelta = (n - sysMicros()) / 1000
      val d = new java.util.Date(System.currentTimeMillis() + msDelta)
      df.format(d)
    }

    override def toString = s"Info(cpuTime = ${smartMicros(cpuTime)}; frame = ${smartLong(frame)}, state = $state, " +
      s"nextProcTime = ${smartLong(nextProcTime)}, nextGraphemeTime = ${smartLong(nextGraphemeTime)}, valid = $valid)"
  }

  private sealed trait State
  private case object Stopped extends State
  private case object Playing extends State

  private def flatSpans[S <: Sys[S]](in: (SpanLike, Vec[TimedProc[S]])): Vec[(SpanLike, TimedProc[S])] = {
    val (span, procs) = in
    procs.map(span -> _)
  }

  //   private def flatSpans[ S <: Sys[ S ]]( in: (SpanLike, Vec[ TimedProc[ S ]])) : Vec[ TimedProc[ S ]] = in._2

  //   private val anyEmptySeq = Vec.empty[ Nothing ]
  //   @inline private def emptySeq[ A ] = anyEmptySeq.asInstanceOf[ Vec[ A ]]

  private val emptySeq = Vec.empty[Nothing]

  private def dummySerializer[A, I <: stm.Sys[I]]: Serializer[I#Tx, I#Acc, A] =
    DummySerializer.asInstanceOf[Serializer[I#Tx, I#Acc, A]]

  private object DummySerializer extends Serializer[stm.InMemory#Tx, stm.InMemory#Acc, Nothing] {
    def write(v: Nothing, out: DataOutput) = ()
    def read(in: DataInput, access: stm.InMemory#Acc)(implicit tx: stm.InMemory#Tx): Nothing =
      sys.error("Operation not supported")
  }

  private type Update[S <: Sys[S]] = Transport.Update[S, Proc[S], Transport.Proc.Update[S]]

  private final class Observation[S <: Sys[S], I <: stm.Sys[I]](impl: Impl[S, I], val fun: S#Tx => Update[S] => Unit)
    extends Disposable[S#Tx] {

    override def toString = impl.toString + ".react@" + hashCode().toHexString

    def dispose()(implicit tx: S#Tx): Unit = impl.removeObservation(this)
  }

  private final val dummySegment: Segment = Segment.Undefined(Span.from(Long.MaxValue))

  // --------------------------------------------

  private final case class GraphemeCache[S <: Sys[S]](nextTime: Long, grapheme: stm.Source[S#Tx, Grapheme[S]]) {
    def hasNextTime = nextTime != Long.MaxValue
  }
  // private type ScanCache[S <: Sys[S]] = Vec[GraphemeCache[S]]
  private final case class ScanCache[S <: Sys[S]](graphemes: Vec[GraphemeCache[S]]) {
    lazy val nextTime: Long = if (graphemes.isEmpty) Long.MaxValue else graphemes.map(_.nextTime).min
    def hasNextTime = nextTime != Long.MaxValue
  }
  private final case class ProcCache[S <: Sys[S]](staleID: S#ID, stop: Long,
                                                  scans: Map[String, ScanCache[S]]) {
    def nextTime: Long = math.min(stop, nextScanTime)
    lazy val nextScanTime: Long = if (scans.isEmpty) Long.MaxValue else scans.map(_._2.nextTime).min
    def hasNextTime = nextTime != Long.MaxValue

    def addScan(key: String, cache: ScanCache[S]): ProcCache[S] = copy(scans = scans + (key -> cache))
    def removeScan(key: String): ProcCache[S] = copy(scans = scans - key)
  }

  // --------------------------------------------

  // see `submit` docs for argument descriptions
  private final class OfflineStep(val logicalNow: Long, val logicalDelay: Long, val schedValid: Int)
  private final val offlineEmptyStep = new OfflineStep(0L, 0L, -1)

  private final class Offline[S <: Sys[S], I <: stm.Sys[I]](
          protected val groupHandle: stm.Source[S#Tx, ProcGroup[S]],
          val sampleRate: Double,
          protected val infoVar: I#Var[Info],
          protected val gMap: IdentifierMap[S#ID, S#Tx, ProcCache[S]],
          protected val gPrio: SkipList.Map[I, Long, Set[S#ID]],
          protected val timedMap: IdentifierMap[S#ID, S#Tx, TimedProc[S]],
          protected val obsVar: I#Var[Vec[Observation[S, I]]])
         (implicit val cursor: Cursor[S], protected val trans: S#Tx => I#Tx)
    extends Impl[S, I] with Transport.Offline[S, Proc[S], Transport.Proc.Update[S]] {

    private val submitRef = Ref(offlineEmptyStep)
    private val timeRef   = Ref(0L)

    protected def logicalTime             ()(implicit tx: S#Tx): Long = timeRef.get       (tx.peer)
    protected def logicalTime_=(value: Long)(implicit tx: S#Tx): Unit = timeRef.set(value)(tx.peer)

    protected def submit(logicalNow: Long, logicalDelay: Long, schedValid: Int)(implicit tx: S#Tx): Unit = {
      log("scheduled: logicalDelay = " + logicalDelay)
      submitRef.set(new OfflineStep(logicalNow, logicalDelay, schedValid))(tx.peer)
    }

    def step()(implicit tx: S#Tx): Unit = {
      val subm = submitRef.swap(offlineEmptyStep)(tx.peer)
      import subm._
      if (schedValid >= 0) {
        eventReached(logicalNow = logicalNow, logicalDelay = logicalDelay, expectedValid = schedValid)
      }
    }

    def stepTarget(implicit tx: S#Tx): Option[Long] = {
      val subm = submitRef()(tx.peer)
      import subm._
      // if (schedValid >= 0) Some(logicalNow + logicalDelay) else None
      if (schedValid >= 0) {
        implicit val itx: I#Tx = tx
        val info = infoVar()
        if (info.valid == schedValid) Some(info.nextTime) else None
      } else None
    }

    def position(implicit tx: S#Tx): Long = {
      implicit val itx: I#Tx = tx
      val info = infoVar()
      info.frame
    }

    def elapse(seconds: Double)(implicit tx: S#Tx): Unit = {
      val micros = (seconds * 1e6).toLong
      timeRef.transform(_ + micros)(tx.peer)
    }
  }

  private final val rt_cpuTime = TxnLocal(sysMicros())

  // system wide wall clock in microseconds

  private final class Realtime[S <: Sys[S], I <: stm.Sys[I]](
        protected val groupHandle: stm.Source[S#Tx, ProcGroup[S]],
        val sampleRate: Double,
        protected val infoVar: I#Var[Info],
        protected val gMap: IdentifierMap[S#ID, S#Tx, ProcCache[S]],
        protected val gPrio: SkipList.Map[I, Long, Set[S#ID]],
        protected val timedMap: IdentifierMap[S#ID, S#Tx, TimedProc[S]],
        protected val obsVar: I#Var[Vec[Observation[S, I]]])
       (implicit val cursor: Cursor[S], protected val trans: S#Tx => I#Tx)
    extends Impl[S, I] {

    protected def logicalTime             ()(implicit tx: S#Tx): Long = rt_cpuTime.get       (tx.peer)
    protected def logicalTime_=(value: Long)(implicit tx: S#Tx): Unit = rt_cpuTime.set(value)(tx.peer)

    protected def submit(logicalNow: Long, logicalDelay: Long, schedValid: Int)(implicit tx: S#Tx): Unit = {
      val jitter      = sysMicros() - logicalNow
      val actualDelay = math.max(0L, logicalDelay - jitter)
      log("scheduled: logicalDelay = " + logicalDelay + ", actualDelay = " + actualDelay)
      ScalaTxn.afterCommit(_ => {
        // log("(after commit)")
        SoundProcesses.pool.schedule(new Runnable {
          def run(): Unit = {
            log("scheduled: execute")
            cursor.step { implicit tx =>
              eventReached(logicalNow = logicalNow, logicalDelay = logicalDelay, expectedValid = schedValid)
            }
          }
        }, actualDelay, TimeUnit.MICROSECONDS)
      })(tx.peer)
    }
  }

  // ------------------------------------------------
  // ------------------------------------------------

  private sealed trait Impl[S <: Sys[S], I <: stm.Sys[I]]
    extends Transport[S, Proc[S], Transport.Proc.Update[S]] {
    impl =>

    private implicit final val procGroupSer = ProcGroup.serializer[S]
    private final val microsPerSample = 1000000 / sampleRate  // used in system time calculations

    // the three structures maintained for the update algorithm
    // (1) for each observed timed proc, store information about all scans whose sources contain graphemes.
    //     an observed proc is one whose time span overlaps the current span in the transport info.
    //     the value found in the map is a tuple. the tuple's first element is the _stale_ (timed-proc) ID which
    //     corresponds to the observed timed proc's ID in the transaction in which the underlying grapheme value
    //     was stored in structure (2), thereby allowing to find that value in (2) with a normal map.
    //     the tuple's second element is a map from the scan keys
    //     to a grapheme segment. the segment span's start time value is the value (transport time) at
    //     which the grapheme value was stored in (2)
    // (2) a skiplist is used as priority queue for the next interesting point in time at which the
    //     transport needs to emit an advancement message. the value is a map from stale timed-proc ID's
    //     to a map from scan keys to grapheme values, i.e. for scans whose source is a grapheme.
    // (3) a refreshment map for the timed procs
    protected def gMap    : IdentifierMap[S#ID, S#Tx, ProcCache[S]]   // (1)
    protected def gPrio   : SkipList.Map [I, Long   , Set[S#ID]]      // (2)
    protected def timedMap: IdentifierMap[S#ID, S#Tx, TimedProc[S]]   // (3)

    protected def groupHandle: stm.Source[S#Tx, ProcGroup[S]]

    protected def infoVar: I#Var[Info]

    protected def obsVar: I#Var[Vec[Observation[S, I]]]

    protected implicit def trans: S#Tx => I#Tx

    private final var groupObs: Disposable[S#Tx] = _

    private final class GroupUpdateState(val g: ProcGroup[S], info0: Info) {
      var info: Info = info0
      var procAdded:   Vec[TimedProc[S]] = emptySeq
      var procRemoved: Vec[TimedProc[S]] = emptySeq
      var procChanged: Vec[(TimedProc[S], Transport.Proc.Update[S])] = emptySeq

      def shouldFire: Boolean = procAdded.nonEmpty || procRemoved.nonEmpty || procChanged.nonEmpty

      def infoChanged: Boolean = info != info0

      def advanceMessage = {
        val isPly = info.isRunning
        Transport.Advance(transport = impl, time = info.frame, isSeek = false, isPlaying = isPly,
          added = procAdded, removed = procRemoved, changes = procChanged)
      }
    }

    /** Invoked to submit a schedule step either to a realtime scheduler or other mechanism.
      * When the step is performed, execution should be handed over to `eventReached`, passing
      * over the same three arguments.
      *
      * @param logicalNow       the logical now time at the time the event was scheduled
      * @param logicalDelay     the logical delay corresponding with the delay of the scheduled event
      *                         (the event `happens` at `logicalNow + logicalDelay`)
      * @param schedValid       the valid counter at the time of scheduling
      */
    protected def submit(logicalNow: Long, logicalDelay: Long, schedValid: Int)(implicit tx: S#Tx): Unit

    /** Invoked from the `submit` body after the scheduled event is reached.
      *
      * @param logicalNow       the logical now time at the time the event was scheduled
      * @param logicalDelay     the logical delay corresponding with the delay of the scheduled event
      * @param expectedValid    the valid counter at the time of scheduling
      */
    final protected def eventReached(logicalNow: Long, logicalDelay: Long, expectedValid: Int)(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx = tx
      val info = infoVar()
      if (info.valid != expectedValid) return // the scheduled task was invalidated by an intermediate stop or seek command

      // this is crucial to eliminate drift: since we reached the scheduled event, do not
      // let the cpuTime txn-local determine a new free wheeling time, but set it to the
      // time we targetted at; then in the next scheduleNext call, the jitter is properly
      // calculated.
      val newLogical = logicalNow + logicalDelay
      logicalTime_=(newLogical)
      advance(newFrame = info.nextTime)
    }

    protected def logicalTime()(implicit tx: S#Tx): Long

    protected def logicalTime_=(value: Long)(implicit tx: S#Tx): Unit

    // the following illustrates what actions need to be done with respect
    // to the overlap/touch or not between the current info span and the change span
    // (the `v` indicates the result of calcCurrentTime; ! is a non-important point, whereas | is an important point)
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

    // returns: `true` if the changes are perceived (updates should be fired), else `false`
    private def u_addRemoveProcs(state      : GroupUpdateState,
                                 doFire     : Boolean,
                                 oldSpan    : SpanLike,
                                 newSpan    : SpanLike,
                                 procAdded  : Option[TimedProc[S]],
                                 procRemoved: Option[TimedProc[S]])(implicit tx: S#Tx): Unit = {

      val oldInfo   = state.info
      val newFrame  = oldInfo.frame

      @inline def calcNeedsNewProcTime(span: SpanLike): Boolean = span match {
        case hs: Span.HasStart  => oldInfo.frame <= hs.start && oldInfo.nextProcTime > hs.start
        case _                  => false
      }

      // perceived means the proc was either active at the current moment in time (`newFrame`), or it becomes active
      val perceived = oldSpan.contains(newFrame) || newSpan.contains(newFrame) // (1)
      val needsNewProcTime = perceived || // (2)
          calcNeedsNewProcTime(oldSpan) || calcNeedsNewProcTime(newSpan)

      if (!(perceived || needsNewProcTime)) return // keep scheduled task running, don't overwrite infoVar

      if (perceived) {
        procRemoved.foreach { timed =>
          removeProc(timed)
          if (doFire) state.procRemoved :+= timed
        }
        procAdded.foreach { timed =>
          addProc(newFrame, timed)
          if (doFire) state.procAdded :+= timed
        }
      }

      val nextProcTime = if (needsNewProcTime) {
        state.g.nearestEventAfter(newFrame + 1).getOrElse(Long.MaxValue)
      } else {
        oldInfo.nextProcTime
      }
      val nextGraphemeTime = if (perceived) {
        implicit val itx: I#Tx = tx
        val headOption = gPrio.ceil(Long.MinValue) // headOption method missing
        headOption.map(_._1).getOrElse(Long.MaxValue)
      } else {
        oldInfo.nextGraphemeTime
      }
      val newInfo = oldInfo.copy1(nextProcTime = nextProcTime, nextGraphemeTime = nextGraphemeTime)
      state.info = newInfo
    }

    /* A movement in time of the proc has been observed. */
    private def u_moveProc(state: GroupUpdateState, timed: TimedProc[S],
                           oldSpan: SpanLike, newSpan: SpanLike)(implicit tx: S#Tx): Unit = {
      // ... possible situations
      //     (1) old span contains current time frame `v`, but new span not --> treat as removal
      //     (2) old span does not contain `v`, but new span does --> treat as addition
      //     (3) neither old nor new span contain `v`
      //         --> info.span contains start point of either span? then recalc nextProcTime.
      //         (indeed we could call addRemoveProcs with empty lists for the procs, and a
      //          second span argument, which would be just Span.Void in the normal add/remove calls)
      //     (4) both old and new span contain `v`
      //         --> remove map entries (gMap -> gPrio), and rebuild them, then calc new next times

      val newFrame    = state.info.frame

      val oldPercv    = oldSpan.contains(newFrame)
      val newPercv    = newSpan.contains(newFrame)
      val doFire      = oldPercv ^ newPercv                 // fire for cases (1) and (2)

      val procRemoved = if (oldPercv) Some(timed) else None // case (1)
      val procAdded   = if (newPercv) Some(timed) else None // case (2)

      // if the proc was active and is still active, we simplify by successive removal and (re-)addition
      u_addRemoveProcs(state, doFire = doFire, oldSpan = oldSpan, newSpan = newSpan,
        procAdded = procAdded, procRemoved = procRemoved)
    }

    final def init()(implicit tx: S#Tx): Unit = {
      // we can use groupStale because init is called straight away after instantiating Impl
      groupObs = group.changed.react { implicit tx =>
        biGroupUpdate(_)(tx)
      }
      // seek(0L)
    }

    // adds a segment to the update to be fired (does not update the structure)
    private def u_addSegments(state: GroupUpdateState, timed: TimedProc[S], key: String, segms: Vec[Grapheme.Segment])
                             (implicit tx: S#Tx): Unit = {
      val entry = key -> segms
      // try to re-use and update a previous grapheme changed message in the state
      state.procChanged.lastOption match {
        case Some((`timed`, GraphemesChanged(map))) =>
          val newMap = GraphemesChanged[S](map + entry)
          state.procChanged = state.procChanged.init :+ (timed -> newMap)
        case _ =>
          val map = GraphemesChanged[S](Map(entry))
          state.procChanged :+= timed -> map
      }
    }

    private def removePrio(cache: ProcCache[S])(implicit tx: S#Tx): Unit =
      if (cache.hasNextTime) {
        implicit val itx: I#Tx  = tx
        val setRemoved = gPrio.get(time).fold(Set.empty[S#ID])(_ - cache.staleID)
        if (setRemoved.isEmpty) {
          gPrio.remove(cache.nextTime)
        } else {
          gPrio.add(cache.nextTime -> setRemoved)
        }
      }

    private def addPrio(cache: ProcCache[S])(implicit tx: S#Tx): Unit =
      if (cache.hasNextTime) {
        implicit val itx: I#Tx  = tx
        val setAdded = gPrio.get(time).fold(Set(cache.staleID))(_ + cache.staleID)
        gPrio.add(cache.nextTime -> setAdded)
      }

    // adds the structure for a newly added scan
    // NOT: also adds the current grapheme segment if applicable
    private def u_addScan(state: GroupUpdateState, timed: TimedProc[S], key: String, scan: Scan[S])
                         (implicit tx: S#Tx): Unit = {
      val newFrame = state.info.frame
      for {
        scanCache <- mkScanCache(key, scan, newFrame)
        procCache <- gMap.get(timed.id)
      } {
        val procCacheNew = procCache.addScan(key, scanCache)
        gMap.put(timed.id, procCacheNew)
        val oldTime = procCache.nextTime
        val newTime = procCacheNew.nextTime
        if (oldTime != newTime) {
          removePrio(procCache)
          addPrio(procCacheNew)

          val needsNewGraphemeTime = newTime < state.info.nextGraphemeTime
          if (needsNewGraphemeTime) {
            state.info = state.info.copy1(nextGraphemeTime = newTime /* time */)
          }
        }
      }
    }

    // store a new scan connected to grapheme sources in the structure,
    // given the already calculated segments
    private def u_addScan2(state: GroupUpdateState, timed: TimedProc[S], key: String, segm: DefSeg)
                          (implicit tx: S#Tx): Unit = {
//      implicit val itx: I#Tx  = tx
//      val id                  = timed.id
//      val (staleID, keyMap1)  = gMap.get(id).getOrElse(id -> Map.empty[String, DefSegs])
//      val entry               = key -> segms
//      val newKeyMap1          = keyMap1 + entry
//      gMap.put(id, staleID -> newKeyMap1)
//      val time                = segms.map(_.span.start).min
//      val skipMap             = gPrio.get(time).getOrElse(Map.empty)
//      val keyMap2             = skipMap.getOrElse(staleID, Map.empty)
//      val newKeyMap2          = keyMap2 + entry
//      val newSkipMap          = skipMap + (staleID -> newKeyMap2)
//      gPrio.add(time -> newSkipMap)
//
//      val needsNewGraphemeTime = time < state.info.nextGraphemeTime
//      if (needsNewGraphemeTime) {
//        //            assert( time > newFrame )
//        state.info = state.info.copy1(nextGraphemeTime = time)
//      }
    }

    private def u_removeScan(timed: TimedProc[S], key: String)(implicit tx: S#Tx): Unit = {
      val id = timed.id
      gMap.get(id).foreach { procCache =>
        val procCacheNew  = procCache.removeScan(key)
        gMap.put(timed.id, procCacheNew)
        val oldTime       = procCache.nextTime
        val newTime       = procCacheNew.nextTime
        if (oldTime != newTime) {
          removePrio(procCache)
          addPrio(procCacheNew)

          // XXX TODO: should we update state.info?
        }
      }
    }

    //        AssociativeChange : we need to track the addition and removal of scans.
    //                            filter only those AssociativeKeys which are ScanKeys.
    //                            track appearance or disappearence of graphemes as sources
    //                            of these scans, and update structures
    // addendum: Meaning, _do not_ include segments in updates
    private def u_assocChange(state: GroupUpdateState, timed: TimedProc[S], change: Proc.AssociativeChange[S])
                             (implicit tx: S#Tx): Unit = {
      change match {
        case Proc.AssociationAdded(Proc.ScanKey(key)) =>
          val proc = timed.value
          proc.scans.get(key).foreach {
            scan => u_addScan(state, timed, key, scan)
          }

        case Proc.AssociationRemoved(Proc.ScanKey(key)) =>
          u_removeScan(timed, key)

        case _ =>
      }
    }

    private def u_graphemeChange(state: GroupUpdateState, timed: TimedProc[S], key: String, scan: Scan[S],
                                 grapheme: Grapheme[S], graphCh: Vec[Grapheme.Segment])(implicit tx: S#Tx): Unit = {
      //        SourceUpdate (passing on changes in a grapheme source) :
      //          - grapheme changes must then be tracked, structures updated

      // graphUpd.changes: Vec[Segment]

      val id = timed.id
      gMap.get(id).foreach { procCache =>
        val newFrame  = state.info.frame
        // determine closest new segment (`newSegm`) and add all relevant segments to update list
        val (s1, s2) = ((dummySegment -> Vec.empty[Segment]) /: graphCh) {
          case ((nextSegm, dirtySpans), segm) =>
            val span        = segm.span
            val start       = span.start
            val nextSegm1   = if (segm.isDefined && start > newFrame &&
              start < math.min(nextSegm.span.start, procCache.nextTime)) segm else nextSegm
            val dirtySpans1 = if (span.contains(newFrame)) dirtySpans :+ segm else dirtySpans
            nextSegm1 -> dirtySpans1
        }
        if (s2.nonEmpty) u_addSegments(state, timed, key, s2)

        s1 match {
          case defSegm: Segment.Defined /* if defSegm.span.overlaps(segmOld.span) */ =>
            u_removeScan(timed, key)
            u_addScan(state, timed, key, scan)
            // u_addScan2(state, timed, key, defSegm)
          case _ =>
        }
      }
    }

    //    private def u_scanSourceAdded(state: GroupUpdateState, timed: TimedProc[S], key: String, scan: Scan[S],
    //                                  source: Grapheme[S])(implicit tx: S#Tx): Unit = {
    //      u_removeScan(timed, key)
    //      u_addScan(state, timed, key, sourceOpt)
    //    }

    private def u_scanSourceChange(state: GroupUpdateState, timed: TimedProc[S], key: String, scan: Scan[S])
                                  (implicit tx: S#Tx): Unit = {
      //        SourceChanged : if it means a grapheme is connected or disconnect, update structures
      u_removeScan(timed, key)
      u_addScan(state, timed, key, scan)
    }

    // a change in the transported group has been observed.
    private def biGroupUpdate(groupUpd: BiGroup.Update[S, Proc[S], Proc.Update[S]])(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx = tx
      val state = {
        val info0     = infoVar()
        val newFrame  = calcCurrentTime(info0)
        // actualise logical time and frame, but do _not_ increment valid counter
        // (call `copy1` instead of `copy`; valid counter is incremented only if
        //  if info is written back to infoVar)
        val info1     = info0.copy1(cpuTime = logicalTime(), frame = newFrame)
        // info only needs to be written back to infoVar if it is != info0 at the end of the processing
        new GroupUpdateState(groupUpd.group, info1)
      }

      groupUpd.changes.foreach {
        case BiGroup.Added(span, timed) =>
          u_addRemoveProcs(state, doFire = true, oldSpan = Span.Void, newSpan = span,
            procAdded = Some(timed), procRemoved = None)

        case BiGroup.Removed(span, timed) =>
          u_addRemoveProcs(state, doFire = true, oldSpan = Span.Void, newSpan = span,
            procAdded = None, procRemoved = Some(timed))

        // changes: Vec[ (TimedProc[ S ], BiGroup.ElementUpdate[ U ])]
        // ElementUpdate is either of Moved( change: m.Change[ SpanLike ])
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

        case BiGroup.ElementMutated(timed, procUpd) if gMap.contains(timed.id) =>
          def forward(u: Proc.Change[S]): Unit =
            state.procChanged :+= timed -> ProcChanged(u)

          procUpd.changes.foreach {
            case assoc: Proc.AssociativeChange[S] =>
              forward(assoc)
              u_assocChange(state, timed, assoc)

            case sc @ Proc.ScanChange(key, scan, scanChanges) =>
              val flt = scanChanges.filter {
                case Scan.GraphemeChange(grapheme, graphCh) =>
                  u_graphemeChange(state, timed, key, scan, grapheme, graphCh)
                  false // scan changes are filtered and prepared already by u_scanSourceUpdate

                case u: Scan.SourceChange[S] =>
                  u_scanSourceChange(state, timed, key, scan)
                  true

                case _ => true // SinkAdded, SinkRemoved
              }
              if (flt.nonEmpty) forward(sc.copy(changes = flt))

            case other => // StateChange other than AssociativeChange, or GraphemeChange
              forward(other)
          }

        case BiGroup.ElementMoved(timed, m.Change(oldSpan, newSpan)) if gMap.contains(timed.id) =>
           u_moveProc(state, timed, oldSpan, newSpan)

        case _ => // mutated or moved unobserved proc
      }

      if (state.shouldFire) fire(state.advanceMessage)
      if (state.infoChanged) {
        // meaning that either nextProcTime or nextGraphemeTime changed
        val infoNew = state.info.incValid
        val isPly = infoNew.isRunning
        infoVar() = infoNew
        if (isPly) scheduleNext(infoNew)
      }
    }

    final def dispose()(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx = tx
      groupObs.dispose()
      infoVar() = Info.init // if there is pending scheduled tasks, they should abort gracefully
      //         infoVar.dispose()
      gMap    .dispose()
      gPrio   .dispose()
      timedMap.dispose()
      obsVar  .dispose()
    }

    override def toString = s"Transport(group=$groupHandle)@${hashCode.toHexString}"

    final def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, (SpanLike, TimedProc[S])] =
      group.intersect(time).flatMap(flatSpans)

    private def calcCurrentTime(info: Info)(implicit tx: S#Tx): Long = {
      val startFrame = info.frame
      if (info.isRunning) {
        val logicalNow    = logicalTime()
        val logicalDelay  = logicalNow - info.cpuTime
        val stopFrame     = info.nextTime
        // for this to work as expected it is crucial that the reported current time is
        // _less than_ the info's target time (<= stopFrame -1)
        math.min(stopFrame - 1, startFrame + (logicalDelay / microsPerSample).toLong)
      } else startFrame
    }

    final def time(implicit tx: S#Tx): Long = {
      implicit val itx: I#Tx = tx
      calcCurrentTime(infoVar())
    }

    //      def playing( implicit tx: S#Tx ) : Expr[ S, Boolean ] = playingVar.get
    //      def playing_=( expr: Expr[ S, Boolean ])( implicit tx: S#Tx ): Unit = playingVar.set( expr )

    final def play()(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx = tx
      val oldInfo = infoVar()
      if (oldInfo.isRunning) return
      val newInfo = oldInfo.copy(cpuTime = logicalTime(), state = Playing)
      infoVar()   = newInfo
      fire(Transport.Play(impl, newInfo.frame))
      scheduleNext(newInfo)
    }

    final def stop()(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx = tx
      val oldInfo = infoVar()
      if (!oldInfo.isRunning) return

      val newInfo = oldInfo.copy(cpuTime = logicalTime(), frame = calcCurrentTime(oldInfo), state = Stopped)
      infoVar() = newInfo
      fire(Transport.Stop(impl, newInfo.frame))
    }

    final def seek(time: Long)(implicit tx: S#Tx): Unit = {
      advance(isSeek = true, newFrame = time)
    }

    final def isPlaying(implicit tx: S#Tx): Boolean = {
      implicit val itx: I#Tx = tx
      infoVar().isRunning
    }

    private def scheduleNext(info: Info)(implicit tx: S#Tx): Unit = {
      //         implicit val itx  = tx.inMemory
      //         val info          = infoVar.get
      val targetFrame = info.nextTime

      if ( /* !info.isRunning || */ targetFrame == Long.MaxValue) return

      val logicalDelay = ((targetFrame - info.frame) * microsPerSample).toLong
      val logicalNow = info.cpuTime
      val schedValid = info.valid

      submit(logicalNow = logicalNow, logicalDelay = logicalDelay, schedValid = schedValid)
    }

    final def group(implicit tx: S#Tx): ProcGroup[S] = groupHandle() // tx.refresh( csrPos, groupStale )

    private def fire(update: Update[S])(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx = tx
      val obs = obsVar()
      obs.foreach(_.fun(tx)(update))
    }

    final def removeObservation(obs: Observation[S, I])(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx = tx
      obsVar.transform(_.filterNot(_ == obs))
    }

    final def react(fun: S#Tx => Update[S] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = {
      implicit val itx: I#Tx = tx
      val obs = new Observation[S, I](this, fun)
      obsVar.transform(_ :+ obs)
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

    private def removeProc(timed: TimedProc[S])(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx = tx
      val id        = timed.id
      timedMap.remove(id) // in (3)
      val entries   = gMap.get(id)
      gMap.remove(id)     // in (1)
      entries.foreach(removePrio)
    }

    private def mkScanCache(key: String, scan: Scan[S], newFrame: Long)(implicit tx: S#Tx): Option[ScanCache[S]] = {
      val graphemesIt = scan.sources.flatMap {
        case Scan.Link.Grapheme(peer) => peer.nearestEventAfter(newFrame + 1).map(GraphemeCache(_, tx.newHandle(peer)))
        case _                        => None
      }
      if (graphemesIt.nonEmpty) {
        val graphemes = graphemesIt.toIndexedSeq
        Some(ScanCache[S](graphemes))

      } else None
    }

    /*
      @param  newFrame  the current transport frame
     */
    private def addProc(newFrame: Long, timed: TimedProc[S])(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx = tx
      val id          = timed.id
      val proc        = timed.value

      val scansIt     = proc.scans.iterator.flatMap {
        case (key, scan) => mkScanCache(key, scan, newFrame).map(key -> _)
      }

      val scans     = scansIt.toMap
      val procStop  = timed.span.value match {
        case hs: Span.HasStop => hs.stop
        case _                => Long.MaxValue
      }
      val procCache = ProcCache(staleID = id, stop = procStop, scans = scans)

      gMap.put(id, procCache)           // in (1)
      if (procCache.hasNextTime) {
        val time    = procCache.nextTime
        val newSet  = gPrio.get(time).getOrElse(Set.empty) + id
        gPrio.add(time -> newSet)       // in (2)
      }

      timedMap.put(id, timed)           // in (3)
    }

    /** The core method: based on the previous info and the reached target frame, update the structures
      * accordingly (invalidate obsolete information, gather new information regarding the next
      * advancement), assemble and fire the corresponding events, and update the info to be ready for
      * a subsequent call to `scheduleNext`.
      *
      * @param isSeek     whether the advancement is due to a hard seek (`true`) or a regular passing of time (`false`).
      *                   the information is carried in the fired event.
      * @param newFrame   the frame which has been reached
      */
    private def advance(newFrame: Long, isSeek: Boolean = false, startPlay: Boolean = false)(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx = tx
      val oldInfo           = infoVar()
      val oldFrame          = oldInfo.frame
      log(s"advance(newFrame = $newFrame, isSeek = $isSeek, startPlay = $startPlay); oldInfo = $oldInfo")
      // do not short cut and return; because we may want to enforce play and call `scheduleNext`
      //         if( newFrame == oldFrame ) return

      val g                 = group
      val needsNewProcTime  = newFrame < oldFrame || newFrame >= oldInfo.nextProcTime
      val newFrameP         = newFrame + 1

      var procAdded:   Vec[TimedProc[S]] = emptySeq
      var procRemoved: Vec[TimedProc[S]] = emptySeq
      var procUpdated: Vec[(TimedProc[S], Transport.Proc.Update[S])] = emptySeq

      // algorithm [A] or [B]
      if (needsNewProcTime) {
        // algorithm [A]
        val (itAdded, itRemoved) = if (newFrame == oldInfo.nextProcTime) {
          // we went exactly till a known event spot
          // - find the procs to remove and add via group.eventsAt.
          g.eventsAt(newFrame)

          // algorithm [B]
        } else {
          // the new time frame lies outside the range for the known next proc event.
          // therefore we need to fire proc additions and removals (if there are any)
          // and recalculate the next proc event time after the new time frame

          // - find the procs with to remove and add via range searches

          val (remStart, remStop, addStart, addStop) = if (newFrame > oldFrame) {
            // ... those which end in the interval (LRP, t] && begin <= LRP must be removed ...
            // ... those which begin in the interval (LRP, t] && end > t must be added ...
            val skipInt = Span(oldFrame + 1, newFrameP)
            (Span.until(oldFrame + 1), skipInt, skipInt, Span.from(newFrameP))
          } else {
            // ... those which begin in the interval (t, LRP] && end > LRP must be removed ...
            // ... those which end in the interval (t, LRP] && begin <=t must be added ...
            val skipInt = Span(newFrameP, oldFrame + 1)
            (skipInt, Span.from(oldFrame + 1), Span.until(newFrameP), skipInt)
          }

          (g.rangeSearch(addStart, addStop), g.rangeSearch(remStart, remStop))
        }

        // continue algorithm [A] with removed and added procs

        // - for the removed procs, remove the corresponding entries in (1), (2), and (3)
        procRemoved = itRemoved.flatMap(_._2).toIndexedSeq
        procRemoved.foreach(removeProc)

        // - for the added procs, find all sinks whose source connects to a grapheme. calculate the values
        //   of those graphemes at the current transport time. (NOT YET: this goes with the Transport.Update so that
        //   listeners do not need to perform that step themselves. Also this information is useful because it
        //   yields the ceil time ct); if no value is found at the current transport time, find the ceiling time ct
        //   explicitly; for ct, evaluate the grapheme value and store it in (1) and (2) accordingly.
        //   store procs in (3)
        procAdded = itAdded.flatMap(_._2).toIndexedSeq
        procAdded.foreach(timed => addProc(newFrame, timed))
      }

      val needsNewGraphemeTime = newFrame < oldFrame || newFrame >= oldInfo.nextGraphemeTime

      // algorithm [C] or [D]
      if (needsNewGraphemeTime) {
        // [C]
        val updMap: Vec[(TimedProc[S], Map[String, DefSegs])] = if (newFrame == oldInfo.nextGraphemeTime) {
          // we went exactly till a known event spot

          // - in (2) find and remove the map for the given time frame
          // - for each scan (S#ID, String) collect the grapheme values so that they be dispatched
          //   in the advancement message
          // - and for each of these scans, look up the timed proc through (3) and gather the new next grapheme
          //   values, store (replace) them in (1) and (2), and calculate the new nextGraphemeTime.

          val staleSet = gPrio.remove(newFrame).getOrElse(emptySeq)

          // the following is horrible with flatMap, let's just use mutable builders, easier to read
          var scanMap = Vec.empty[(TimedProc[S], Map[String, DefSegs])]

          staleSet.foreach { staleID =>
            for {
              timed     <- timedMap.get(staleID)
              procCache <- gMap    .get(staleID)
            } {
              if (procCache.stop == newFrame) {
                removeProc(timed)

              } else {
                var map       = Map.empty[String, DefSegs]
                var newScans  = Map.empty[String, ScanCache[S]]
                procCache.scans.foreach { case (key, scanCache) =>
                  var newGraphemes  = Vec.empty[GraphemeCache[S]]
                  var defSegs       = Vec.empty[DefSeg]
                  scanCache.graphemes.foreach { gCache =>
                    if (gCache.nextTime == newFrame) {
                      val grapheme  = gCache.grapheme()
                      grapheme.segment(newFrame).foreach { segm =>
                        segm.span match {
                          case hs: Span.HasStop =>
                            newGraphemes :+= GraphemeCache(hs.stop, gCache.grapheme)
                          case _ =>
                        }
                        defSegs :+= segm
                      }
                    } else {
                      newGraphemes :+= gCache // keep old information
                    }
                  }
                  if (defSegs     .nonEmpty) map      += key -> defSegs
                  if (newGraphemes.nonEmpty) newScans += key -> ScanCache(newGraphemes)
                }
                if (map.nonEmpty) scanMap :+= (timed, map)
                val procCacheNew  = procCache.copy(scans = newScans)
                // removePrio(newFrame, procCache.staleID) - that's wrong?!
                removePrio(procCache)
                addPrio(procCacheNew)
                gMap.put(timed.id, procCacheNew)
              }
            }
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

          val newProcs: Set[TimedProc[S]] = procAdded.toSet // .map( _._2 )( breakOut )

          // filter because new procs have already build up their scan maps
          val oldProcs = g.intersect(newFrame).flatMap(_._2.filterNot(newProcs.contains))
          oldProcs.foreach { timed =>
            val id  = timed.id
            val p   = timed.value
            gMap.get(id).foreach { procCache =>
              var scanMap = Map.empty[String, ScanCache[S]] // this will be the new map
              p.scans.iterator.foreach {
                case (key, scan) =>
                  scan.sources.foreach {
                    case Scan.Link.Grapheme(peer) =>
                      mkScanCache(key, scan, newFrame).foreach(scanMap += key -> _)

                    case _ =>
                  }
              }
              val procCacheNew = procCache.copy(scans = scanMap)
              gMap.put(id, procCacheNew)
              removePrio(procCache)
              addPrio(procCacheNew)
            }
          }
          Vec.empty // XXX TODO itMap.toIndexedSeq
        }

        procUpdated = updMap.map {
          case (timed, map) => timed -> GraphemesChanged[S](map)
        }
      }

      val nextProcTime = if (needsNewProcTime) {
        g.nearestEventAfter(newFrameP).getOrElse(Long.MaxValue)
      } else {
        oldInfo.nextProcTime
      }

      val nextGraphemeTime = if (needsNewGraphemeTime) {
        val headOption = gPrio.ceil(Long.MinValue) // headOption method missing
        headOption.map(_._1).getOrElse(Long.MaxValue)
      } else {
        oldInfo.nextGraphemeTime
      }

      val newState: State = if (startPlay) Playing else oldInfo.state
      val newInfo = oldInfo.copy(cpuTime = logicalTime(),
        frame             = newFrame,
        state             = newState,
        nextProcTime      = nextProcTime,
        nextGraphemeTime  = nextGraphemeTime
      )
      infoVar() = newInfo
      log("advance - newInfo = " + newInfo)

      if (procAdded.nonEmpty || procRemoved.nonEmpty || procUpdated.nonEmpty) {
        val upd = Transport.Advance(transport = impl, time = newFrame,
          isSeek = isSeek, isPlaying = newInfo.isRunning,
          added = procAdded, removed = procRemoved, changes = procUpdated)
        logTransport("advance - fire " + upd)
        fire(upd)
      }

      if (newState == Playing) scheduleNext(newInfo)
    }
  }
}
