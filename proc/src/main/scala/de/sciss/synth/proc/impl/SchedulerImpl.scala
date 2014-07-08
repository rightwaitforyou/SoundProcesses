package de.sciss.synth.proc
package impl

import java.util.concurrent.TimeUnit

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.stm
import de.sciss.lucre.synth.Sys
import de.sciss.serial.ImmutableSerializer
import de.sciss.synth.proc
import proc.{logTransport => logT}

import scala.concurrent.stm.{Ref, TMap, TxnLocal}

object SchedulerImpl {
  def apply[S <: Sys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S]): Scheduler[S] = {
    val system  = tx.system
    // system.inMemoryTx
    implicit val iSys = system.inMemoryTx _ // (tx)
    implicit val itx  = iSys(tx)
    implicit val fuckYourselfScalaImplicitResolution = ImmutableSerializer.set[Int]
    val prio    = SkipList.Map.empty[system.I, Long, Set[Int]]()
    new Impl[S, system.I](prio)
  }

  private object Info {
    // the initial info is at minimum possible frame. that way, calling seek(0L) which initialise
    // the structures properly
    final val init: Info = apply(
      issueTime   = 0L,
      targetTime  = Long.MinValue + 1,
      valid       = -1
    )
  }

  /* Information about the current situation of the transport.
   *
   * @param issueTime          the CPU time in microseconds at which the info was last updated
   * @param targetTime         the next frame greater than `frame` at which a significant event happens in terms
   *                           of processes starting or stopping in the transport's group
   * @param valid               a counter which is automatically incremented by the `copy` method, used to determine
   *                            whether a scheduled runnable is still valid. the scheduler needs to read this value
   *                            before scheduling the runnable, then after the runnable is invoked, the current
   *                            info must be retrieved and its valid counter compared to the previously extracted
   *                            valid value. if both are equal, the runnable should go on executing, otherwise it
   *                            is meant to silently abort.
   */
  private final case class Info private(issueTime: Long, targetTime: Long, valid: Int) {
    def copy(issueTime: Long = issueTime, targetTime: Long = targetTime): Info =
      Info(issueTime = issueTime, targetTime = targetTime, valid = valid + 1)

    // does not increment valid
    def copy1(issueTime: Long = issueTime, targetTime: Long = targetTime): Info =
      Info(issueTime = issueTime, targetTime = targetTime, valid = valid)

    def incValid: Info = copy1()
  }

  private final class Impl[S <: Sys[S], I <: stm.Sys[I]](prio  : SkipList.Map [I , Long, Set[Int]])
                                                        (implicit cursor: stm.Cursor[S], iSys: S#Tx => I#Tx)
    extends Scheduler[S] {

    private final class ScheduledFunction(val time: Long, fun: S#Tx => Unit)

    type Token = Int

    private val timeZero    = System.nanoTime()
    private val timeRef     = TxnLocal(calcFrame())
    private val sampleRateN = 0.014112 // = Timeline.SampleRate * 1.0e-9
    private val tokenRef    = Ref(0)
    private val tokenMap    = TMap.empty[Int, ScheduledFunction]
    private val infoVar     = Ref(Info.init)

    def time(implicit tx: S#Tx): Long = timeRef.get(tx.peer)
    private def time_=(value: Long)(implicit tx: S#Tx): Unit = timeRef.set(value)(tx.peer)

    // ---- scheduling ----

    def schedule(time: Long)(fun: S#Tx => Unit)(implicit tx: S#Tx): Token = {
      implicit val itx = tx.peer
      val token = tokenRef.getAndTransform(_ + 1)
      tokenMap.put(token, new ScheduledFunction(time, fun))
      ???
      token
    }

    def cancel(token: Token)(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      implicit val itx: I#Tx = iSys(tx)
      tokenMap.remove(token).foreach { sch =>
        val t = sch.time
        prio.get(t)
      }
      ???
    }

    private def calcFrame(): Long = {
      // 1 ns = 10^-9 s
      val delta = System.nanoTime() - timeZero
      (delta * sampleRateN).toLong
    }

    // XXX TODO: `submit` should take `Info`

    /* Invoked to submit a schedule step either to a realtime scheduler or other mechanism.
     * When the step is performed, execution should be handed over to `eventReached`, passing
     * over the same three arguments.
     *
     * @param logicalNow       the logical now time at the time the event was scheduled
     * @param logicalDelay     the logical delay corresponding with the delay of the scheduled event
     *                         (the event `happens` at `logicalNow + logicalDelay`)
     * @param schedValid       the valid counter at the time of scheduling
     */
    private def submit(logicalNow: Long, logicalDelay: Long, schedValid: Int)(implicit tx: S#Tx): Unit = {
      val jitter        = calcFrame() - logicalNow
      val actualDelayN  = math.max(0L, ((logicalDelay - jitter) / sampleRateN).toLong)
      logT(s"scheduled: logicalDelay = $logicalDelay, actualDelay = $actualDelayN")
      tx.afterCommit {
        // logT("(after commit)")
        SoundProcesses.pool.schedule(new Runnable {
          def run(): Unit = {
            logT(s"scheduled: execute $schedValid")
            cursor.step { implicit tx =>
              eventReached(logicalNow = logicalNow, logicalDelay = logicalDelay, expectedValid = schedValid)
            }
          }
        }, actualDelayN, TimeUnit.NANOSECONDS)
      }
    }

    // XXX TODO: `eventReached` should take `Info`

    /* Invoked from the `submit` body after the scheduled event is reached.
     *
     * @param logicalNow       the logical now time at the time the event was scheduled
     * @param logicalDelay     the logical delay corresponding with the delay of the scheduled event
     * @param expectedValid    the valid counter at the time of scheduling
     */
    private def eventReached(logicalNow: Long, logicalDelay: Long, expectedValid: Int)(implicit tx: S#Tx): Unit = {
      // implicit val itx: I#Tx = tx
      implicit val ptx = tx.peer
      val info = infoVar()
      if (info.valid != expectedValid) return // the scheduled task was invalidated by an intermediate stop or seek command

      // this is crucial to eliminate drift: since we reached the scheduled event, do not
      // let the cpuTime txn-local determine a new free wheeling time, but set it to the
      // time we targeted at; then in the next scheduleNext call, the jitter is properly
      // calculated.
      val newLogical  = logicalNow + logicalDelay
      time            = newLogical
      val newTime     = info.targetTime
      advance(newTime)
    }

    private def advance(newTime: Long)(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx  = iSys(tx)
      implicit val ptx        = tx.peer
      val oldInfo             = infoVar()
      // val oldTime             = oldInfo.issueTime
      logT(s"advance(newTime = $newTime); oldInfo = $oldInfo")
      // do not short cut and return; because we may want to enforce play and call `scheduleNext`
      //         if( newFrame == oldFrame ) return

      val headOption    = prio.ceil(Long.MinValue) // headOption method missing
      val newTargetTime = headOption.fold(Long.MaxValue)(_._1)

      val newInfo = oldInfo.copy(issueTime = time, targetTime = newTargetTime)
      infoVar() = newInfo
      logT(s"advance - newInfo = $newInfo")

      scheduleNext(newInfo)
    }

    private def scheduleNext(info: Info)(implicit tx: S#Tx): Unit = {
      val targetTime = info.targetTime

      if (targetTime == Long.MaxValue) return

      val logicalDelay  = targetTime - info.issueTime //  * microsPerSample).toLong
      val logicalNow    = info.issueTime
      val schedValid    = info.valid

      submit(logicalNow = logicalNow, logicalDelay = logicalDelay, schedValid = schedValid)
    }
  }
}
