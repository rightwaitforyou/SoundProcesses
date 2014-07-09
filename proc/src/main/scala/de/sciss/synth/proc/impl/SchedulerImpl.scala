package de.sciss.synth.proc
package impl

import java.util.concurrent.TimeUnit

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm
import de.sciss.serial.ImmutableSerializer
import de.sciss.synth.proc
import proc.{logTransport => logT}

import scala.concurrent.stm.{Ref, TMap, TxnLocal}
import scala.util.control.NonFatal

object SchedulerImpl {
  def apply[S <: Sys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S]): Scheduler[S] = {
    val system  = tx.system
    implicit val iSys = system.inMemoryTx _
    implicit val itx  = iSys(tx)
    implicit val fuckYourselfScalaImplicitResolution = ImmutableSerializer.set[Int]
    val prio    = SkipList.Map.empty[system.I, Long, Set[Int]]()
    new Impl[S, system.I](prio)
  }

  /* Information about the current situation of the scheduler.
   *
   * @param issueTime          the CPU time in sample frames at which the info was last updated
   * @param targetTime         the next frame at which a significant event happens in terms
   */
  private final class Info(val issueTime: Long, val targetTime: Long) {
    def delay: Long = targetTime - issueTime

    override def toString = s"[issueTime = $issueTime, targetTime = $targetTime]"
  }

  private val infInfo = new Info(issueTime = 0L, targetTime  = Long.MaxValue)

  // one can argue whether the values should be ordered, e.g. Seq[Int] instead of Set[Int],
  // such that if two functions A and B are submitted after another for the same target time,
  // then A would be executed before B. But currently we don't think this is an important aspect.
  private final class Impl[S <: Sys[S], I <: stm.Sys[I]](prio  : SkipList.Map [I , Long, Set[Int]])
                                                        (implicit val cursor: stm.Cursor[S], iSys: S#Tx => I#Tx)
    extends Scheduler[S] {

    private final class ScheduledFunction(val time: Long, val fun: S#Tx => Unit)

    type Token = Int

    private val timeZero    = System.nanoTime()
    private val timeRef     = TxnLocal(calcFrame())
    private val sampleRateN = 0.014112 // = Timeline.SampleRate * 1.0e-9
    private val tokenRef    = Ref(0)
    private val tokenMap    = TMap.empty[Int, ScheduledFunction]
    private val infoVar     = Ref(infInfo)

    def time(implicit tx: S#Tx): Long = timeRef.get(tx.peer)
    private def time_=(value: Long)(implicit tx: S#Tx): Unit = timeRef.set(value)(tx.peer)

    // ---- scheduling ----

    def schedule(targetTime: Long)(fun: S#Tx => Unit)(implicit tx: S#Tx): Token = {
      implicit val ptx  = tx.peer
      implicit val itx: I#Tx = iSys(tx)
      val t             = time
      if (targetTime < t) throw new IllegalArgumentException(s"Cannot schedule in the past ($targetTime < $time)")
      val token         = tokenRef.getAndTransform(_ + 1)
      tokenMap.put(token, new ScheduledFunction(targetTime, fun))
      val oldInfo       = infoVar()
      val reschedule    = targetTime < oldInfo.targetTime

      if (reschedule) {   // implies that prio does not have an entry at `timeClip` yet
        prio.add(targetTime -> Set(token))
      } else {
        val newSet = prio.get(targetTime).fold(Set(token))(_ + token)
        prio.add(targetTime -> newSet)
      }

      logT(s"schedule: token = $token, time = $t, old-target ${oldInfo.targetTime}, new-target = $targetTime, submit? $reschedule")

      if (reschedule) {
        val newInfo = new Info(issueTime = t, targetTime = targetTime)
        submit(newInfo)
      }

      token
    }

    def cancel(token: Token)(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      implicit val itx: I#Tx = iSys(tx)
      //      tokenMap.remove(token).fold {
      //        Console.err.println(s"Trying to cancel an unregistered token $token")
      //      } { sch =>
      tokenMap.remove(token).foreach { sch =>
        val t     = sch.time
        val set0  = prio.get(t).getOrElse(
          throw new AssertionError(s"Token $token found but no entry at $t in priority queue")
        )
        val set1  = set0 - token
        if (set1.isEmpty) {
          prio.remove(t)
          // if entry became empty, see if it was
          // scheduled; if so, re-submit
          val info = infoVar()
          if (info.targetTime == t) scheduleNext()

        } else {
          prio.add(t -> set1)
        }
      }
    }

    private def calcFrame(): Long = {
      // 1 ns = 10^-9 s
      val delta = System.nanoTime() - timeZero
      (delta * sampleRateN).toLong
    }

    /* Invoked to submit a schedule step either to a realtime scheduler or other mechanism.
     * When the step is performed, execution should be handed over to `eventReached`, passing
     * over the same three arguments.
     *
     * @param logicalNow       the logical now time at the time the event was scheduled
     * @param logicalDelay     the logical delay corresponding with the delay of the scheduled event
     *                         (the event `happens` at `logicalNow + logicalDelay`)
     * @param schedValid       the valid counter at the time of scheduling
     */
    private def submit(info: Info)(implicit tx: S#Tx): Unit = {
      implicit val ptx  = tx.peer
      infoVar()         = info
      val jitter        = calcFrame() - info.issueTime
      val actualDelayN  = math.max(0L, ((info.delay - jitter) / sampleRateN).toLong)
      logT(s"scheduled: $info; logicalDelay (f) = ${info.delay}, actualDelay (ns) = $actualDelayN")
      tx.afterCommit {
        SoundProcesses.pool.schedule(new Runnable {
          def run(): Unit = {
            logT(s"scheduled: execute $info")
            cursor.step { implicit tx =>
              eventReached(info)
            }
          }
        }, actualDelayN, TimeUnit.NANOSECONDS)
      }
    }

    /* Invoked from the `submit` body after the scheduled event is reached.
     *
     * @param logicalNow       the logical now time at the time the event was scheduled
     * @param logicalDelay     the logical delay corresponding with the delay of the scheduled event
     * @param expectedValid    the valid counter at the time of scheduling
     */
    private def eventReached(info: Info)(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx  = iSys(tx)
      implicit val ptx = tx.peer
      if (info != infoVar()) return // the scheduled task was invalidated by an intermediate stop or seek command

      // this is crucial to eliminate drift: since we reached the scheduled event, do not
      // let the timeRef txn-local determine a new free wheeling time, but set it to the
      // time we targeted at; then in the next scheduleNext call, the jitter is properly
      // calculated.
      val t = info.targetTime
      time = t

      prio.remove(t).foreach { tokens =>
        tokens.foreach { token =>
          tokenMap.remove(token).foreach { sched =>
            try {
              sched.fun(tx)
            } catch {
              case NonFatal(e) =>
                Console.err.println(s"While executing scheduled function $token:")
                e.printStackTrace()
            }
          }
        }
      }

      scheduleNext()
    }

    // looks at the smallest time on the queue. if it exists, submits to peer scheduler
    private def scheduleNext()(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx = iSys(tx)
      val headOption = prio.ceil(Long.MinValue) // headOption method missing

      headOption.fold {
        infoVar.set(infInfo)(tx.peer)   // so that subsequent `schedule` will succeed

      } { case (newTargetTime, _) =>
        val t       = time
        val newInfo = new Info(issueTime = t, targetTime = newTargetTime)
        submit(newInfo)
      }
    }
  }
}
