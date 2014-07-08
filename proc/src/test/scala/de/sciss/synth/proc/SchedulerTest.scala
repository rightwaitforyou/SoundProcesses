package de.sciss.synth.proc

import java.util.concurrent.{TimeUnit, Executors}
import de.sciss.lucre.synth.InMemory

import concurrent.stm.{TxnLocal, Txn => ScalaTxn, TxnExecutor, InTxn, Ref => STMRef}

object SchedulerTest extends App {
  type S = InMemory
  implicit val cursor = InMemory()
  showTransportLog = true

  def frames(secs: Double) = (Timeline.SampleRate * secs).toLong

  cursor.step { implicit tx =>
    val sched = Scheduler[S]
    val now   = sched.time
    val token2 = sched.schedule(now + frames(2.0)) { implicit tx =>
      println("After 2 seconds - should have been cancelled")
    }
    sched.schedule(now + frames(4.0)) { implicit tx =>
      println("After 4 seconds")
    }
    sched.schedule(now + frames(1.0)) { implicit tx =>
      println("After 1 seconds")
      val now1 = sched.time
      println(s"Logically ${now1 - now} frames elapsed.")
      sched.schedule(now1 + frames(2.0)) { implicit tx =>
        println(s"After 1+2 seconds (txn hash ${tx.hashCode().toHexString})")
        val now2 = sched.time
        println(s"Logically ${now2 - now1} frames elapsed.")
      }
    }
    sched.schedule(now + frames(3.0)) { implicit tx =>
      println(s"After 3   seconds (txn hash ${tx.hashCode().toHexString})")
    }
    sched.cancel(token2)
  }

  new Thread {
    override def run(): Unit = {
      Thread.sleep(5000)
      println("Terminating.")
    }
    start()
  }
}
