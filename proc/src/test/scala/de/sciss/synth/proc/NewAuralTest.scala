package de.sciss.synth.proc

import de.sciss.lucre.stm
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.Server
import de.sciss.span.Span
import de.sciss.synth
import de.sciss.synth.SynthGraph

import scala.concurrent.stm.Txn

object NewAuralTest extends App {
  type S  = Confluent
  type I  = S#I
  val sys = Confluent(BerkeleyDB.tmp())
  val (_, cursor) = sys.cursorRoot(_ => ())(implicit tx => _ => sys.newCursor())
  implicit val _cursor: stm.Cursor[S] = cursor

  val as = AuralSystem()
  cursor.step { implicit tx =>
    as.whenStarted(initView)
    as.start()
  }

  def after(secs: Double)(code: S#Tx => Unit): Unit = {
    val t = new Thread {
      override def run(): Unit = {
        Thread.sleep((secs * 1000).toLong)
        cursor.step { implicit tx =>
          code(tx)
        }
      }
    }
    Txn.findCurrent.fold(t.start()) { implicit tx =>
      Txn.afterCommit(_ => t.start())
    }
  }

  def initView(s: Server): Unit = {
    s.peer.dumpOSC()

    val view = cursor.step { implicit tx =>
      val p = Proc[S]
      val g = SynthGraph {
        import synth._
        import ugen._
        Out.ar(0, PinkNoise.ar(Seq(0.5, 0.5)))
      }
      p.graph() = SynthGraphs.newConst[S](g)

      implicit val context = AuralContext[S](s)

      val pObj = Obj(Proc.Elem(p))
      val _view = AuralObj(pObj)

      _view.react { implicit tx => upd => println(s"Observed: $upd") }
      _view
    }

    cursor.step { implicit tx =>
      println("--issue play--")
      view.play()
    }

    after(4.0) { implicit tx =>
      as.stop()
      tx.afterCommit {
        Thread.sleep(1000)  // have to wait a bit for scsynth to quit
        scala.sys.exit()
      }
    }
  }
}
