package de.sciss.synth.proc

import de.sciss.lucre.stm
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.Server
import de.sciss.synth

import scala.concurrent.stm.Txn

object NewAuralTest extends App {
  type S  = Confluent
  type I  = S#I
  val sys = Confluent(BerkeleyDB.tmp())
  val (_, cursor) = sys.cursorRoot(_ => ())(implicit tx => _ => sys.newCursor())
  implicit val _cursor: stm.Cursor[S] = cursor
  showAuralLog = true
  // de.sciss.lucre.synth.showLog = true

  val as = AuralSystem()
  cursor.step { implicit tx =>
    as.whenStarted(initView)
    as.start()
  }

  def initView(s: Server): Unit = {
    s.peer.dumpOSC()
    implicit val context = cursor.step { implicit tx =>
      AuralContext[S](s)
    }
    test2()
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

  def quit()(implicit tx: S#Tx): Unit =
    tx.afterCommit {
      Thread.sleep(1000)  // have to wait a bit for scsynth to quit
      scala.sys.exit()
    }

  import synth._
  import ugen._

  def proc(graph: => Unit)(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Proc[S] = {
    val p = Proc[S]
    val g = SynthGraph {
      graph
    }
    p.graph() = SynthGraphs.newConst[S](g)

    val pObj = Obj(Proc.Elem(p))
    val _view = AuralObj.Proc(pObj)
    _view
  }

  def putDouble(proc: Proc.Obj[S], key: String, value: Double)(implicit tx: S#Tx): Unit = {
    val imp = ExprImplicits[S]
    import imp._
    proc.attr.put(key, Obj(DoubleElem(value)))
  }

  def stopAndQuit(delay: Double = 4.0): Unit =
    after(delay) { implicit tx =>
      as.stop()
      quit()
    }

  def addScan(proc: Proc.Obj[S], key: String)(implicit tx: S#Tx): Scan[S] = {
    proc.elem.peer.scans.add(key)
  }

  implicit class ScanOps(val `this`: Scan[S]) extends AnyVal {
    def ~> (that: Scan[S])(implicit tx: S#Tx): Unit =
      `this`.addSink(Scan.Link.Scan(that))
  }

  def test2()(implicit context: AuralContext[S]): Unit = {
    val (view1, view2) = cursor.step { implicit tx =>
      val _view1 = proc {
        val amp   = graph.attribute("amp").ir(0.0)
        val noise = PinkNoise.ar(Seq(amp, amp))
        graph.scan.Out("out", noise)
      }
      _view1.react { implicit tx => upd => println(s"Observed: $upd") }
      val proc1 = _view1.obj()
      putDouble(proc1, "amp", 0.5)

      val _view2 = proc {
        val freq  = graph.attribute("freq").ir(440)
        val in    = graph.scan.In("in")
        Out.ar(0, Resonz.ar(in, freq, 0.1) * 10)
      }
      val proc2 = _view2.obj()
      putDouble(proc2, "freq", 666)

      (_view1, _view2)
    }

    cursor.step { implicit tx =>
      println("--issue play--")
      view1.play()
      view2.play()
      val proc1   = view1.obj()
      val proc2   = view2.obj()
      val scanOut = addScan(proc1, "out")
      val scanIn  = addScan(proc2, "in" )
      scanOut ~> scanIn
    }

    stopAndQuit()
  }

  def test1()(implicit context: AuralContext[S]): Unit = {
    val view = cursor.step { implicit tx =>
      val _view = proc {
        Out.ar(0, PinkNoise.ar(Seq(0.5, 0.5)))
      }
      _view.react { implicit tx => upd => println(s"Observed: $upd") }
      _view
    }

    cursor.step { implicit tx =>
      println("--issue play--")
      view.play()
    }

    stopAndQuit()
  }
}
