/*
 *  AutomaticVoices.scala
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

package de.sciss.synth.proc

import java.util.concurrent.TimeUnit

import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.lucre.swing.IntSpinnerView
import de.sciss.lucre.synth.{Server, InMemory}
import de.sciss.synth.{GE, proc, SynthGraph}
import de.sciss.{synth, lucre}
import de.sciss.lucre.expr.{Expr, Boolean => BooleanEx, Int => IntEx}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.swing.deferTx
import de.sciss.numbers.Implicits._
import proc.Implicits._

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.duration.Duration
import scala.swing.Swing

object AutomaticVoices {
  val DumpOSC         = false
  val ShowLog         = false

  val NumLayers       = 3
  val MaxVoices       = 2
  val NumSpeakers     = 5
  // val NumSpeakers     = 42
  val NumTransitions  = 4

  type S = Confluent
  // type S = InMemory

  private[this] val imp = ExprImplicits[S]
  import imp._

  def main(args: Array[String]): Unit = {
    showAuralLog = ShowLog

    val dbc = BerkeleyDB.Config()
    dbc.lockTimeout = Duration(2, TimeUnit.SECONDS)    // this value appears to be crucial to prevent deadlocks / inf loops
    val sys: S = Confluent(BerkeleyDB.tmp(dbc))
    val (_, _cursor) = sys.cursorRoot(_ => ())(implicit tx => _ => sys.newCursor())
    implicit val cursor = _cursor
    //    val sys = InMemory()
    //    implicit val cursor = sys
    lucre.synth.expr.initTypes()
    val compiler = proc.Compiler()
    atomic { implicit tx =>
      println("Making the world...")
      val world = mkWorld()
      println("Making action...")
      mkAction(world, compiler)
      println("Making procs...")
      val transport = mkAural(world)
      println("Making views...")
      mkViews(world, transport, sys)
    }
  }

  class Speaker(val gate  : stm.Source[S#Tx, Expr    [S, Boolean]],
                val active: stm.Source[S#Tx, Expr.Var[S, Boolean]])

  class Layer(val ensemble: stm.Source[S#Tx, Ensemble.Obj[S]],
              val speakers: Vec[Speaker],
              val playing : stm.Source[S#Tx, Expr[S, Boolean]],
              val transId : stm.Source[S#Tx, Expr.Var[S, Int]],
              val input   : stm.Source[S#Tx, Proc[S]],
              val output  : stm.Source[S#Tx, Proc[S]])

  class World(val diffusion     :     stm.Source[S#Tx, Proc.Obj[S]],
              val layers        : Vec[Layer],
              val sensors       : Vec[stm.Source[S#Tx, Expr.Var[S, Int    ]]],
              val transId       :     stm.Source[S#Tx, Expr    [S, Int    ]],
              val activeVoices  :     stm.Source[S#Tx, Expr    [S, Int    ]],
              val hasFreeVoices :     stm.Source[S#Tx, Expr    [S, Boolean]])

  def mkViews(w: World, transport: Transport[S], system: S)(implicit tx: S#Tx, _cursor: stm.Cursor[S]): Unit = {
    implicit val undo = new UndoManagerImpl
    val views = w.sensors.zipWithIndex.map { case (s, si) =>
      s().changed.react { implicit tx => _ =>
        checkWorld(w)
      }
      IntSpinnerView(s(), s"s$si", 64)
    }
    val transView = IntSpinnerView(w.transId(), "trans", 64)
    val vcView    = IntSpinnerView(w.activeVoices(), "vc", 64)
    deferTx {
      import scala.swing._
      new Frame {
        title = "Automatic Voices"
        contents = new BoxPanel(Orientation.Vertical) {
          // contents += new ServerStatusPanel
          contents += new GridPanel(0, 12) {
            views.zipWithIndex.foreach { case (v, vi) =>
              contents += new Label(s"s$vi:", null, Alignment.Right)
              contents += v.component
            }
          }
          contents += Swing.VStrut(4)
          contents += new FlowPanel(new Label("trans:"), transView.component, new Label("vc:"), vcView.component)
        }
        pack().centerOnScreen()
        open()

        private val timer = new javax.swing.Timer(1000, Swing.ActionListener { _ =>
          atomic { implicit tx =>
            checkWorld(w)
          }
        })
        timer.start()

        override def closeOperation(): Unit = {
          timer.stop()
          atomic { implicit tx =>
            transport.dispose()
            tx.afterCommit {
              system.close()
              sys.exit(0)
            }
          }
        }
      }
    }
  }

  def mkAural(w: World)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Transport[S] = {
    val aural = AuralSystem()
    if (DumpOSC) aural.whenStarted(_.peer.dumpOSC())
    val transport = Transport[S](aural)
    transport.addObject(w.diffusion())
    w.layers.foreach { l => transport.addObject(l.ensemble()) }
    transport.play()
    val cfg = Server.Config()
    cfg.audioBusChannels = 1024
    aural.start(cfg)
    transport
  }

  def checkWorld(w: World)(implicit tx: S#Tx): Unit = {
    val free = w.hasFreeVoices()
    w.layers.zipWithIndex.foreach { case (l, li) =>
      if (!l.playing().value && free.value) {
        val gateVals      = l.speakers.map(s => s -> s.gate().value)
        val becomesActive = gateVals.exists(_._2)

        if (becomesActive) {
          l.transId().update(w.transId().value)
          // XXX TODO - here it would come to the front

          gateVals.collect {
            case (s, true) => s.active().update(true)
          }
        }
      }
    }
  }

  def atomic(fun: S#Tx => Unit)(implicit cursor: stm.Cursor[S]): Unit =
    SoundProcesses.executionContext.execute(Swing.Runnable {
      cursor.step { implicit tx =>
        fun(tx)
      }
    })

  def mkAction(w: World, c: Code.Compiler)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Unit = {
    val source =
      """val imp = ExprImplicits[S]
        |import imp._
        |// println("Action")
        |
        |self.attr[BooleanElem]("active").foreach {
        |  case Expr.Var(active) => active() = false
        |}
        |""".stripMargin
    implicit val compiler = c
    import compiler.executionContext
    val fut = Action.compile[S](Code.Action(source))
    fut.foreach { actionH =>
      println("Action compiled.")
      atomic { implicit tx =>
        val action = actionH()
        w.layers.foreach { l =>
          l.speakers.foreach { s =>
            val activeObj = Obj(BooleanElem(s.active()))
            val actionObj = Obj(Action.Elem(action))
            actionObj.attr.put("active", activeObj)
            println("TODO - STORE DONE ACTION") // s.proc() .attr.put("done"  , actionObj)
          }
        }
      }
    }
  }

  implicit class ScanOps(val `this`: Scan[S]) extends AnyVal {
    def ~> (that: Scan[S])(implicit tx: S#Tx): Unit =
      `this`.addSink(Scan.Link.Scan(that))

    def ~/> (that: Scan[S])(implicit tx: S#Tx): Unit =
      `this`.removeSink(Scan.Link.Scan(that))
  }

  def mkWorld()(implicit tx: S#Tx): World = {
    val sensors = Vec.tabulate(NumSpeakers) { speaker =>
      val sensor = IntEx.newVar[S](-1)
      sensor.changed.react(_ => ch => println(s"sensor$speaker -> ${ch.now}"))
      sensor
    }

    import BooleanEx.{serializer => boolSer, varSerializer => boolVarSer}
    import IntEx    .{serializer => intSer , varSerializer => intVarSer }

    // for simplicity, same graph for
    // all layers, distinguished by
    // resonant frequency depending on
    // attribute 0 <= `li` < NumLayers
    val genGraph = SynthGraph {
      import synth._
      import ugen._
      val li    = graph.Attribute.ir("li", 0)
      val freq  = li.linexp(0, NumLayers - 1, 300.0, 2000.0)
      val amp   = 0.5
      val dust  = Decay.ar(Dust.ar(Seq.fill(NumSpeakers)(10)), 1).min(1)
      val sig   = Resonz.ar(dust, freq, 0.5) * amp
      graph.ScanOut(sig)
    }

    def mkTransition(fun: (GE, GE, GE) => GE): SynthGraph = SynthGraph {
      import synth._
      import ugen._
      val pred  = graph.ScanInFix("pred", 1)
      val succ  = graph.ScanInFix("succ", 1)
      val gate  = graph.Attribute.kr("gate", 0)
      val env   = Env.asr(attack = 10, release = 10, curve = Curve.linear)
      val fade  = EnvGen.ar(env, gate = gate)
      val done  = Done.kr(fade)
      graph.Action(done, "done")
      val sig   = fun(pred, succ, fade)
      graph.ScanOut(sig)
    }

    // transition 1: rising LPF
    val t1 = mkTransition { (pred, succ, fade) =>
      import synth._
      import ugen._
      val freq = fade.linexp(0, 1, 22.05, 22050)
      HPF.ar(pred, freq) + LPF.ar(succ, freq)
    }

    // transition 2: descending HPF
    val t2 = mkTransition { (pred, succ, fade) =>
      import synth._
      import ugen._
      val freq = fade.linexp(1, 0, 22.05, 22050)
      HPF.ar(succ, freq) + LPF.ar(pred, freq)
    }

    // transition 3: rising PV_MagBelow
    val t3 = mkTransition { (pred, succ, fade) =>
      import synth._
      import ugen._
      val thresh  = fade.linexp(0, 1, 1.0e-3, 1.0e-1)
      val bufPred = LocalBuf(1024)
      val bufSucc = LocalBuf(1024)
      val fltPred = IFFT.ar(PV_MagAbove(FFT(bufPred, pred), thresh))
      val fltSucc = IFFT.ar(PV_MagBelow(FFT(bufSucc, succ), thresh))
      fltSucc + fltPred
    }

    // transition 4: descending PV_MagAbove
    val t4 = mkTransition { (pred, succ, fade) =>
      import synth._
      import ugen._
      val thresh  = fade.linexp(1, 0, 1.0e-3, 1.0e-1)
      val bufPred = LocalBuf(1024)
      val bufSucc = LocalBuf(1024)
      val fltPred = IFFT.ar(PV_MagBelow(FFT(bufPred, pred), thresh))
      val fltSucc = IFFT.ar(PV_MagAbove(FFT(bufSucc, succ), thresh))
      fltSucc + fltPred
    }

    val transGraphs = Vec(t1, t2, t3, t4)
    assert(transGraphs.size == NumTransitions)

    // multi-channel single scan in, multiple signal-channel scan outs
    val splitGraph = SynthGraph {
      import synth._
      val in = graph.ScanInFix(NumSpeakers)
      Vec.tabulate(NumSpeakers) { ch =>
        graph.ScanOut(s"out-$ch", in \ ch)
      }
    }

    // multiple signal-channel scan ins, multi-channel single scan out,
    val collGraph = SynthGraph {
      import synth._
      import ugen._
      val in = Vec.tabulate(NumSpeakers) { ch =>
        graph.ScanInFix(s"in-$ch", 1)
      }
      graph.ScanOut(Flatten(in))
    }

    val diff = Proc[S]
    diff.graph() = SynthGraph {
      import synth._
      import ugen._
      val in = graph.ScanInFix(NumSpeakers)
      val mix = Mix.tabulate(NumSpeakers) { ch =>
        val inc = in \ ch
        val pan = ch.linlin(0, NumSpeakers - 1, -1, 1)
        val sig = Pan2.ar(inc, pan)
        sig
      }
      Out.ar(0, mix)
    }
    diff.scans.add("in")
    val diffObj = Obj(Proc.Elem(diff))
    diffObj.attr.name = "diff"

    val vecLayer = Vec.tabulate(NumLayers) { li =>
      val transId = IntEx.newVar[S](-1) // "sampled" in `checkWorld`

      val vecActive = Vec.tabulate(NumSpeakers) { si =>
        val active = BooleanEx.newVar[S](false)
        active.changed.react(_ => ch => println(s"active$li$si -> ${ch.now}"))
        active
      }
      val vecActiveObj = vecActive.map(ex => Obj(BooleanElem(ex)))

      val vecGate = Vec.tabulate(NumSpeakers) { si =>
        val isLayer = sensors(si) sig_== li
        val gate    = isLayer
        // println(s"gate$li$si: $gate")
        gate.changed.react(_ => ch => println(s"gate$li$si -> ${ch.now}"))
        gate
      }
      val vecGateObj = vecGate.map(ex => Obj(BooleanElem(ex)))

      val sumActive = count(vecActive)
      val lPlaying  = sumActive > 0
      lPlaying.changed.react(_ => ch => println(s"playing$li -> ${ch.now}"))

      val lFolder = Folder[S]
      val ensL    = Ensemble[S](lFolder, 0L, lPlaying)
      val ensLObj = Obj(Ensemble.Elem(ensL))

      val gen       = Proc[S]
      val genObj    = Obj(Proc.Elem(gen))
      lFolder.addHead(genObj)

      gen.graph()   = genGraph
      val liObj     = Obj(IntElem(li))
      genObj.attr.put("li", liObj)

      val pred        = Proc[S] // aka background splitter
      pred.graph()    = splitGraph
      pred.scans.add("in")      // layer-ensemble input from predecessor
      val predObj     = Obj(Proc.Elem(pred))
      predObj.attr.name = s"pred-$li"
      lFolder.addLast(predObj)
      val succ        = Proc[S] // aka foreground splitter
      succ.graph()    = splitGraph
      val succObj     = Obj(Proc.Elem(succ))
      succObj.attr.name = s"succ-$li"
      lFolder.addLast(succObj)
      val coll        = Proc[S] // aka collector
      coll.graph()    = collGraph
      coll.scans.add("out")     // layer-ensemble output to successor
      val collObj     = Obj(Proc.Elem(coll))
      collObj.attr.name = s"coll-$li"
      lFolder.addLast(collObj)

      gen.scans.add("out") ~> succ.scans.add("in")

      val vecTrans = transGraphs.zipWithIndex.map { case (g, gi) =>
        val tPlaying    = transId sig_== gi
        val tFolder     = Folder[S]
        val ensTrans    = Ensemble[S](tFolder, 0L, tPlaying)

        val vecChans = (vecGateObj zip vecActiveObj).zipWithIndex.map { case ((gate, active), si) =>
          val procT     = Proc[S]
          procT.graph() = g
          val predOut   = pred  .scans.add(s"out-$si")
          val succOut   = succ  .scans.add(s"out-$si")
          val predIn    = procT .scans.add("pred")
          val succIn    = procT .scans.add("succ")
          val tOut      = procT .scans.add("out")
          val collIn    = coll  .scans.add(s"in-$si")
          predOut ~> predIn
          succOut ~> succIn
          tOut    ~> collIn

          val procTObj  = Obj(Proc.Elem(procT))
          val attr      = procTObj.attr
          attr.name     = s"T$gi$si"
          attr.put("gate", gate)
          // attr.put("done", ...) // XXX TODO - based on `active`

          procTObj
        }
        vecChans.foreach(tFolder.addLast)

        Obj(Ensemble.Elem(ensTrans))
      }

      vecTrans.foreach(lFolder.addLast)

      // XXX TODO - short debug solution; just connect all layer outputs to main diffusion
      coll.scans.add("out") ~> diff.scans.add("in")

      val speakers = (vecGate zip vecActive).map { case (gate, active) =>
        new Speaker(gate   = tx.newHandle(gate),
                    active = tx.newHandle(active))
      }
      new Layer(ensemble  = tx.newHandle(ensLObj),
                speakers  = speakers,
                playing   = tx.newHandle(lPlaying),
                transId   = tx.newHandle(transId),
                input     = tx.newHandle(pred),
                output    = tx.newHandle(coll))
    }

    val vecPlaying      = vecLayer.map(_.playing())
    val activeVoices    = count(vecPlaying)
    val hasFreeVoices   = activeVoices < MaxVoices
    activeVoices.changed.react(_ => ch => println(s"activeVoices -> ${ch.now}"))

    val wTransId = IntEx.newVar[S](0)

    new World(diffusion     = tx.newHandle(diffObj),
              layers        = vecLayer,
              sensors       = sensors.map(tx.newHandle(_)),
              transId       = tx.newHandle(wTransId),
              activeVoices  = tx.newHandle(activeVoices),
              hasFreeVoices = tx.newHandle(hasFreeVoices))
  }

  private def count(in: Vec[Expr[S, Boolean]])(implicit tx: S#Tx): Expr[S, Int] = {
    val imp = ExprImplicits[S]
    import imp._
    reduce(in.map(_.toInt))(_ + _)
  }

  // like Vec.reduce, but splitting at the half,
  // thus allowing the composition of bin-ops with guaranteed
  // tree depth of ld(N)
  private def reduce[A](in: Vec[A])(op: (A, A) => A): A = {
    val sz = in.size
    if (sz <= 1) in.head else {
      val (front, back) = in.splitAt(sz >> 1)
      val a = reduce(front)(op)
      val b = reduce(back )(op)
      op(a, b)
    }
  }
}