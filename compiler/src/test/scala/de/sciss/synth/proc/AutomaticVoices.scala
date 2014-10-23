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
import de.sciss.lucre.synth.impl.ServerImpl
import de.sciss.lucre.synth.{NodeGraph, Server, InMemory}
import de.sciss.synth.{GE, proc, SynthGraph}
import de.sciss.{osc, synth, lucre}
import de.sciss.lucre.expr.{Expr, Boolean => BooleanEx, Int => IntEx}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.swing.deferTx
import de.sciss.numbers.Implicits._
import proc.Implicits._
import SoundProcesses.atomic

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.duration.Duration
import scala.swing.Swing

object AutomaticVoices {
  val DumpOSC         = false
  val ShowLog         = false
  val PrintStates     = false
  val Shadowing       = true
  val Attack          = 30
  val Release         = 30

  val NumLayers       = 3
  val MaxVoices       = 2
  // val NumSpeakers     = 1 // 5
  val NumSpeakers     = 42
  val NumTransitions  = 4

  type S = Confluent
  // type S = InMemory

  private[this] val imp = ExprImplicits[S]
  import imp._

  def main(args: Array[String]): Unit = {
    showAuralLog = ShowLog
    ServerImpl.DEBUG_SIZE = true

    val compiler = proc.Compiler()
    println("Making action...")
    val (actionName, actionBytes) = mkAction(compiler)

    val dbc = BerkeleyDB.Config()
    dbc.lockTimeout = Duration(4, TimeUnit.SECONDS)    // this value appears to be crucial to prevent deadlocks / inf loops
    val sys: S = Confluent(BerkeleyDB.tmp(dbc))
    val (_, _cursor) = sys.cursorRoot(_ => ())(implicit tx => _ => sys.newCursor())
    implicit val cursor = _cursor
    //    val sys = InMemory()
    //    implicit val cursor = sys
    lucre.synth.expr.initTypes()
    atomic[S] { implicit tx =>
      val action = Action[S](actionName, actionBytes)
      println("Making the world...")
      val world = mkWorld(action)
      println("Making procs...")
      val (aural, transport) = mkAural(world)
      println("Making views...")
      mkViews(world, aural, transport, sys)
    }
  }

  class Speaker(val gate  : stm.Source[S#Tx, Expr    [S, Boolean]],
                val active: stm.Source[S#Tx, Expr.Var[S, Boolean]])

  class Layer(val ensemble: stm.Source[S#Tx, Ensemble.Obj[S]],
              val bypass  : stm.Source[S#Tx, Ensemble.Obj[S]],
              val speakers: Vec[Speaker],
              val playing : stm.Source[S#Tx, Expr[S, Boolean]],
              val transId : stm.Source[S#Tx, Expr.Var[S, Int]],
              val input   : stm.Source[S#Tx, Proc[S]],
              val output  : stm.Source[S#Tx, Proc[S]])

  class World(val diffusion     :     stm.Source[S#Tx, Proc.Obj[S]],
              val layers        : Vec[Layer],
              val sensors       : Vec[stm.Source[S#Tx, Expr.Var[S, Int    ]]],
              val transId       :     stm.Source[S#Tx, Expr.Var[S, Int    ]],
              val activeVoices  :     stm.Source[S#Tx, Expr    [S, Int    ]],
              val hasFreeVoices :     stm.Source[S#Tx, Expr    [S, Boolean]])

  def rrand(lo: Int, hi: Int): Int = util.Random.nextInt(hi - lo + 1) + lo

  def mkViews(w: World, aural: AuralSystem, transport: Transport[S], system: S)
             (implicit tx: S#Tx, _cursor: stm.Cursor[S]): Unit = {
    implicit val undo = new UndoManagerImpl
    val views = w.sensors.zipWithIndex.map { case (s, si) =>
      s().changed.react { implicit tx => _ =>
        checkWorld(w)
      }
      IntSpinnerView(s(), s"s$si", 64)
    }
    val transView = IntSpinnerView(w.transId     (), "trans", 64)
    val vcView    = IntSpinnerView(w.activeVoices(), "vc"   , 64)
    deferTx {
      import scala.swing._

      val butTopology = Button("Topology") {
        atomic[S] { implicit tx =>
          aural.serverOption.map { s =>
            val top = NodeGraph(s).topology
            tx.afterCommit {
              println("---- TOPOLOGY ----")
              top.edges.foreach(println)
            }
          }
        }
      }

      val butTree = Button("Tree") {
        atomic[S] { implicit tx =>
          aural.serverOption.map { s =>
            s.peer.dumpTree()
          }
        }
      }

      val butRandomize = Button("Randomize") {
        atomic[S] { implicit tx =>
          w.transId().update(rrand(0, NumTransitions - 1))
        }
        w.sensors.foreach { s =>
          atomic[S] { implicit tx =>
            s().update(rrand(-1, NumLayers - 1))
          }
        }
      }

      val butClear = Button("Clear") {
        w.sensors.foreach { s =>
          atomic[S] { implicit tx =>
            s().update(-1)
          }
        }
      }

      var battleCount = 0
      val battleTimer = new javax.swing.Timer(5000, Swing.ActionListener { _ =>
        if (battleCount > 0) {
          battleCount -= 1
        } else {
          val but = if (rrand(1, 5) == 1) {
            battleCount = 7   // enough for all to stop
            butClear
          } else {
            battleCount = rrand(0, 3)
            butRandomize
          }
          but.doClick()
        }
      })

      val butBattle = new ToggleButton("Battle Test") {
        listenTo(this)
        reactions += {
          case event.ButtonClicked(_) =>
            battleTimer.stop()
            if (selected) battleTimer.restart()
        }
      }

      new Frame {
        title = "Automatic Voices"
        contents = new BoxPanel(Orientation.Vertical) {
          // contents += new ServerStatusPanel
          contents += new GridPanel(0, math.min(6, views.size) * 2) {
            views.zipWithIndex.foreach { case (v, vi) =>
              contents += new Label(s"s$vi:", null, Alignment.Right)
              contents += v.component
            }
          }
          contents += Swing.VStrut(4)
          contents += new FlowPanel(new Label("trans:"), transView.component, new Label("vc:"), vcView.component)
          contents += Swing.VStrut(4)
          contents += new FlowPanel(
            butTopology, butTree, butRandomize, butClear, butBattle
          )
        }
        pack().centerOnScreen()
        open()

        private val checkTimer = new javax.swing.Timer(1000, Swing.ActionListener { _ =>
          atomic[S] { implicit tx =>
            checkWorld(w)
          }
        })
        checkTimer.start()

        override def closeOperation(): Unit = {
          checkTimer .stop()
          battleTimer.stop()
          atomic[S] { implicit tx =>
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

  def mkAural(w: World)(implicit tx: S#Tx, cursor: stm.Cursor[S]): (AuralSystem, Transport[S]) = {
    val aural = AuralSystem()
    if (DumpOSC) aural.whenStarted(_.peer.dumpOSC())
    val transport = Transport[S](aural)
    transport.addObject(w.diffusion())
    w.layers.foreach { l =>
      transport.addObject(l.ensemble())
      transport.addObject(l.bypass  ())
    }
    transport.play()
    val config = Server.Config()
    config.audioBusChannels  = 1024
    config.transport         = osc.TCP
    aural.start(config)
    (aural, transport)
  }

  def checkWorld(w: World)(implicit tx: S#Tx): Unit = {
    val free = w.hasFreeVoices()
    w.layers.zipWithIndex.foreach { case (l, li) =>
      if (!l.playing().value && free.value) {
        val gateVals      = l.speakers.map(s => s -> s.gate().value)
        val becomesActive = gateVals.exists(_._2)

        if (becomesActive) {
          l.transId().update(w.transId().value)
          if (Shadowing) for {
            layerIn  <- l.input ().scans.get("in" )
            layerOut <- l.output().scans.get("out")
            diffIn   <- w.diffusion().elem.peer.scans.get("in")
          } {
            val diffSources = diffIn.sources.collect {
              case l @ Scan.Link.Scan(_) => l
            } .toSet
            val layerOutL = Scan.Link.Scan(layerOut)
            // only act if we're not there
            if (!diffSources.contains(layerOutL)) {
              val existingIn = layerIn.sources.collect {
                case l @ Scan.Link.Scan(_) => l
              } .toSet
              val existingOut = layerOut.sinks.collect {
                case l @ Scan.Link.Scan(_) => l
              } .toSet
              val toAddIn     = diffSources -- existingIn
              val toRemoveIn  = existingIn  -- diffSources
              toRemoveIn .foreach(layerIn .removeSource)
              diffSources.foreach(diffIn  .removeSource)
              existingOut.foreach(layerOut.removeSink  )
              toAddIn    .foreach(layerIn .addSource   )
              diffIn.addSource(layerOutL)
            }
          }

          gateVals.collect {
            case (s, true) => s.active().update(true)
          }
        }
      }
    }
  }

  def mkAction(c: Code.Compiler): (String, Array[Byte]) = {
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
    val code  = Code.Action(source)
    val name  = "Done"
    val bytes = code.execute(name)
    println("Action compiled.")

    /* Action */ (name, bytes)
  }

  implicit class ScanOps(val `this`: Scan[S]) extends AnyVal {
    def ~> (that: Scan[S])(implicit tx: S#Tx): Unit =
      `this`.addSink(Scan.Link.Scan(that))

    def ~/> (that: Scan[S])(implicit tx: S#Tx): Unit =
      `this`.removeSink(Scan.Link.Scan(that))
  }

  def mkWorld(done: Action[S])(implicit tx: S#Tx): World = {
    val sensors = Vec.tabulate(NumSpeakers) { speaker =>
      val sensor = IntEx.newVar[S](-1)
      if (PrintStates) sensor.changed.react(_ => ch => println(s"sensor$speaker -> ${ch.now}"))
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
      val env   = Env.asr(attack = Attack, release = Release, curve = Curve.linear)
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
      val thresh  = fade.linexp(0, 1, 1.0e-3, 1.0e1)
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
      val thresh  = fade.linexp(1, 0, 1.0e-3, 1.0e1)
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

    val bypassGraph = SynthGraph {
      graph.ScanOut(graph.ScanIn())
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
        if (PrintStates) active.changed.react(_ => ch => println(s"active$li$si -> ${ch.now}"))
        active
      }
      val vecActiveObj = vecActive.map(ex => Obj(BooleanElem(ex)))

      val vecGate = Vec.tabulate(NumSpeakers) { si =>
        val isLayer = sensors(si) sig_== li
        val gate    = isLayer
        if (PrintStates) gate.changed.react(_ => ch => println(s"gate$li$si -> ${ch.now}"))
        gate
      }
      val vecGateObj = vecGate.map(ex => Obj(BooleanElem(ex)))

      val sumActive = count(vecActive)
      val lPlaying  = sumActive > 0
      if (PrintStates) lPlaying.changed.react(_ => ch => println(s"playing$li -> ${ch.now}"))

      val lFolder = Folder[S]
      val ensL    = Ensemble[S](lFolder, 0L, lPlaying)
      val ensLObj = Obj(Ensemble.Elem(ensL))

      val gen       = Proc[S]
      val genObj    = Obj(Proc.Elem(gen))
      lFolder.addLast(genObj)

      gen.graph()   = genGraph
      val liObj     = Obj(IntElem(li))
      genObj.attr.put("li", liObj)

      val pred        = Proc[S]
      pred.graph()    = bypassGraph
      pred.scans.add("in")      // layer-ensemble input from predecessor
      val predObj     = Obj(Proc.Elem(pred))
      predObj.attr.name = s"pred-$li"
      lFolder.addLast(predObj)

      val split       = Proc[S] // aka background splitter
      split.graph()   = splitGraph
      val splitObj    = Obj(Proc.Elem(split))
      splitObj.attr.name = s"split-$li"
      lFolder.addLast(splitObj)
      pred.scans.add("out") ~> split.scans.add("in")

      val succ        = Proc[S] // aka foreground splitter
      succ.graph()    = splitGraph
      val succObj     = Obj(Proc.Elem(succ))
      succObj.attr.name = s"succ-$li"
      lFolder.addLast(succObj)
      gen.scans.add("out") ~> succ.scans.add("in")

      val out         = Proc[S]
      out.graph()     = bypassGraph
      out.scans.add("out")     // layer-ensemble output to successor
      val outObj      = Obj(Proc.Elem(out))
      outObj.attr.name = s"out-$li"
      lFolder.addLast(outObj)

      val coll        = Proc[S] // aka collector
      coll.graph()    = collGraph
      val collObj     = Obj(Proc.Elem(coll))
      collObj.attr.name = s"coll-$li"
      lFolder.addLast(collObj)
      coll.scans.add("out") ~> out.scans.add("in")

      val bypassPlaying = !lPlaying
      val bypassF       = Folder[S]
      val ensBypass     = Ensemble[S](bypassF, 0L, bypassPlaying)
      val ensBypassObj  = Obj(Ensemble.Elem(ensBypass))
      val bypass        = Proc[S]
      bypass.graph()    = bypassGraph
      val bypassObj     = Obj(Proc.Elem(bypass))
      bypassObj.attr.name = s"bypass-$li"
      bypassF.addLast(bypassObj)
      pred  .scans.add("out") ~> bypass.scans.add("in")
      bypass.scans.add("out") ~> out   .scans.add("in")

      val vecTrans = transGraphs.zipWithIndex.map { case (g, gi) =>
        val tPlaying    = transId sig_== gi
        val tFolder     = Folder[S]
        val ensTrans    = Ensemble[S](tFolder, 0L, tPlaying)

        val vecChans = (vecGateObj zip vecActiveObj).zipWithIndex.map { case ((gate, active), si) =>
          val procT     = Proc[S]
          procT.graph() = g
          val predOut   = split  .scans.add(s"out-$si")
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
          val doneObj   = Obj(Action.Elem(done))
          doneObj.attr.put("active", active)
          attr.put("done", doneObj)

          procTObj
        }
        vecChans.foreach(tFolder.addLast)

        Obj(Ensemble.Elem(ensTrans))
      }

      vecTrans.foreach(lFolder.addLast)

      // short debug solution; just connect all layer outputs to main diffusion
      if (!Shadowing) coll.scans.add("out") ~> diff.scans.add("in")

      val speakers = (vecGate zip vecActive).map { case (gate, active) =>
        new Speaker(gate   = tx.newHandle(gate),
                    active = tx.newHandle(active))
      }
      new Layer(ensemble  = tx.newHandle(ensLObj),
                bypass    = tx.newHandle(ensBypassObj),
                speakers  = speakers,
                playing   = tx.newHandle(lPlaying),
                transId   = tx.newHandle(transId),
                input     = tx.newHandle(split),
                output    = tx.newHandle(coll))
    }

    val vecPlaying      = vecLayer.map(_.playing())
    val activeVoices    = count(vecPlaying)
    val hasFreeVoices   = activeVoices < MaxVoices
    if (PrintStates) activeVoices.changed.react(_ => ch => println(s"activeVoices -> ${ch.now}"))

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