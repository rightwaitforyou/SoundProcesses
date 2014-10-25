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
import de.sciss.synth.swing.NodeTreePanel
import de.sciss.synth.swing.j.JServerStatusPanel
import de.sciss.synth.{GE, proc, SynthGraph}
import de.sciss.{osc, synth, lucre}
import de.sciss.lucre.expr.{Expr, Boolean => BooleanEx, Int => IntEx}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.swing.{defer, deferTx}
import de.sciss.numbers.Implicits._
import proc.Implicits._
import SoundProcesses.atomic

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.duration.Duration

object AutomaticVoices {
  val DumpOSC         = false
  val ShowLog         = false
  var ShowNodeTree    = false   // warning - buggy
  val PrintStates     = true
  val Shadowing       = true
  val Attack          = 10 // 30
  val Release         = 10 // 30
  val FFTSize         = 512 // 1024

  val NumLayers       = 3 // 15
  val MaxVoices       = 2 // 3
  val NumSpeakers     = 2 // 3 // 5
  // val NumSpeakers     = 42
  val NumTransitions  = 2 // 7

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

  class Layer(val ensemble:     stm.Source[S#Tx, Ensemble.Obj[S]],
              val states  : Vec[stm.Source[S#Tx, Expr.Var[S, Int    ]]],
              val playing :     stm.Source[S#Tx, Expr.Var[S, Boolean]],
              val transId :     stm.Source[S#Tx, Expr.Var[S, Int    ]],
              val input   :     stm.Source[S#Tx, Proc.Obj[S]],
              val output  :     stm.Source[S#Tx, Proc.Obj[S]])

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
      //      s().changed.react { implicit tx => _ =>
      //        checkWorld(w)
      //      }
      IntSpinnerView(s(), s"s$si", 64)
    }
    val transView = IntSpinnerView(w.transId     (), "trans", 64)
    val vcView    = IntSpinnerView(w.activeVoices(), "vc"   , 64)

    lazy val status   = new JServerStatusPanel(JServerStatusPanel.COUNTS)
    lazy val nodeTree = new NodeTreePanel { nodeActionMenu = true }
    aural.whenStarted { s =>
      defer {
        status.server   = Some(s.peer)
        if (ShowNodeTree) nodeTree.group  = Some(s.defaultGroup.peer)
      }
    }

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
        // }
        w.sensors.foreach { s =>
        // atomic[S] { implicit tx =>
            s().update(rrand(-1, NumLayers - 1))
          }
          checkWorld(w)
        }
      }

      val butClear = Button("Clear") {
        atomic[S] { implicit tx =>
        w.sensors.foreach { s =>
          // atomic[S] { implicit tx =>
            s().update(-1)
          }
          checkWorld(w)
        }
      }

      var battleCount     = 0
      var battleWasClear  = true
      val battleTimer = new javax.swing.Timer(5000, Swing.ActionListener { _ =>
        if (battleCount > 0) {
          battleCount -= 1
        } else {
          val but = if (!battleWasClear && rrand(1, 5) == 1) {
            battleCount = (Release+4)/5 + 1   // enough for all to stop
            battleWasClear = true
            butClear
          } else {
            battleCount = rrand(0, 3)
            battleWasClear = false
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

      if (ShowNodeTree) new Frame {
        title = "Nodes"
        contents = nodeTree
        pack().open()
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
          contents += Swing.VStrut(4)
          contents += Component.wrap(status)
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
          if (ShowNodeTree) nodeTree.group = None
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
    w.layers.zipWithIndex.foreach { case (l, li) =>
      // println(s"Adding layer $li (playing = ${l.playing().value}; bypass = ${l.bypass().elem.peer.playing.value})")
      // transport.addObject(l.input   ())
      // transport.addObject(l.output  ())
      transport.addObject(l.ensemble())
      // transport.addObject(l.bypass  ())
    }
    transport.play()
    val config = Server.Config()
    config.audioBusChannels  = 4096 // 1024
    config.transport         = osc.TCP
    config.pickPort()
    aural.start(config)
    (aural, transport)
  }

  def checkWorld(w: World)(implicit tx: S#Tx): Unit = {
    val free  = w.hasFreeVoices()
    val sense: Vec[Int] = w.sensors.map(_.apply().value)
    w.layers.zipWithIndex /* .scramble() */.foreach { case (l, li) =>
      val hasFadeIn = (false /: l.states.zipWithIndex) { case (res, (stateH, si)) =>
        val state   = stateH()
        val gate    = sense(si) == li
        val before  = state().value
        val now     = before match {
          case 0 | 3 if  gate => 2
          case 1 | 2 if !gate => 3
          case _ => before
        }
        if (now != before) state() = now
        res | (now == 2)
      }

      val isActive      = l.playing().value
      val becomesActive = !isActive && free.value && hasFadeIn

      if (becomesActive) {
        l.transId().update(w.transId().value)
        if (Shadowing) layerToFront(w, l)
        l.playing().update(true)
      }
    }
  }

  def unlinkLayer(l: Layer)(implicit tx: S#Tx): Unit =
    for {
      layerIn  <- l.input    ().elem.peer.scans.get("in" )
      layerOut <- l.output   ().elem.peer.scans.get("out")
    } {
      val oldLayerIn = layerIn.sources.collect {
        case l @ Scan.Link.Scan(_) => l
      } .toSet
      val oldLayerOut = layerOut.sinks.collect {
        case l @ Scan.Link.Scan(_) => l
      } .toSet

      // disconnect old inputs
      oldLayerIn .foreach(layerIn .removeSource)
      // disconnect old outputs
      oldLayerOut.foreach(layerOut.removeSink  )
      // connect old layer inputs to old layer outputs
      oldLayerIn.foreach { in =>
        oldLayerOut.foreach { out =>
          in.peer.addSink(out)
        }
      }
    }

  def layerToFront(w: World, l: Layer)(implicit tx: S#Tx): Unit =
    for {
      layerIn  <- l.input    ().elem.peer.scans.get("in" )
      layerOut <- l.output   ().elem.peer.scans.get("out")
      diffIn   <- w.diffusion().elem.peer.scans.get("in" )
    } {
      val oldDiffIn = diffIn.sources.collect {
        case l @ Scan.Link.Scan(_) => l
      } .toSet
      val layerOutL = Scan.Link.Scan(layerOut)
      // only act if we're not there
      if (!oldDiffIn.contains(layerOutL)) {
        unlinkLayer(l)
        // disconnect old diff inputs
        oldDiffIn  .foreach(diffIn  .removeSource)
        // connect old diff inputs as new layer inputs
        oldDiffIn  .foreach(layerIn .addSource   )
        // connect layer output to diff input
        diffIn.addSource(layerOutL)
      }
    }

  def mkAction(c: Code.Compiler): (String, Array[Byte]) = {
    val source =
      """val imp = ExprImplicits[S]
        |import imp._
        |
        |for {
        |  Expr.Var(state) <- self.attr[IntElem]("state")
        |} {
        |  state.transform(x => x.value match {
        |    case 2 => 1
        |    case 3 => 0
        |    case y => y // should not occur
        |  })
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

  private lazy val transGraphs: Vec[SynthGraph] = {
    def mkTransition(fun: (GE, GE, GE) => GE): SynthGraph = SynthGraph {
      import synth._
      import ugen._
      val pred  = graph.ScanInFix("pred", 1)
      val succ  = graph.ScanInFix("succ", 1)
      val state = graph.Attribute.kr("state", 2)
      val target = 3 - state // 1 for fade-in, 0 for fade-out
      val start  = 1 - target
      val atk    = 10.0
      val rls    = 10.0
      val in     = Select.kr(ToggleFF.kr(1), Seq(start, target))
      val fade   = Slew.kr(in, atk.reciprocal, rls.reciprocal)
      val done   = fade sig_== target
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
      val bufPred = LocalBuf(FFTSize)
      val bufSucc = LocalBuf(FFTSize)
      val fltPred = IFFT.ar(PV_MagAbove(FFT(bufPred, pred), thresh))
      val fltSucc = IFFT.ar(PV_MagBelow(FFT(bufSucc, succ), thresh))
      fltSucc + fltPred
    }

    // transition 4: descending PV_MagAbove
    val t4 = mkTransition { (pred, succ, fade) =>
      import synth._
      import ugen._
      val thresh  = fade.linexp(1, 0, 1.0e-3, 1.0e1)
      val bufPred = LocalBuf(FFTSize)
      val bufSucc = LocalBuf(FFTSize)
      val fltPred = IFFT.ar(PV_MagBelow(FFT(bufPred, pred), thresh))
      val fltSucc = IFFT.ar(PV_MagAbove(FFT(bufSucc, succ), thresh))
      fltSucc + fltPred
    }

    // transition 5: to dust
    val t5 = mkTransition { (pred, succ, fade) =>
      import synth._
      import ugen._
      val f1   =   20
      val f2   = 2000

      val dustFreqS = fade.linexp(0, 1, f1, f2)
      val dustFreqP = fade.linexp(1, 0, f1, f2)

      val decayTime = 0.01
      val dustS = Decay.ar(Dust.ar(dustFreqS), decayTime).min(1)
      val dustP = Decay.ar(Dust.ar(dustFreqP), decayTime).min(1)

      val fltSucc = succ * dustS
      val fltPred = pred * dustP

      //      val fadeIn = Line.kr(0, 1, dur = 2)
      //      val sig = in * (1 - fadeIn) + mod * fadeIn

      fltSucc + fltPred
    }

    // transition 6: shift upwards
    val t6 = mkTransition { (pred, succ, fade) =>
      import synth._
      import ugen._

      val freq = fade.linexp(1, 0, 22.05, 22050) - 22.05

      val fltSucc = FreqShift.ar(LPF.ar(succ, 22050 - freq),  freq)
      val fltPred = FreqShift.ar(HPF.ar(pred, 22050 - freq), -freq)

      fltSucc + fltPred
    }

    // transition 7: shift downwards
    val t7 = mkTransition { (pred, succ, fade) =>
      import synth._
      import ugen._

      val freq = fade.linexp(0, 1, 22.05, 22050) - 22.05

      val fltSucc = FreqShift.ar(HPF.ar(succ, 22050 - freq), -freq)
      val fltPred = FreqShift.ar(LPF.ar(pred, 22050 - freq),  freq)

      fltSucc + fltPred
    }

    val transGraphs0  = Vec(t1, t2, t3, t4, t5, t6, t7)
    val res   = Vec.tabulate(NumTransitions)(i => transGraphs0(i % transGraphs0.size))
    // val transGraphs = Vec.fill(NumTransitions)(t3)
    assert(res.size == NumTransitions)
    res
  }

  import BooleanEx.{serializer => boolSer, varSerializer => boolVarSer}
  import IntEx    .{serializer => intSer , varSerializer => intVarSer }

  def mkWorld(done: Action[S])(implicit tx: S#Tx): World = {
    val sensors = Vec.tabulate(NumSpeakers) { speaker =>
      val sensor = IntEx.newVar[S](-1)
      if (PrintStates) sensor.changed.react(_ => ch => println(s"sensor$speaker -> ${ch.now}"))
      sensor
    }

    val diff = Proc[S]
    diff.graph() = SynthGraph {
      import synth._
      import ugen._
      val in = graph.ScanInFix(NumSpeakers)
      val mix = Mix.tabulate(NumSpeakers) { ch =>
        val inc = in \ ch
        val pan = if (NumSpeakers == 1) 0.0 else ch.linlin(0, NumSpeakers - 1, -1, 1)
        val sig = Pan2.ar(inc, pan)
        sig
      }
      Out.ar(0, mix)
    }
    diff.scans.add("in")
    val diffObj = Obj(Proc.Elem(diff))
    diffObj.attr.name = "diff"

    val vecLayer = Vec.tabulate(NumLayers) { li =>
      mkLayer(sensors, diff, done, li)
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

  // for simplicity, same graph for
  // all layers, distinguished by
  // resonant frequency depending on
  // attribute 0 <= `li` < NumLayers
  private lazy val genGraph = SynthGraph {
    import synth._
    import ugen._
    val li    = graph.Attribute.ir("li", 0)
    val freq  = if (NumLayers == 1) 1000.0: GE else li.linexp(0, NumLayers - 1, 200.0, 4000.0)
    val amp   = 0.5
    val dust  = Decay.ar(Dust.ar(Seq.fill(NumSpeakers)(10)), 1).min(1)
    val sig   = Resonz.ar(dust, freq, 0.5) * amp
    graph.ScanOut(sig)
  }

  // multi-channel single scan in, multiple signal-channel scan outs
  private lazy val splitGraph = SynthGraph {
    import synth._
    val in = graph.ScanInFix(NumSpeakers)
    Vec.tabulate(NumSpeakers) { ch =>
      graph.ScanOut(s"out$ch", in \ ch)
    }
  }

  // multiple signal-channel scan ins, multi-channel single scan out,
  private lazy val collGraph = SynthGraph {
    import synth._
    import ugen._
    val in = Vec.tabulate(NumSpeakers) { ch =>
      graph.ScanInFix(s"in$ch", 1)
    }
    graph.ScanOut(Flatten(in))
  }

  // simply in -> out
  private lazy val throughGraph = SynthGraph {
    graph.ScanOut(graph.ScanIn())
  }

  private lazy val switchGraph = SynthGraph {
    import synth._
    import ugen._
    val pred    = graph.ScanInFix("pred", 1)
    val succ    = graph.ScanInFix("succ", 1)
    val state   = graph.Attribute.kr("state", 2)  // 0 - bypass (pred), 1 - engage (succ)
    val sig     = Select.ar(state, Seq(pred, succ))
    graph.ScanOut(sig)
  }

  private def mkLayer(sensors: Vec[Expr[S, Int]], diff: Proc[S], done: Action[S], li: Int)
                     (implicit tx: S#Tx): Layer = {
    val transId = IntEx.newVar[S](-1) // "sampled" in `checkWorld`

    // layer-level ensemble
    val lFolder = Folder[S]

    // the actual sound layer
    val gen       = Proc[S]
    gen.graph()   = genGraph
    val genObj    = Obj(Proc.Elem(gen))
    val liObj     = Obj(IntElem(li))
    genObj.attr.put("li", liObj)
    genObj.attr.name = s"gen$li"
    lFolder.addLast(genObj)

    // layer-ensemble input from predecessor
    val pred        = Proc[S]
    pred.graph()    = throughGraph
    pred.scans.add("in")
    val predObj     = Obj(Proc.Elem(pred))
    predObj.attr.name = s"pred$li"
    lFolder.addLast(predObj)

    // aka background splitter
    val split       = Proc[S]
    split.graph()   = splitGraph
    val splitObj    = Obj(Proc.Elem(split))
    splitObj.attr.name = s"split$li"
    lFolder.addLast(splitObj)
    pred.scans.add("out") ~> split.scans.add("in")

    // aka foreground splitter
    val succ        = Proc[S]
    succ.graph()    = splitGraph
    val succObj     = Obj(Proc.Elem(succ))
    succObj.attr.name = s"succ$li"
    lFolder.addLast(succObj)
    gen.scans.add("out") ~> succ.scans.add("in")

    // aka collector
    val coll        = Proc[S]
    coll.graph()    = collGraph
    val collObj     = Obj(Proc.Elem(coll))
    collObj.attr.name = s"coll$li"
    lFolder.addLast(collObj)

    // layer-ensemble output to successor
    val out         = Proc[S]
    out.graph()     = throughGraph
    out.scans.add("out")
    val outObj      = Obj(Proc.Elem(out))
    outObj.attr.name = s"foo$li"
    lFolder.addLast(outObj)
    coll.scans.add("out") ~> out.scans.add("in")

    class Channel(val stateObj: Obj[S], val state: Expr.Var[S, Int], val fPlaying: Expr[S, Boolean],
                  val active: Expr[S, Boolean], val predOut: Scan[S], val succOut: Scan[S], val collIn: Scan[S],
                  val doneObj: Action.Obj[S])

    val vecChannels = Vec.tabulate[Channel](NumSpeakers) { si =>
      val state     = IntEx.newVar[S](0)  // 0 - bypass, 1 - engaged, 2 - fade-in, 3 - fade-out
      val stateObj  = Obj(IntElem(state))
      if (PrintStates) state.changed.react(_ => ch => println(s"state$li$si -> ${ch.now}"))
      val fPlaying  = state >= 2 // ongoing transition per channel
      if (PrintStates) fPlaying.changed.react(_ => ch => println(s"fPlaying$li$si -> ${ch.now}"))

      val predOut   = split .scans.add(s"out$si")
      val succOut   = succ  .scans.add(s"out$si")
      val collIn    = coll  .scans.add(s"in$si")

      val procB     = Proc[S]   // transition bypass/engage per channel
      procB.graph() = switchGraph
      val procBObj  = Obj(Proc.Elem(procB))
      procBObj.attr.put("state", stateObj)
      procBObj.attr.name = s"by$li$si"
      val bPlaying  = state <  2
      val bFolder   = Folder[S]
      bFolder.addLast(procBObj)
      val ensB      = Ensemble(bFolder, 0L, bPlaying)
      val ensBObj   = Obj(Ensemble.Elem(ensB))
      val predInB   = procB .scans.add("pred")
      val succInB   = procB .scans.add("succ")
      val outB      = procB .scans.add("out")
      lFolder.addLast(ensBObj)
      predOut ~> predInB
      succOut ~> succInB
      outB    ~> collIn

      val active    = state > 0

      val doneObj   = Obj(Action.Elem(done))
      doneObj.attr.put("state", stateObj)

      new Channel(stateObj = stateObj, state = state, fPlaying = fPlaying, active = active,
        predOut = predOut, succOut = succOut, collIn = collIn, doneObj = doneObj)
    }

    val activeCount = count(vecChannels.map(_.active))
    if (PrintStates) activeCount.changed.react(_ => ch => println(s"activeCount$li -> ${ch.now}"))

    val lPlaying    = BooleanEx.newVar[S](false)
    // if (PrintStates) lPlaying.changed.react(_ => ch => println(s"playing$li -> ${ch.now}"))

    val ensL    = Ensemble[S](lFolder, 0L, lPlaying)
    val ensLObj = Obj(Ensemble.Elem(ensL))

    //    val bypassPlaying = !lPlaying
    //    val bypassF       = Folder[S]
    //    val ensBypass     = Ensemble[S](bypassF, 0L, bypassPlaying)
    //    val ensBypassObj  = Obj(Ensemble.Elem(ensBypass))
    //    val bypass        = Proc[S]
    //    bypass.graph()    = throughGraph
    //    val bypassObj     = Obj(Proc.Elem(bypass))
    //    bypassObj.attr.name = s"bypass$li"
    //    bypassF.addLast(bypassObj)
    //    pred  .scans.add("out") ~> bypass.scans.add("in")
    //    bypass.scans.add("out") ~> out   .scans.add("in")

    transGraphs.zipWithIndex.foreach { case (g, gi) =>
      val tPlaying    = transId sig_== gi
      val tFolder     = Folder[S]
      val ensT        = Ensemble[S](tFolder, 0L, tPlaying)
      val ensTObj     = Obj(Ensemble.Elem(ensT))
      lFolder.addLast(ensTObj)

      vecChannels.zipWithIndex.foreach { case (channel, si) =>
        val fFolder   = Folder[S]
        val ensF      = Ensemble(fFolder, 0L, channel.fPlaying)
        tFolder.addLast(Obj(Ensemble.Elem(ensF)))

        val procT     = Proc[S]
        procT.graph() = g
        val predInT   = procT.scans.add("pred")
        val succInT   = procT.scans.add("succ")
        val outT      = procT.scans.add("out")

        channel.predOut ~> predInT
        channel.succOut ~> succInT
        outT            ~> channel.collIn

        val procTObj  = Obj(Proc.Elem(procT))
        val attr      = procTObj.attr
        attr.name     = s"t$gi$si"
        attr.put("state", channel.stateObj)
        attr.put("done" , channel.doneObj )

        fFolder.addLast(procTObj)
      }
    }

    // short debug solution; just connect all layer outputs to main diffusion
    if (!Shadowing) coll.scans.add("out") ~> diff.scans.add("in")

    val states = vecChannels.map { channel => tx.newHandle(channel.state) }
    val l = new Layer(
              ensemble  = tx.newHandle(ensLObj),
              states    = states,
              playing   = tx.newHandle(lPlaying),
              transId   = tx.newHandle(transId),
              input     = tx.newHandle(predObj),
              output    = tx.newHandle(outObj))
    activeCount.changed.react { implicit tx => ch =>
      if (ch.now == 0) {
        l.playing().update(false)
        unlinkLayer(l)
      }
    }
    l
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