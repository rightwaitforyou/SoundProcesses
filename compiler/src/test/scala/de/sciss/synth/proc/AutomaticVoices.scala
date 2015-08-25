/*
 *  AutomaticVoices.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.lucre.expr.{BooleanObj, IntObj}
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.swing.{IntSpinnerView, defer, deferTx}
import de.sciss.lucre.synth.Server
import de.sciss.lucre.synth.impl.ServerImpl
import de.sciss.lucre.{expr, stm}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.SoundProcesses.atomic
import de.sciss.synth.swing.NodeTreePanel
import de.sciss.synth.swing.j.JServerStatusPanel
import de.sciss.synth.{GE, SynthGraph, proc}
import de.sciss.{osc, synth}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.duration.Duration

object AutomaticVoices {
  val DumpOSC         = false
  val ShowLog         = false
  var ShowNodeTree    = false   // warning - buggy
  val PrintStates     = false
  val Shadowing       = true
  val Attack          = 30  // 10
  val Release         = 30  // 10
  val FFTSize         = 512 // 1024

  val NumLayers       = 15  // 3
  val MaxVoices       = 3 // 2
  val NumSpeakers     = 42  // 2
  val NumTransitions  = 7   // 2

  type S = Confluent
  // type S = InMemory

  import expr.Ops._

  def main(args: Array[String]): Unit = {
    showAuralLog = ShowLog
    ServerImpl.VERIFY_BUNDLE_SIZE = true

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
    // lucre.synth.expr.initTypes()

    atomic[S, Unit] { implicit tx =>
      val action = Action[S](actionName, actionBytes)
      println("Making the world...")
      val world = mkWorld(action)
      println("Making procs...")
      import WorkspaceHandle.Implicits._
      val (aural, transport) = mkAural(world)
      println("Making views...")
      mkViews(world, aural, transport, sys)
    }
  }

  class Layer(val ensemble:     stm.Source[S#Tx, Ensemble[S]],
              val states  : Vec[stm.Source[S#Tx, IntObj.Var[S]]],
              val playing :     stm.Source[S#Tx, BooleanObj.Var[S]],
              val transId :     stm.Source[S#Tx, IntObj.Var[S]],
              val input   :     stm.Source[S#Tx, Proc[S]],
              val output  :     stm.Source[S#Tx, Proc[S]])

  class World(val diffusion     :     stm.Source[S#Tx, Proc[S]],
              val layers        : Vec[Layer],
              val sensors       : Vec[stm.Source[S#Tx, IntObj.Var[S]]],
              val transId       :     stm.Source[S#Tx, IntObj.Var[S]],
              val activeVoices  :     stm.Source[S#Tx, IntObj[S]],
              val hasFreeVoices :     stm.Source[S#Tx, BooleanObj[S]])

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
        atomic[S, Unit] { implicit tx =>
          aural.serverOption.foreach { s =>
            val top = s.topology
            tx.afterCommit {
              println("---- TOPOLOGY ----")
              top.edges.foreach(println)
            }
          }
        }
      }

      val butTree = Button("Tree") {
        atomic[S, Unit] { implicit tx =>
          aural.serverOption.foreach { s =>
            s.peer.dumpTree()
          }
        }
      }

      val butRandomize = Button("Randomize") {
        atomic[S, Unit] { implicit tx =>
          w.transId().update(rrand(0, NumTransitions - 1))
        // }
        w.sensors.foreach { s =>
        // atomic[S] { implicit tx =>
            s().update(rrand(-1, NumLayers - 1))
          }
          checkWorld(w, debug = true)
        }
      }

      val butClear = Button("Clear") {
        atomic[S, Unit] { implicit tx =>
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
          atomic[S, Unit] { implicit tx =>
            checkWorld(w)
          }
        })
        checkTimer.start()

        override def closeOperation(): Unit = {
          checkTimer .stop()
          battleTimer.stop()
          if (ShowNodeTree) nodeTree.group = None
          atomic[S, Unit] { implicit tx =>
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

  def mkAural(w: World)(implicit tx: S#Tx, cursor: stm.Cursor[S], workspace: WorkspaceHandle[S]): (AuralSystem, Transport[S]) = {
    val aural = AuralSystem()
    if (DumpOSC) aural.whenStarted(_.peer.dumpOSC())
    val transport = Transport[S](aural)
    transport.addObject(w.diffusion())
    w.layers.zipWithIndex.foreach { case (l, li) =>
      // println(s"Adding layer $li (playing = ${l.playing().value}; bypass = ${l.bypass().playing.value})")
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

  def checkWorld(w: World, debug: Boolean = false)(implicit tx: S#Tx): Unit = {
    val free  = w.hasFreeVoices()
    val sense: Vec[Int] = w.sensors.map(_.apply().value)
    w.layers.zipWithIndex /* .scramble() */.foreach { case (l, li) =>
      if (PrintStates && debug) {
        println(s"----- LAYER $li -----")
      }

      val isActive        = l.playing().value
      val mayBecomeActive = !isActive && free.value

      val hasFadeIn = (isActive || mayBecomeActive) && (false /: l.states.zipWithIndex) { case (res, (stateH, si)) =>
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

      if (PrintStates && debug) {
        println(l.states.zipWithIndex.map { case (s, si) => f"s$si%02d = ${s.apply().value}" } .mkString(", "))
      }

      val becomesActive = mayBecomeActive && hasFadeIn

      if (becomesActive) {
        if (PrintStates) println(s"Layer $li becomes active.")
        l.transId().update(w.transId().value)
        if (Shadowing) layerToFront(w, l)
        l.playing().update(true)
      }
    }
  }

  def unlinkLayer(l: Layer)(implicit tx: S#Tx): Unit =
    for {
      layerIn  <- l.input    ().inputs .get("in" )
      layerOut <- l.output   ().outputs.get("out")
    } {
      val oldLayerIn = layerIn.iterator.collect {
        case l @ Scan.Link.Scan(_) => l
      } .toSet
      val oldLayerOut = layerOut.iterator.collect {
        case l @ Scan.Link.Scan(_) => l
      } .toSet

      // disconnect old inputs
      oldLayerIn .foreach(layerIn .remove)
      // disconnect old outputs
      oldLayerOut.foreach(layerOut.remove)
      // connect old layer inputs to old layer outputs
      oldLayerIn.foreach { in =>
        oldLayerOut.foreach { out =>
          in.peer.add(out)
        }
      }
    }

  def layerToFront(w: World, l: Layer)(implicit tx: S#Tx): Unit =
    for {
      layerIn  <- l.input    ().inputs .get("in" )
      layerOut <- l.output   ().outputs.get("out")
      diffIn   <- w.diffusion().inputs .get("in" )
    } {
      val oldDiffIn = diffIn.iterator.collect {
        case l @ Scan.Link.Scan(_) => l
      } .toSet
      val layerOutL = Scan.Link.Scan(layerOut)
      // only act if we're not there
      if (!oldDiffIn.contains(layerOutL)) {
        unlinkLayer(l)
        // disconnect old diff inputs
        oldDiffIn  .foreach(diffIn  .remove)
        // connect old diff inputs as new layer inputs
        oldDiffIn  .foreach(layerIn .add   )
        // connect layer output to diff input
        diffIn.add(layerOutL)
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
    val code  = Code.Action(source)
    val name  = "Done"
    val bytes = code.execute(name)
    println("Action compiled.")

    /* Action */ (name, bytes)
  }

  implicit class ScanOps(val `this`: Scan[S]) extends AnyVal {
    def ~> (that: Scan[S])(implicit tx: S#Tx): Unit =
      `this`.add(Scan.Link.Scan(that))

    def ~/> (that: Scan[S])(implicit tx: S#Tx): Unit =
      `this`.remove(Scan.Link.Scan(that))
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
    // val res = Vec.fill(NumTransitions)(t1)
    assert(res.size == NumTransitions)
    res
  }

  def mkWorld(done: Action[S])(implicit tx: S#Tx): World = {
    val sensors = Vec.tabulate(NumSpeakers) { speaker =>
      val sensor = IntObj.newVar[S](-1)
      if (PrintStates) sensor.changed.react(_ => ch => println(s"sensor$speaker -> ${ch.now}"))
      sensor
    }

//    import ExprImplicits._

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
    diff.inputs.add("in")
    val diffObj = diff // Obj(Proc.Elem(diff))
    diffObj.name = "diff"

    val vecLayer = Vec.tabulate(NumLayers) { li =>
      mkLayer(sensors, diff, done, li)
    }

    val vecPlaying      = vecLayer.map(_.playing())
    val activeVoices    = count(vecPlaying)
    val hasFreeVoices   = activeVoices < MaxVoices
    if (PrintStates) activeVoices.changed.react(_ => ch => println(s"activeVoices -> ${ch.now}"))

    val wTransId = IntObj.newVar[S](0)

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

  private def mkLayer(sensors: Vec[IntObj[S]], diff: Proc[S], done: Action[S], li: Int)
                     (implicit tx: S#Tx): Layer = {
    val transId = IntObj.newVar[S](-1) // "sampled" in `checkWorld`

    // layer-level ensemble
    val lFolder = Folder[S]

//    import ExprImplicits._

    // the actual sound layer
    val gen       = Proc[S]
    gen.graph()   = genGraph
    val genObj    = gen // Obj(Proc.Elem(gen))
    val liObj     = li: IntObj[S]
    genObj.attr.put("li", liObj)
    genObj.name = s"gen$li"
    lFolder.addLast(genObj)

    // layer-ensemble input from predecessor
    val pred        = Proc[S]
    pred.graph()    = throughGraph
    pred.inputs.add("in")
    val predObj     = pred // Obj(Proc.Elem(pred))
    predObj.name = s"pred$li"
    lFolder.addLast(predObj)

    // aka background splitter
    val split       = Proc[S]
    split.graph()   = splitGraph
    val splitObj    = split // Obj(Proc.Elem(split))
    splitObj.name = s"split$li"
    lFolder.addLast(splitObj)
    pred.outputs.add("out") ~> split.inputs.add("in")

    // aka foreground splitter
    val succ        = Proc[S]
    succ.graph()    = splitGraph
    val succObj     = succ // Obj(Proc.Elem(succ))
    succObj.name = s"succ$li"
    lFolder.addLast(succObj)
    gen.outputs.add("out") ~> succ.inputs.add("in")

    // aka collector
    val coll        = Proc[S]
    coll.graph()    = collGraph
    val collObj     = coll // Obj(Proc.Elem(coll))
    collObj.name = s"coll$li"
    lFolder.addLast(collObj)

    // layer-ensemble output to successor
    val out       = Proc[S]
    out.graph()   = throughGraph
    out.outputs.add("out")
    val outObj    = out // Obj(Proc.Elem(out))
    outObj.name   = s"foo$li"
    lFolder.addLast(outObj)
    coll.outputs.add("out") ~> out.inputs.add("in")

    class Channel(val stateObj: Obj[S], val state: IntObj.Var[S], val fPlaying: BooleanObj[S],
                  val active: BooleanObj[S], val predOut: Scan[S], val succOut: Scan[S], val collIn: Scan[S],
                  val doneObj: Action[S])

    val vecChannels = Vec.tabulate[Channel](NumSpeakers) { si =>
      val state     = IntObj.newVar[S](0)  // 0 - bypass, 1 - engaged, 2 - fade-in, 3 - fade-out
      val stateObj  = state // Obj(IntElem(state))
      if (PrintStates) state.changed.react(_ => ch => println(s"state${li}_$si -> ${ch.now}"))
      val fPlaying  = state >= 2 // ongoing transition per channel
      if (PrintStates) fPlaying.changed.react(_ => ch => println(s"fPlaying${li}_$si -> ${ch.now}"))

      val predOut   = split .outputs.add(s"out$si")
      val succOut   = succ  .outputs.add(s"out$si")
      val collIn    = coll  .inputs .add(s"in$si")

      val procB     = Proc[S]   // transition bypass/engage per channel
      procB.graph() = switchGraph
      val procBObj  = procB // Obj(Proc.Elem(procB))
      procBObj.attr.put("state", stateObj)
      procBObj.name = s"by$li$si"
      val bPlaying  = state <  2
      val bFolder   = Folder[S]
      bFolder.addLast(procBObj)

      val ensB      = Ensemble[S](bFolder, 0L, bPlaying)
      val ensBObj   = ensB // Obj(Ensemble.Elem(ensB))
      val predInB   = procB .inputs .add("pred")
      val succInB   = procB .inputs .add("succ")
      val outB      = procB .outputs.add("out" )
      lFolder.addLast(ensBObj)
      predOut ~> predInB
      succOut ~> succInB
      outB    ~> collIn

      val active    = state > 0

      val doneObj   = done // Obj(Action.Elem(done))
      doneObj.attr.put("state", stateObj)

      new Channel(stateObj = stateObj, state = state, fPlaying = fPlaying, active = active,
        predOut = predOut, succOut = succOut, collIn = collIn, doneObj = doneObj)
    }

    val activeCount = count(vecChannels.map(_.active))
    if (PrintStates) activeCount.changed.react(_ => ch => println(s"activeCount$li -> ${ch.now}"))

    val lPlaying    = BooleanObj.newVar[S](false)
    // if (PrintStates) lPlaying.changed.react(_ => ch => println(s"lPlaying$li -> ${ch.now}"))

    val ensL    = Ensemble[S](lFolder, 0L, lPlaying)
    val ensLObj = ensL // Obj(Ensemble.Elem(ensL))

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
      val ensTObj     = ensT // Obj(Ensemble.Elem(ensT))
      lFolder.addLast(ensTObj)

      vecChannels.zipWithIndex.foreach { case (channel, si) =>
        val fFolder   = Folder[S]
        val ensF      = Ensemble[S](fFolder, 0L, channel.fPlaying)
        tFolder.addLast(ensF) // Obj(Ensemble.Elem(ensF)))

        val procT     = Proc[S]
        procT.graph() = g
        val predInT   = procT.inputs .add("pred")
        val succInT   = procT.inputs .add("succ")
        val outT      = procT.outputs.add("out")

        channel.predOut ~> predInT
        channel.succOut ~> succInT
        outT            ~> channel.collIn

        val procTObj  = procT // Obj(Proc.Elem(procT))
        procTObj.name = s"t$gi$si"
        val procAttr = procTObj.attr
        procAttr.put("state", channel.stateObj)
        procAttr.put("done" , channel.doneObj )

        fFolder.addLast(procTObj)
      }
    }

    // short debug solution; just connect all layer outputs to main diffusion
    if (!Shadowing) coll.outputs.add("out") ~> diff.inputs.add("in")

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
        if (PrintStates) println(s"Layer $li becomes inactive.")
        l.playing().update(false)
        unlinkLayer(l)
      }
    }
    l
  }

  private def count(in: Vec[BooleanObj[S]])(implicit tx: S#Tx): IntObj[S] = {
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