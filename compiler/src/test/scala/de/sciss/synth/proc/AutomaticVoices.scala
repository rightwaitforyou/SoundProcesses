package de.sciss.synth.proc

import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.lucre.swing.IntSpinnerView
import de.sciss.lucre.synth.InMemory
import de.sciss.synth.{proc, SynthGraph}
import de.sciss.{synth, lucre}
import de.sciss.lucre.expr.{Expr, Boolean => BooleanEx, Double => DoubleEx, Int => IntEx}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.swing.deferTx
import de.sciss.numbers.Implicits._

import scala.collection.immutable.{IndexedSeq => Vec}

object AutomaticVoices {
  val NumLayers   = 3
  val MaxVoices   = 2
  val NumSpeakers = 4

  type S = Confluent
  // type S = InMemory

  private[this] val imp = ExprImplicits[S]
  import imp._

  def main(args: Array[String]): Unit = {
    val sys = Confluent(BerkeleyDB.tmp())
    val (_, _cursor) = sys.cursorRoot(_ => ())(implicit tx => _ => sys.newCursor())
    implicit val cursor = _cursor
    //    val sys = InMemory()
    //    implicit val cursor = sys
    lucre.synth.expr.initTypes()
    val compiler = proc.Compiler()
    cursor.step { implicit tx =>
      println("Making the world...")
      val world = mkWorld()
      println("Making action...")
      mkAction(world, compiler)
      println("Making procs...")
      val transport = mkProcs(world)
      println("Making views...")
      mkViews(world, transport, sys)
    }

    //    println("Changing a sensor...")
    //    cursor.step { implicit tx =>
    //      val s0 = world.sensors(0)()
    //      s0()   = 0
    //      checkWorld(world)
    //    }
    //    println("End.")
    //    sys.close()
  }

  class Speaker(val gate  : stm.Source[S#Tx, Expr    [S, Boolean]],
                val active: stm.Source[S#Tx, Expr.Var[S, Boolean]],
                val proc  : stm.Source[S#Tx, Proc.Obj[S]])

  class Layer(val speakers: Vec[Speaker], val playing: stm.Source[S#Tx, Expr[S, Boolean]])

  class World(val layers   : Vec[Layer],
              val sensors  : Vec[stm.Source[S#Tx, Expr.Var[S, Int    ]]],
              val activeVoices : stm.Source[S#Tx, Expr    [S, Int    ]],
              val hasFreeVoices: stm.Source[S#Tx, Expr    [S, Boolean]])

  def mkViews(w: World, transport: Transport[S], system: S)(implicit tx: S#Tx, _cursor: stm.Cursor[S]): Unit = {
    implicit val undo = new UndoManagerImpl
    val views = w.sensors.zipWithIndex.map { case (s, si) =>
      s().changed.react { implicit tx => _ =>
        checkWorld(w)
      }
      IntSpinnerView(s(), s"s$si", 64)
    }
    val vcView = IntSpinnerView(w.activeVoices(), "vc", 64)
    deferTx {
      import scala.swing._
      new Frame {
        title = "Automatic Voices"
        contents = new BoxPanel(Orientation.Vertical) {
          contents += new FlowPanel(views.zipWithIndex.flatMap { case (v, vi) =>
            new Label(s"s$vi:") :: v.component :: Nil
          }: _*)
          contents += Swing.VStrut(4)
          contents += new FlowPanel(new Label("vc:"), vcView.component)
        }
        pack().centerOnScreen()
        open()

        private val timer = new javax.swing.Timer(1000, Swing.ActionListener { _ =>
          _cursor.step { implicit tx =>
            checkWorld(w)
          }
        })
        timer.start()

        override def closeOperation(): Unit = {
          timer.stop()
          _cursor.step { implicit tx => transport.dispose() }
          system.close()
          sys.exit(0)
        }
      }
    }
  }

  def mkProcs(w: World)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Transport[S] = {
    val aural = AuralSystem()
    aural.whenStarted(_.peer.dumpOSC())
    val transport = Transport[S](aural)

    val g = SynthGraph {
      import synth._
      import ugen._
      val li    = graph.Attribute.ir("li"  , 0)
      val si    = graph.Attribute.ir("si"  , 0)
      val gate  = graph.Attribute.kr("gate", 0)
      // (li * 10 + si + 900).poll(0, "synth")
      val freq  = li.linexp(0, NumLayers - 1, 300.0, 2000.0)
      val pan   = si.linlin(0, NumSpeakers - 1, -1, 1)
      val env   = Env.asr(attack = 10, release = 10)
      val amp   = EnvGen.ar(env, gate = gate, levelScale = 0.5)
      val done  = Done.kr(amp)
      graph.Action(done, "done")
      val dust  = Decay.ar(Dust.ar(10), 1).min(1)
      val sig   = Resonz.ar(dust, freq, 0.1) * amp
      Out.ar(0, Pan2.ar(sig, pan))
    }

    w.layers.zipWithIndex.foreach { case (l, li) =>
      val folder  = Folder[S]
      val liObj   = Obj(IntElem(li))
      l.speakers.zipWithIndex.foreach { case (s, si) =>
        val siObj     = Obj(IntElem(si))
        // val proc      = Proc[S]
        // val procObj   = Obj(Proc.Elem(proc))
        val procObj   = s.proc()
        val proc      = procObj.elem.peer
        proc.graph()  = g
        val attr      = procObj.attr
        val gateObj   = Obj(BooleanElem(s.gate()))
        attr.put("li"   , liObj   )
        attr.put("si"   , siObj   )
        attr.put("gate" , gateObj )
        folder.addLast(procObj)
      }
      val playing = l.playing()
      val ens     = Ensemble(folder, 0L, playing)
      val ensObj  = Obj(Ensemble.Elem(ens))
      transport.addObject(ensObj)
    }

    transport.play()
    aural.start()
    transport
  }

  def checkWorld(w: World)(implicit tx: S#Tx): Unit = {
    val free = w.hasFreeVoices()
    w.layers.zipWithIndex.foreach { case (l, li) =>
      if (!l.playing().value && free.value) {
        l.speakers.zipWithIndex.foreach { case (s, si) =>
          val gate = s.gate()
          // println(s"gate$li$si: $gate")
          if (gate.value) s.active().update(true)
        }
      }
    }
  }

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
      cursor.step { implicit tx =>
        val action = actionH()
        w.layers.foreach { l =>
          l.speakers.foreach { s =>
            val activeObj = Obj(BooleanElem(s.active()))
            val actionObj = Obj(Action.Elem(action))
            actionObj.attr.put("active", activeObj)
            s.proc() .attr.put("done"  , actionObj)
          }
        }
      }
    }
  }

  def mkWorld()(implicit tx: S#Tx): World = {
    val sensors = Vec.tabulate(NumSpeakers) { speaker =>
      val sensor = IntEx.newVar[S](-1)
      sensor.changed.react(_ => ch => println(s"sensor$speaker -> ${ch.now}"))
      sensor
    }

    import BooleanEx.{serializer => boolSer, varSerializer => boolVarSer}
    import IntEx    .{serializer => intSer , varSerializer => intVarSer }

    val vecLayer = Vec.tabulate(NumLayers) { li =>
      val vecActive = Vec.tabulate(NumSpeakers) { si =>
        val active = BooleanEx.newVar[S](false)
        active.changed.react(_ => ch => println(s"active$li$si -> ${ch.now}"))
        active
      }
      val vecGate = Vec.tabulate(NumSpeakers) { si =>
        val isLayer = sensors(si) sig_== li
        val gate    = isLayer
        // println(s"gate$li$si: $gate")
        gate.changed.react(_ => ch => println(s"gate$li$si -> ${ch.now}"))
        gate
      }
      val sumActive = count(vecActive)
      val playing   = sumActive > 0
      playing.changed.react(_ => ch => println(s"playing$li -> ${ch.now}"))

      val speakers = (vecGate zip vecActive).map { case (gate, active) =>
        val proc      = Proc[S]
        val procObj   = Obj(Proc.Elem(proc))
        new Speaker(gate = tx.newHandle(gate), active = tx.newHandle(active), proc = tx.newHandle(procObj))
      }
      new Layer(speakers, tx.newHandle(playing))
    }

    val vecPlaying      = vecLayer.map(_.playing())
    val activeVoices    = count(vecPlaying)
    val hasFreeVoices   = activeVoices < MaxVoices
    activeVoices.changed.react(_ => ch => println(s"activeVoices -> ${ch.now}"))

    new World(vecLayer, sensors.map(tx.newHandle(_)), tx.newHandle(activeVoices), tx.newHandle(hasFreeVoices))
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