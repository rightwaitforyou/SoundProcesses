package de.sciss.synth.proc

import de.sciss.lucre
import de.sciss.lucre.expr.{Int => IntEx, Boolean => BooleanEx, Expr}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.{Sys, InMemory}

import scala.collection.immutable.{IndexedSeq => Vec}

object AutomaticVoices {
  val NumLayers   = 3
  val MaxVoices   = 2
  val NumSpeakers = 4

  type S = Confluent
  //  type S = InMemory

  private[this] val imp = ExprImplicits[S]
  import imp._

  def main(args: Array[String]): Unit = {
    val sys = Confluent(BerkeleyDB.tmp())
    val (_, cursor) = sys.cursorRoot(_ => ())(implicit tx => _ => sys.newCursor())
    //    val sys       = InMemory()
    //    val cursor    = sys
    lucre.synth.expr.initTypes()
    println("Making the world...")
    val world = cursor.step { implicit tx =>
      mkWorld()
    }
    println("Changing a sensor...")
    cursor.step { implicit tx =>
      val imp = ExprImplicits[S]
      import imp._
      val s0 = world.sensors(0)()
      s0()   = 0
      checkWorld(world)
    }
    println("End.")
    sys.close()
  }

  class Speaker(val gate: stm.Source[S#Tx, Expr    [S, Boolean]],
                val active: stm.Source[S#Tx, Expr.Var[S, Boolean]])

  class Layer(val speakers: Vec[Speaker], val playing: stm.Source[S#Tx, Expr[S, Boolean]])

  class World(val layers: Vec[Layer],
              val sensors: Vec[stm.Source[S#Tx, Expr.Var[S, Int]]],
              val hasFreeVoices: stm.Source[S#Tx, Expr[S, Boolean]])

  def checkWorld(w: World)(implicit tx: S#Tx): Unit =
    w.layers.foreach { l =>
      if (!l.playing().value) {
        l.speakers.foreach { s =>
          if (s.gate().value) s.active().update(true)
        }
      }
    }

  def mkWorld()(implicit tx: S#Tx): World = {
    val sensors = Vec.tabulate(NumSpeakers) { speaker =>
      val sensor = IntEx.newVar[S](-1)
      sensor.changed.react(_ => ch => println(s"sensor$speaker -> ${ch.now}"))
      sensor
    }

    //    // decoupling data-flow recursion here
    //    val vecPlaying = Vec.tabulate(NumLayers) { layer =>
    //      val playing = BooleanEx.newVar[S](false)
    //      playing.changed.react(_ => ch => println(s"playing$layer -> ${ch.now}"))
    //      playing
    //    }

    import IntEx.{varSerializer => intVarSer}
    import BooleanEx.{serializer => boolSer, varSerializer => boolVarSer}

    val vecLayer = Vec.tabulate(NumLayers) { layer =>
      val vecActive   = Vec.tabulate(NumSpeakers) { speaker =>
        val active = BooleanEx.newVar[S](false)
        active.changed.react(_ => ch => println(s"active$layer$speaker -> ${ch.now}"))
        active
      }
      // val playing     = vecPlaying(layer)
      val vecGate: Vec[Expr[S, Boolean]] = Vec.tabulate(NumSpeakers) { speaker =>
        val isLayer = sensors(speaker) sig_== layer
        val gate    = isLayer // && (playing || hasFreeVoices)
        gate.changed.react(_ => ch => println(s"gate$layer$speaker -> ${ch.now}"))
        gate
      }
      val sumActive = count(vecActive)
      // val sumGate   = count(vecGate  )
      val playing   = sumActive /* + sumGate */ > 0
      playing.changed.react(_ => ch => println(s"playing$layer -> ${ch.now}"))

      val speakers = (vecGate zip vecActive).map { case (gate, active) =>
        new Speaker(gate = tx.newHandle(gate), active = tx.newHandle(active))
      }
      new Layer(speakers, tx.newHandle(playing))
    }

    val vecPlaying      = vecLayer.map(_.playing())
    val activeVoices    = count(vecPlaying)
    val hasFreeVoices   = activeVoices < MaxVoices
    activeVoices.changed.react(_ => ch => println(s"activeVoices -> ${ch.now}"))

    new World(vecLayer, sensors.map(tx.newHandle(_)), tx.newHandle(hasFreeVoices))
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