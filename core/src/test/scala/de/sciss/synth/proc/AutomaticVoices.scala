package de.sciss.synth.proc

import de.sciss.lucre.expr.{Int => IntEx, Boolean => BooleanEx, Expr}
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.InMemory

import scala.collection.immutable.{IndexedSeq => Vec}

object AutomaticVoices {
  val NumLayers   = 3
  val MaxVoices   = 2
  val NumSpeakers = 4

  type S = InMemory // Confluent

  def main(args: Array[String]): Unit = {
    //    val sys = Confluent(BerkeleyDB.tmp())
    //    val (_, cursor) = sys.cursorRoot(_ => ())(implicit tx => _ => sys.newCursor())
    val sys       = InMemory()
    val cursor    = sys
    val sensorsH  = cursor.step { implicit tx =>
      import IntEx.varSerializer
      mkWorld().map(tx.newHandle(_))
    }
    println("Made the world.")
    cursor.step { implicit tx =>
      val imp = ExprImplicits[S]
      import imp._
      val s0 = sensorsH(0)()
      s0()   = 0
    }
    println("Made a difference.")
    sys.close()
  }

  def mkWorld()(implicit tx: S#Tx): Vec[Expr.Var[S, Int]] = {
    val imp = ExprImplicits[S]
    import imp._

    val sensors = Vec.tabulate(NumSpeakers) { speaker =>
      IntEx.newVar[S](-1)
    }

    // decoupling data-flow recursion here
    val vecPlaying = Vec.tabulate(NumLayers) { layer =>
      val playing = BooleanEx.newVar[S](false)
      playing.changed.react(_ => ch => println(s"playing$layer -> ${ch.now}"))
      playing
    }

    val activeVoices   = count(vecPlaying)
    val hasFreeVoices  = activeVoices < MaxVoices
    activeVoices.changed.react(_ => ch => println(s"activeVoices -> ${ch.now}"))

    for (layer <- 0 until NumLayers) {
      val vecActive   = Vec.fill(NumSpeakers)(BooleanEx.newVar[S](false))
      val playing     = vecPlaying(layer)
      val vecGate: Vec[Expr[S, Boolean]] = Vec.tabulate(NumSpeakers) { speaker =>
        val isLayer = sensors(speaker) sig_== layer
        val gate    = isLayer && (playing || hasFreeVoices)
        gate.changed.react(_ => ch => println(s"gate$layer$speaker -> ${ch.now}"))
        gate
      }
      val sumActive = count(vecActive)
      val sumGate   = count(vecGate  )
      val playing1  = sumActive + sumGate > 0
      playing()     = playing1
    }

    sensors
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