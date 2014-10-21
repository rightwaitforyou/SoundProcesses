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
    val sys = InMemory()
    val cursor = sys
    cursor.step { implicit tx =>
      mkWorld()
    }
    println("Made the world.")
    sys.close()
  }

  def mkWorld()(implicit tx: S#Tx): Unit = {
    val imp = ExprImplicits[S]
    import imp._

    val sensors = Vec.tabulate(NumSpeakers) { speaker =>
      IntEx.newVar[S](0)
    }

    lazy val vecPlaying: Vec[Expr[S, Boolean]] = Vec.tabulate(NumLayers) { layer =>
      val vecActive  = Vec.fill(NumSpeakers)(BooleanEx.newVar[S](false))
      lazy val vecGate: Vec[Expr[S, Boolean]] = Vec.tabulate(NumSpeakers) { speaker =>
        val isLayer = sensors(speaker) sig_== layer
        val gate    = isLayer && (playing || hasFreeVoices)
        gate.changed.react(_ => ch => println(s"gate$layer$speaker -> ${ch.now}"))
        gate
      }
      lazy val sumActive  = count(vecActive)
      lazy val sumGate    = count(vecGate  )
      lazy val playing    = sumActive + sumGate > 0
      playing.changed.react(_ => ch => println(s"playing$layer -> ${ch.now}"))
      playing
    }
    lazy val activeVoices   = count(vecPlaying)
    lazy val hasFreeVoices  = activeVoices < MaxVoices
    activeVoices.changed.react(_ => ch => println(s"activeVoices -> ${ch.now}"))
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