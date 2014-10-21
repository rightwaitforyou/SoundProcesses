package de.sciss.synth.proc

import de.sciss.lucre.expr.{Int => IntEx, Boolean => BooleanEx}

object AutomaticVoices {
  val NumLayers   = 3
  val MaxVoices   = 2
  val NumSpeakers = 4

  type S = Confluent

  def main(args: Array[String]): Unit = {
    ???
  }

  def mkWorld()(implicit tx: S#Tx): Unit = {
    val imp = ExprImplicits[S]
    import imp._

    val sensors = Vector.tabulate(NumSpeakers) { speaker =>
      IntEx.newVar[S](0)
    }

    Vector.tabulate(NumLayers) { layer =>
      val active  = Vector.fill(NumSpeakers)(BooleanEx.newVar[S](false))
      val gates   = Vector.tabulate(NumSpeakers) { speaker =>
        val isLayer = sensors(speaker) sig_== layer
      }
      val playing = reduce(active.map(_.toInt))(_ + _)
    }
  }

  // like Vector.reduce, but splitting at the half,
  // thus allowing the composition of bin-ops with guaranteed
  // tree depth of ld(N)
  private def reduce[A](in: Vector[A])(op: (A, A) => A): A = {
    val sz = in.size
    if (sz <= 1) in.head else {
      val (front, back) = in.splitAt(sz >> 1)
      val a = reduce(front)(op)
      val b = reduce(back )(op)
      op(a, b)
    }
  }
}