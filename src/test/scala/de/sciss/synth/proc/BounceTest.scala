package de.sciss.synth.proc

import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.synth.expr.ExprImplicits
import java.io.File
import de.sciss.synth.io.AudioFile
import de.sciss.synth.{ugen, SynthGraph, linShape, stepShape}
import de.sciss.synth.ugen.{Pan2, Out, FreqShift, Mix}
import de.sciss.span.Span

object BounceTest extends App {
  type S = Durable
  type I = S#I

  implicit val system = Durable(BerkeleyDB.tmp())

  system.step { implicit tx =>
    val expr      = ExprImplicits[S]
    import expr._

    val proc      = Proc[S]
    proc.graph_=(SynthGraph {
      import ugen._
      Out.ar(0, SinOsc.ar(440))
    })
    val group     = ProcGroup.Modifiable[S]
    group.add(Span(4410, 8820), proc)

    val bounce  = Bounce[S, I]
    val bCfg    = bounce.Config()
  }
}