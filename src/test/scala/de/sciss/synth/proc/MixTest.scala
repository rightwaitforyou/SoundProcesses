package de.sciss.synth.proc

import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.synth.expr.ExprImplicits
import de.sciss.synth.{ugen, SynthGraph}
import de.sciss.span.Span
import ugen._

object MixTest extends App {

  //  type S = Durable
  //  type I = S#I
  //
  //  implicit val system = Durable(BerkeleyDB.tmp())

  type S  = InMemory
  type I  = InMemory
  implicit val system = InMemory()

  implicit class RichDouble(d: Double) {
    def seconds = (d * 44100).toLong
  }

  val aural = AuralSystem()

  val transp = system.step { implicit tx =>
    val expr      = ExprImplicits[S]
    import expr._

    val proc1     = Proc[S]
    val proc3     = Proc[S]
    val proc2     = Proc[S]

    proc1.scans.add("out")
    proc3.scans.add("in")
    proc2.scans.add("out")

    proc1.graph() = SynthGraph {
      graph.scan("out") := SinOsc.ar(400)
    }

    proc3.graph() = SynthGraph {
      val sig = graph.scan("in").ar * LFPulse.ar(3)
      Out.ar(0, Pan2.ar(sig))
    }

    proc2.graph() = SynthGraph {
      graph.scan("out") := SinOsc.ar(LFSaw.ar(0.5).linexp(-1, 1, 400, 800))
    }

    val group     = ProcGroup.Modifiable[S]
    group.add(Span(1.seconds, 8.seconds), proc1)
    group.add(Span(1.seconds, 8.seconds), proc2)
    group.add(Span(1.seconds, 8.seconds), proc3)

    import Durable.inMemory
    Transport[S, I](group)
  }

  aural.whenStarted { s =>
    s.peer.dumpOSC()
    system.step { implicit tx =>
      AuralPresentation.runTx[S](transp, aural)
      // transp.react(tx => upd => println(s"Observed: $upd"))
      transp.play()
    }
    new Thread {
      override def run() {
        Thread.sleep(10 * 1000L)
        sys.exit()
      }
    } .start()
  }

  aural.start()
}