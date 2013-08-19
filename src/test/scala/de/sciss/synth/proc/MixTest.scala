package de.sciss.synth.proc

import de.sciss.synth.expr.ExprImplicits
import de.sciss.synth.{ugen, SynthGraph}
import de.sciss.span.Span
import ugen._
import Predef.{any2stringadd => _, _}

object MixTest extends App {

  //  type S = Durable
  //  type I = S#I
  //
  //  implicit val system = Durable(BerkeleyDB.tmp())

  type S  = InMemory
  type I  = InMemory
  implicit val system = InMemory()

  showAuralLog  = true
  showTxnLog    = true

  implicit class RichDouble(d: Double) {
    def seconds = (d * 44100).toLong
  }

  val aural = AuralSystem()

  val transp = system.step { implicit tx =>
    import Implicits._
    val expr      = ExprImplicits[S]
    import expr._

    val procOut1  = Proc[S]
    val procIn    = Proc[S]
    val procOut2  = Proc[S]

    procOut1.name = "proc-out1"
    procOut2.name = "proc-out2"
    procIn  .name = "proc-in"

    val out1      = procOut1.scans.add("out")
    val in        = procIn  .scans.add("in" )
    val out2      = procOut2.scans.add("out")

    out1.addSink(in)
    out2.addSink(in)

    procOut1.graph() = SynthGraph {
      graph.scan.Out("out", WhiteNoise.ar(0.5)) // SinOsc.ar(SinOsc.ar(10) * 30 + 400)
    }

    procIn.graph() = SynthGraph {
      val sig = graph.scan.In("in") * Lag.ar(LFPulse.ar(3), 0.02)
      Out.ar(0, Pan2.ar(sig))
    }

    procOut2.graph() = SynthGraph {
      graph.scan.Out("out", SinOsc.ar(LFSaw.ar(0.5).linexp(-1, 1, 400, 800)))
    }

    val group     = ProcGroup.Modifiable[S]
    group.add(Span(1.seconds, 8.seconds), procOut1)
    group.add(Span(1.seconds, 8.seconds), procIn  )
    group.add(Span(1.seconds, 8.seconds), procOut2)

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