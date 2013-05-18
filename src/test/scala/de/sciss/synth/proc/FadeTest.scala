package de.sciss.synth
package proc

import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.synth.expr.{EnvShapes, ExprImplicits}
import de.sciss.synth.io.AudioFile
import java.io.File
import de.sciss.span.Span

object FadeTest extends App {
  type S = Durable
  type I = S#I

  implicit val system = Durable(BerkeleyDB.tmp())

  implicit class RichDouble(val d: Double) {
    def seconds = (d * 44100).toLong
  }

  val aural = AuralSystem()

  def twice = true

  val transp = system.step { implicit tx =>
    val expr      = ExprImplicits[S]
    import expr._

    // val spat = Proc[S]
    // val spatIn = spat.scans.add("in")
    // spatIn.addSink()

    val proc      = Proc[S]
    val fadeExpr  = FadeSpec.Elem(44100, EnvShapes.newConst(linShape), 0.0) // FadeSpec.Value(44100, linShape)
    proc.attributes.put("fadeIn", Attribute.FadeSpec(fadeExpr))
    proc.graph_=(SynthGraph {
      import ugen._
      val noise = PinkNoise.ar
      val env   = graph.FadeIn("fadeIn").ar
      val sig   = noise * env
      Out.ar(0, Pan2.ar(sig))
    })
    val group     = ProcGroup.Modifiable[S]
    group.add(Span(1.seconds, 4.seconds), proc)

    import Durable.inMemory
    val _transp  = Transport[S, I](group)

    _transp
  }

  aural.whenStarted { s =>
    // showTransportLog = true
    s.peer.dumpOSC()
    system.step { implicit tx =>
      AuralPresentation.runTx[S](transp, aural)
      transp.react(tx => upd => println(s"Observed: $upd"))
      transp.play()
    }
    new Thread {
      override def run() {
        Thread.sleep(5 * 1000L)
        if (twice) {
          println("\nSecond iteration\n")
          system.step { implicit tx =>
            transp.stop()
            transp.seek(0L)
            transp.play()
            showTxnLog = true
          }
          Thread.sleep(5 * 1000L)
        }
        sys.exit()
      }
    } .start()
    // t.synchronized { t.notifyAll() }
  }

  aural.start()
}