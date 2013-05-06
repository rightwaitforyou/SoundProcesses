package de.sciss.synth
package proc

import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.bitemp.BiExpr
import de.sciss.synth.expr.ExprImplicits
import de.sciss.synth.io.AudioFile
import java.io.File
import de.sciss.span.Span
import de.sciss.lucre.stm

object TapeTest extends App {
  type S = Durable
  type I = S#I

  implicit val system = Durable(BerkeleyDB.tmp())

  implicit class RichDouble(val d: Double) {
    def seconds = (d * 44100).toLong
  }

  system.step { implicit tx =>
    val expr      = ExprImplicits[S]
    import expr._

    val proc      = Proc[S]
    val scan      = proc.scans.add("sig")
    val file      = new File("/Users/hhrutz/Desktop/sciss2013/_creation/CCC/TrailersLostShadowsLim16bCutup.aif")
    val spec      = AudioFile.readSpec(file)
    val audio     = Grapheme.Value.Audio(file, spec, offset = 0L, gain = 1.0)
    val grapheme  = Grapheme.Modifiable[S]
    grapheme.add(1.seconds -> audio)
    scan.source_=(Some(Scan.Link.Grapheme(grapheme)))
    proc.graph_=(SynthGraph {
      import ugen._
      val sig = graph.scan("sig").ar(0.0)
      Out.ar(0, sig)
    })
    val group     = ProcGroup.Modifiable[S]
    group.add(Span(1.seconds, 3.seconds), proc)

    import Durable.inMemory
    val transp  = Transport[S, I](group)
    val aural   = AuralSystem.start()
    AuralPresentation.run[S, I](transp, aural)

    val t = new Thread {
      override def run() {
        this.synchronized(this.wait())
        Thread.sleep(4000L)
        sys.exit()
      }
      start() // bug in ScalaCollider's server boot - we have to make sure a thread is started before aural.start
    }

    aural.whenStarted { implicit tx => _ =>
      showTransportLog = true
      transp.play()
      t.synchronized { t.notifyAll() }
    }
  }
}