package de.sciss.synth
package proc

import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.synth.expr.ExprImplicits
import de.sciss.synth.io.AudioFile
import java.io.File
import de.sciss.span.Span

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

    // val spat = Proc[S]
    // val spatIn = spat.scans.add("in")
    // spatIn.addSink()

    val proc      = Proc[S]
    val sAudio    = proc.scans.add("sig")
    val file      = new File("/Users/hhrutz/Desktop/sciss2013/_creation/CCC/TrailersLostShadowsLim16bCutup.aif")
    val spec      = AudioFile.readSpec(file)
    val vAudio    = Grapheme.Value.Audio(file, spec, offset = 0L, gain = 2.0)
    val gAudio    = Grapheme.Modifiable[S]
    gAudio.add((1 - 4.5).seconds -> vAudio)  // ... çoit trop complexe ...
    sAudio.source_=(Some(Scan.Link.Grapheme(gAudio)))

    val gSpat     = Grapheme.Modifiable[S]
    val sSpat     = proc.scans.add("spat")
    sSpat.source_=(Some(Scan.Link.Grapheme(gSpat)))
    gSpat.add(1.seconds -> Grapheme.Value.Curve((-1.0, stepShape)))
    gSpat.add(4.seconds -> Grapheme.Value.Curve(( 1.0,  linShape)))

    proc.graph_=(SynthGraph {
      import ugen._
      val sig0  = graph.scan("sig").ar(0.0)
      val sig   = Mix.mono(sig0)
      val spat  = graph.scan("spat").ar(0.0)
      Out.ar(0, Pan2.ar(sig, spat))
    })
    val group     = ProcGroup.Modifiable[S]
    group.add(Span(1.seconds, 4.seconds), proc)

    import Durable.inMemory
    val transp  = Transport[S, I](group)
    val aural   = AuralSystem.start()
    AuralPresentation.run[S, I](transp, aural)

    val t = new Thread {
      override def run() {
        this.synchronized(this.wait())
        Thread.sleep(5 * 1000L)
        sys.exit()
      }
      start() // bug in ScalaCollider's server boot - we have to make sure a thread is started before aural.start
    }

    aural.whenStarted { implicit tx => s =>
      // showTransportLog = true
      s.peer.dumpOSC()
      transp.play()
      t.synchronized { t.notifyAll() }
    }
  }
}