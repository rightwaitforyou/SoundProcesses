package de.sciss.synth
package proc

import de.sciss.synth.io.AudioFile
import java.io.File
import de.sciss.span.Span
import de.sciss.synth.Curve.{step, linear}
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.InMemory

object StreamTest extends App {
  type S = InMemory
  type I = InMemory

  implicit val system = InMemory()

  val aural = AuralSystem()

  showAuralLog  = true
  showLog       = true

  val transp = system.step { implicit tx =>
    val expr      = ExprImplicits[S]
    import expr._
    import Implicits._

    val proc      = Proc[S]
    proc.name     = "tape"
    val file      = new File("/Users/hhrutz/Desktop/sciss2013/_creation/CCC/TrailersLostShadowsLim16bCutup.aif")
    val spec      = AudioFile.readSpec(file)
    // val vAudio    = Grapheme.Value.Audio(file, spec, offset = 0L, gain = 2.0)
    val loc       = Artifact.Location.Modifiable[S](file.getParentFile)
    val artifact  = loc.add(file)
    val eAudio    = Grapheme.Elem.Audio[S](artifact, spec, offset = 0L, gain = 2.0)

    proc.attributes.put("disk", Attribute.AudioGrapheme(eAudio))

    proc.graph() = SynthGraph {
      import ugen._
      val sig   = graph.VDiskIn.ar("disk", speed = 0.7, interp = 1, maxSpeed = 0.7)
      Out.ar(0, sig)
    }
    val group     = ProcGroup.Modifiable[S]
    group.add(Span.from(0L), proc)

    val _transp  = Transport[S, I](group)
    _transp
  }

  aural.whenStarted { s =>
    s.peer.dumpOSC()
    system.step { implicit tx =>
      AuralPresentation.runTx[S](transp, aural)
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