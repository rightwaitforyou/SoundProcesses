package de.sciss.synth
package proc

import de.sciss.synth.io.AudioFile
import java.io.File
import de.sciss.span.Span
import de.sciss.lucre.synth.InMemory
import proc.Implicits._

object StreamTest extends App {
  type S = InMemory
  type I = InMemory

  implicit val system = InMemory()

  val aural = AuralSystem()

  showAuralLog  = true
  showLog       = true

  system.step {
    implicit tx =>
      val expr = ExprImplicits[S]
      import expr._
      import Implicits._

      val proc  = Proc[S]
      val obj   = Obj(Proc.Elem(proc))
      obj.attr.name = "tape"
      val file = new File("/Users/hhrutz/Desktop/sciss2013/_creation/CCC/TrailersLostShadowsLim16bCutup.aif")
      val spec = AudioFile.readSpec(file)
      // val vAudio    = Grapheme.Value.Audio(file, spec, offset = 0L, gain = 2.0)
      val loc = ArtifactLocation.Modifiable[S](file.getParentFile)
      val artifact = loc.add(file)
      val eAudio = Grapheme.Expr.Audio[S](artifact, spec, offset = 0L, gain = 2.0)

      obj.attr.put("disk", AudioGraphemeElem(eAudio))

      proc.graph() = SynthGraph {
        import ugen._
        val sig = graph.VDiskIn.ar("disk", speed = 0.7, interp = 1, maxSpeed = 0.7)
        Out.ar(0, sig)
      }
      val group = ProcGroup.Modifiable[S]
      group.add(Span.from(0L), obj)

      val transp = Transport[S, I](group)

      aural.whenStarted {
        s =>
          s.peer.dumpOSC()
          system.step {
            implicit tx =>
              AuralPresentation.run[S](transp, aural)
              transp.play()
          }
          new Thread {
            override def run(): Unit = {
              Thread.sleep(10 * 1000L)
              sys.exit()
            }
          }.start()
      }

      aural.start()
  }
}