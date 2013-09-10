package de.sciss.synth
package proc

import de.sciss.synth.io.AudioFile
import java.io.File
import de.sciss.span.Span
import de.sciss.synth.Curve.{step, linear}
import de.sciss.lucre.stm.store.BerkeleyDB

object TapeTest extends App {
  type S = Durable
  type I = S#I

  implicit val system = Durable(BerkeleyDB.tmp())

  implicit class RichDouble(val d: Double) {
    def seconds = (d * 44100).toLong
  }

  val aural = AuralSystem()

  def twice     = true
  showAuralLog  = true
  showLog       = true

  val transp = system.step { implicit tx =>
    val expr      = ExprImplicits[S]
    import expr._
    import Implicits._

    // val spat = Proc[S]
    // val spatIn = spat.scans.add("in")
    // spatIn.addSink()

    val proc      = Proc[S]
    proc.name     = "tape"
    val sAudio    = proc.scans.add("sig")
    val file      = new File("/Users/hhrutz/Desktop/sciss2013/_creation/CCC/TrailersLostShadowsLim16bCutup.aif")
    val spec      = AudioFile.readSpec(file)
    val vAudio    = Grapheme.Value.Audio(file, spec, offset = 0L, gain = 2.0)
    val gAudio    = Grapheme.Modifiable[S]
    gAudio.add((1 - 4.5).seconds -> vAudio)  // ... çoit trop complexe ...
    sAudio.addSource(Scan.Link.Grapheme(gAudio))

    val gSpat     = Grapheme.Modifiable[S]
    val sSpat     = proc.scans.add("spat")
    sSpat.addSource(Scan.Link.Grapheme(gSpat))
    gSpat.add(1.seconds -> Grapheme.Value.Curve((-1.0, step  )))
    gSpat.add(4.seconds -> Grapheme.Value.Curve(( 1.0, linear)))

    proc.attributes.put("freq", Attribute.Double(200.0))

    proc.graph() = SynthGraph {
      import ugen._
      val freq  = graph.attribute("freq").ir
      val sig0  = graph.scan.In("sig")
      val sig1  = Mix.mono(sig0)
      val sig   = FreqShift.ar(sig1, freq)
      val spat  = graph.scan.In("spat")
      Out.ar(0, Pan2.ar(sig, spat))
    }
    val group     = ProcGroup.Modifiable[S]
    group.add(Span(1.seconds, 4.seconds), proc)

    import Durable.inMemory
    val _transp  = Transport[S, I](group)

    //    val t = new Thread {
    //      override def run() {
    //        this.synchronized(this.wait())
    //        Thread.sleep(5 * 1000L)
    //        sys.exit()
    //      }
    //      start() // bug in ScalaCollider's server boot - we have to make sure a thread is started before aural.start
    //    }
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
            showLog = true
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