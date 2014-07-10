package de.sciss.synth
package proc

import de.sciss.span.Span
import de.sciss.synth.Curve.{exponential, linear}
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre

object FadeTest extends App {
  type S = Durable
  type I = S#I

  implicit val system = Durable(BerkeleyDB.tmp())

  showAuralLog = true

  implicit class RichDouble(val d: Double) {
    def seconds = (d * 44100).toLong
  }

  val aural = AuralSystem()

  def twice = true

  system.step {
    implicit tx =>
      val expr = ExprImplicits[S]
      import expr._

      // val spat = Proc[S]
      // val spatIn = spat.scans.add("in")
      // spatIn.addSink()lucre.synth.expr.

      val proc  = Proc[S]
      val peer  = Proc.Elem(proc)
      val obj   = Obj(peer)
      val fadeExprIn  = FadeSpec.Expr(44100, lucre.synth.expr.Curve.newConst(linear), 0.0) // FadeSpec.Value(44100, linShape)
      val fadeExprOut = FadeSpec.Expr(44100, lucre.synth.expr.Curve.newConst(exponential), -40.dbamp) // FadeSpec.Value(44100, linShape)
      obj.attr.put("fadeIn" , Obj(FadeSpec.Elem(fadeExprIn)))
      obj.attr.put("fadeOut", Obj(FadeSpec.Elem(fadeExprOut)))
      proc.graph() = SynthGraph {
        import ugen._
        val noise = PinkNoise.ar
        val env = graph.FadeInOut("fadeIn", "fadeOut").ar
        val sig = noise * env
        Out.ar(0, Pan2.ar(sig))
      }
      val group = ProcGroup.Modifiable[S]
      group.add(Span(1.seconds, 4.seconds), obj)

      import Durable.inMemory
      val transp = TransportOLD[S, I](group)

      aural.whenStarted {
        s =>
        // showTransportLog = true
          s.peer.dumpOSC()
          system.step {
            implicit tx =>
              AuralPresentationOLD.run[S](transp, aural)
              transp.react(tx => upd => println(s"Observed: $upd"))
              transp.play()
          }
          new Thread {
            override def run(): Unit = {
              Thread.sleep(5 * 1000L)
              if (twice) {
                println("\nSecond iteration\n")
                system.step {
                  implicit tx =>
                    transp.stop()
                    transp.seek(0L)
                    transp.play()
                    showLog = true
                }
                Thread.sleep(5 * 1000L)
              }
              sys.exit()
            }
          }.start()
        // t.synchronized { t.notifyAll() }
      }

      aural.start()
  }
}