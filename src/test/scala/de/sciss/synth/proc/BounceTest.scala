package de.sciss.synth.proc

import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.synth.expr.ExprImplicits
import de.sciss.synth.{ugen, SynthGraph}
import de.sciss.span.Span
import scala.concurrent.ExecutionContext

object BounceTest extends App {
  type S = Durable
  type I = S#I

  implicit val system = Durable(BerkeleyDB.tmp())

  showTransportLog  = true

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

    val bounce              = Bounce[S, I]
    val bCfg                = bounce.Config()
    bCfg.group              = group
    bCfg.span               = Span(0, 4410 * 3)
    val sCfg                = bCfg.server
    sCfg.inputBusChannels   = 0
    sCfg.outputBusChannels  = 1
    sCfg.sampleRate         = 44100

    val process             = bounce(bCfg)
    import ExecutionContext.Implicits.global
    process.start()

    new Thread { override def run() { Thread.sleep(1000) }} .start()  // process.start() only creates daemon threads, it seems
  }
}