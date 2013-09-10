package de.sciss.synth.proc

import de.sciss.synth.{ugen, SynthGraph}
import de.sciss.span.Span
import scala.concurrent.ExecutionContext
import de.sciss.processor.Processor
import de.sciss.lucre.stm.store.BerkeleyDB

// XXX TODO: this should be a ScalaTest spec, opening the file after bouncing, and
// verifying the contents (easy with a sine).
object BounceTest extends App {
  type S = Durable
  type I = S#I

  implicit val system = Durable(BerkeleyDB.tmp())

  // showTransportLog  = true

  val groupH = system.step { implicit tx =>
    val expr      = ExprImplicits[S]
    import expr._

    val proc      = Proc[S]
    proc.graph() = SynthGraph {
      import ugen._
      Out.ar(0, SinOsc.ar(440))
    }
    val group     = ProcGroup.Modifiable[S]
    group.add(Span(4410, 8820), proc)
    import ProcGroup.serializer
    tx.newHandle(group: ProcGroup[S])
  }

  val bounce              = Bounce[S, I]
  val bCfg                = bounce.Config()
  bCfg.group              = groupH
  bCfg.span               = Span(4410 + 2205, 4410 * 3) // start in the middle of the proc span
  val sCfg                = bCfg.server
  //sCfg.nrtCommandPath = "/Users/hhrutz/Desktop/test.osc"
  // sCfg.nrtOutputPath  = "/Users/hhrutz/Desktop/test.aif"
  //sCfg.programPath    = "/Applications/SuperCollider_3.6.5/SuperCollider.app/Contents/Resources/scsynth"

  // this is default now:
  // sCfg.inputBusChannels   = 0
  sCfg.outputBusChannels  = 1
  sCfg.sampleRate         = 44100

  // this is default now:
  // sCfg.blockSize          = 1       // sample accurate placement of synths

  val process             = bounce(bCfg)
  import ExecutionContext.Implicits.global

  val t = new Thread {
    override def run() {
      this.synchronized(this.wait())
    }
  }
  t.start()

  var lastProg = 0
  process.addListener {
    case prog @ Processor.Progress(_, _) =>
      val p = prog.toInt
      while (lastProg < p) {
        print('#')
        lastProg += 2
      }

    case Processor.Result(_, res) =>
      println(s" $lastProg%")
      println(res)
      t.synchronized(t.notifyAll())
  }
  process.start()
}