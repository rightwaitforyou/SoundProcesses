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

  de.sciss.lucre.synth.showLog = true
  showTransportLog  = true

  def frame(secs: Double): Long = (secs * Timeline.SampleRate).toLong

  println(
    """Expected outcome:
      |
      |A sound file of duration 150ms. a sine tone of 200 Hz
      |is seen for 50ms (or 10 periods), the remaining 100ms are silent.
      |""".stripMargin)

  val groupH = system.step { implicit tx =>
    val expr      = ExprImplicits[S]
    import expr._

    val proc      = Proc[S]
    val peer      = Proc.Elem(proc)
    val obj       = Obj(peer)
    proc.graph() = SynthGraph {
      import ugen._
      Out.ar(0, SinOsc.ar(200))
    }
    val group     = Timeline[S]
    group.add(Span(frame(0.1), frame(0.2)), obj)
    // import ProcGroup.serializer
    tx.newHandle(Obj(Timeline.Elem(group)))
  }

  val bounce              = Bounce[S, I]
  val bCfg                = Bounce.Config[S]
  bCfg.group              = groupH :: Nil
  bCfg.span               = Span(frame(0.15), frame(0.3)) // start in the middle of the proc span
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
    override def run(): Unit = {
      this.synchronized(this.wait())
      sys.exit(0)
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