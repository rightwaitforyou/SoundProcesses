//package de.sciss.synth.proc
//
//import de.sciss.synth.{proc, ugen, SynthGraph}
//import de.sciss.span.Span
//import ugen._
//import Predef.{any2stringadd => _, _}
//import de.sciss.lucre.synth.InMemory
//import proc.Implicits._
//
//object MixTest extends App {
//
//  //  type S = Durable
//  //  type I = S#I
//  //
//  //  implicit val system = Durable(BerkeleyDB.tmp())
//
//  type S  = InMemory
//  type I  = InMemory
//  implicit val system = InMemory()
//
//  showAuralLog  = true
//  showLog       = true
//
//  implicit class RichDouble(d: Double) {
//    def seconds = (d * 44100).toLong
//  }
//
//  val aural = AuralSystem()
//
//  system.step {
//    implicit tx =>
//      import Implicits._
//      val expr = ExprImplicits[S]
//      import expr._
//
//      val procOut1  = Proc[S]
//      val peerOut1  = Proc.Elem(procOut1)
//      val objOut1   = Obj(peerOut1)
//      val procIn    = Proc[S]
//      val peerIn    = Proc.Elem(procIn)
//      val objIn     = Obj(peerIn)
//      val procOut2  = Proc[S]
//      val peerOut2  = Proc.Elem(procOut2)
//      val objOut2   = Obj(peerOut2)
//
//      objOut1.attr.name = "proc-out1"
//      objOut2.attr.name = "proc-out2"
//      objIn  .attr.name = "proc-in"
//
//      val out1 = procOut1.scans.add("out")
//      val in   = procIn  .scans.add("in" )
//      val out2 = procOut2.scans.add("out")
//
//      out1.addSink(in)
//      out2.addSink(in)
//
//      procOut1.graph() = SynthGraph {
//        graph.scan.Out("out", WhiteNoise.ar(0.5)) // SinOsc.ar(SinOsc.ar(10) * 30 + 400)
//      }
//
//      procIn.graph() = SynthGraph {
//        val sig = graph.scan.In("in") * Lag.ar(LFPulse.ar(3), 0.02)
//        Out.ar(0, Pan2.ar(sig))
//      }
//
//      procOut2.graph() = SynthGraph {
//        graph.scan.Out("out", SinOsc.ar(LFSaw.ar(0.5).linexp(-1, 1, 400, 800)))
//      }
//
//      val group = ProcGroup.Modifiable[S]
//      group.add(Span(1.seconds, 8.seconds), objOut1)
//      group.add(Span(1.seconds, 8.seconds), objIn  )
//      group.add(Span(1.seconds, 8.seconds), objOut2)
//
//      import Durable.inMemory
//      val transp = TransportOLD[S, I](group)
//
//      aural.whenStarted { s =>
//        s.peer.dumpOSC()
//        system.step {
//          implicit tx =>
//            AuralPresentationOLD.run[S](transp, aural)
//            // transp.react(tx => upd => println(s"Observed: $upd"))
//            transp.play()
//        }
//        new Thread {
//          override def run(): Unit = {
//            Thread.sleep(10 * 1000L)
//            sys.exit()
//          }
//        }.start()
//      }
//
//      aural.start()
//  }
//}