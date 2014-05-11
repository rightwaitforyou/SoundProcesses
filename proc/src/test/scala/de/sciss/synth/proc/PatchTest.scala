package de.sciss.synth
package proc

import de.sciss.lucre.stm
import ugen._
import de.sciss.span.Span
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.stm.store.BerkeleyDB

object PatchTest extends App {

  {
    type S  = Confluent
    type I  = S#I
    val sys = Confluent(BerkeleyDB.tmp())
    val (_, cursor) = sys.cursorRoot(_ => ())(implicit tx => _ => sys.newCursor())
    implicit val _cursor: stm.Cursor[S] = cursor
    val auralSys = AuralSystem()
    cursor.step {
      implicit tx =>
        auralSys.whenStarted(_ => cursor.step {
          implicit tx =>
            println("Aural System started.")
            run[S, I](auralSys)
        })
        auralSys.start()
    }
  }

  def run[S <: Sys[S], I <: stm.Sys[I]](auralSys: AuralSystem)
                                       (implicit tx: S#Tx, cursor: stm.Cursor[S], bridge: S#Tx => I#Tx): Unit = {

    val imp = ExprImplicits[S]
    import imp._

    //confluent.showLog = true
    //proc.showTransportLog = true
    proc.showAuralLog = true

    val group         = ProcGroup.Modifiable[S]
    val trans         = Transport[S, I](group)
    implicit val loc  = ArtifactLocation.Modifiable.tmp[S]()
    val ap            = AuralPresentation.run[S](trans, auralSys)
    ap.group.foreach {
      _.server.peer.dumpOSC()
    }

    val p1 = Proc[S]
    // p1.name_=( "p1" )
    val p2 = Proc[S]
    // p2.name_=( "p2" )

    val p1out = p1.scans.add("out")
    val p2in = p2.scans.add("freq")

    p1.graph() = SynthGraph {
      graph.scan.Out("out", SinOsc.ar(0.1).linexp(-1, 1, 200, 2000))
    }

    p2.graph() = SynthGraph {
      val freq = graph.scan.In("freq", 441)
      val sig = RLPF.ar(Pulse.ar(freq), freq * 2, 0.1)
      Out.ar(0, Pan2.ar(sig))
    }

    p2in.addSource(Scan.Link.Scan(p1out))

    group.add(Span.from(0L), Obj(Proc.Elem(p1)))
    group.add(Span.from((2.5 * 44100L).toLong), Obj(Proc.Elem(p2))) // begin when sine wave is at positive peak

    trans.play()
  }
}