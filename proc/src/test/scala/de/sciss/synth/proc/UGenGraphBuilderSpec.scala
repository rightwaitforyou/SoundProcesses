package de.sciss.synth.proc

import de.sciss.synth.SynthGraph
import de.sciss.synth.impl.DefaultUGenGraphBuilderFactory
import de.sciss.ConfluentEventSpec
import de.sciss.synth.proc.impl.UGenGraphBuilderImpl
import de.sciss.lucre.bitemp.BiGroup
import de.sciss.span.Span

/*
 To run only this suite:

 test-only de.sciss.synth.proc.UGenGraphBuilderSpec

 */
class UGenGraphBuilderSpec extends ConfluentEventSpec {
  "A SynthGraph" should "expand to UGens" in { system =>
    val g0 = SynthGraph {
      import de.sciss.synth._
      import ugen._
      val v = LFDNoise3.ar(0.1).linexp(-1, 1, 1e-5, 1)
      val n = 32
      var mix: GE = 0
      for (i <- 0 until n) {
        val freq = LFDNoise3.ar(LFDNoise3.ar(v).linexp(-1, 1, 1e-4, 0.01)).linexp(-1, 1, 64, 16000)
        val sin  = SinOsc.ar(freq)
        val mul  = LFDNoise3.ar(LFDNoise3.ar(v).linexp(-1, 1, 1e-4, 1)).linexp(-1, 1, 0.001, 1)
        mix += sin * mul
      }
      val sig = OnePole.ar(mix / n * 2, 0.95)
      Out.ar(0, Pan2.ar(sig))

      //      val a = LFDNoise3.ar(1)
      //      val b = LFDNoise3.ar(1)
      //      Out.ar(0, a + b)
    }

    val poH = system.step { implicit tx =>
      val p     = Proc[S]
      p.graph() = SynthGraphs.newConst[S](g0)
      val obj   = Obj(ProcElem(p))
      tx.newHandle(obj)
    }

    system.step { implicit tx =>
      val p   = poH().elem.peer
      val g1  = p.graph.value
      assert(g1 === g0)

      val timedID   = tx.newID()
      val timedSpan = de.sciss.lucre.bitemp.SpanLike.newConst[S](Span(0L, 1000L))
      val timed     = BiGroup.TimedElem[S, Obj.T[S, ProcElem]](timedID, timedSpan, poH())

      info("---- expanding using default builder ----")
      val u0  = DefaultUGenGraphBuilderFactory.build(g0)
      info("---- expanding using sound processes builder ----")
      val ub1 = UGenGraphBuilderImpl(null, timed, 0L)
      assert(ub1.tryBuild())
      val u1  = ub1.finish

      assert(u0.ugens.size === u1.ugens.size)
      assert(u0.toString === u1.toString) // individual UGens will not equal each other
    }
  }
}