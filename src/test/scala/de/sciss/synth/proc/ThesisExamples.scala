package de.sciss
package synth
package proc

import de.sciss.synth.expr.{Longs, ExprImplicits, Ints}

class ThesisExamples[S <: Sys[S]] {
  import ugen._

  val sg = SynthGraph {
    val sig   = PinkNoise.ar
    val bus   = graph.attribute(ProcKeys.attrBus ).ir(0)
    val mute  = graph.attribute(ProcKeys.attrMute).ir(0)
    val env   = graph.FadeInOut(ProcKeys.attrFadeIn, ProcKeys.attrFadeOut).ar
    val amp   = env * (1 - mute)
    Out.ar(bus, sig * amp)
  }

  val imp = ExprImplicits[S]
  import imp._

  def configure(proc: Proc[S])(implicit tx: S#Tx) {
    val bus       = Ints .newVar[S](    0)
    val fadeInLen = Longs.newVar[S](44100)
    val fadeIn    = FadeSpec.Elem(fadeInLen, Curve.sine, 0.0)
    proc.attributes.put("bus"    , Attribute.Int     (bus   ))
    proc.attributes.put("mute"   , Attribute.Boolean (false ))
    proc.attributes.put("fade-in", Attribute.FadeSpec(fadeIn))
    proc.graph()  = sg

    bus()       = 1       // adjust bus
    fadeInLen() = 22050   // adjust fade-in duration
  }
}