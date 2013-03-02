package de.sciss.synth
package proc

import impl.SynthGraphSerializer
import ugen._
import de.sciss.lucre.io.DataOutput

object SynthDefTest extends App {
  val graph = SynthGraph {
    Out.ar(0, SinOsc.ar(441))
  }

  val out = DataOutput()
  SynthGraphSerializer.write(graph, out)
}
