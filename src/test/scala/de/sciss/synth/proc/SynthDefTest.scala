package de.sciss.synth
package proc

import impl.SynthGraphSerializer
import ugen._
import graph.scan
import de.sciss.lucre.DataOutput

object SynthDefTest extends App {
//   val graph = SynthGraph {
//      scan( "test" ).ar( 1234 )
//   }
//   UGenGraphBuilder.apply()

   val graph = SynthGraph {
      Out.ar( 0, SinOsc.ar( 441 ))
   }

   val out = new DataOutput()
   SynthGraphSerializer.write( graph, out )
}
