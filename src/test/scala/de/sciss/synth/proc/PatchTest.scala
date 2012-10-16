package de.sciss.synth
package proc

import de.sciss.lucre.{event => evt, stm, confluent}
import confluent.reactive.ConfluentReactive
import ugen._
import de.sciss.lucre.bitemp.Span
import expr.ExprImplicits

object PatchTest extends App {

   {
      type S            = ConfluentReactive
      type I            = stm.InMemory
      implicit val sys  = ConfluentReactive.tmp()
      sys.root( _ => () )
      sys.step { implicit tx =>
         val auralSys = AuralSystem.start[ S, I ]()
         auralSys.whenStarted( implicit tx => { _ =>
//            println( "AQUI" )
            run[ S, I ]( auralSys )
         })
      }
   }

   def run[ S <: evt.Sys[ S ], I <: stm.Sys[ I ]]( auralSys: AuralSystem[ S ])
                                                 ( implicit tx: S#Tx, cursor: stm.Cursor[ S ], bridge: S#Tx => I#Tx ) {

      val imp = ExprImplicits[ S ]
      import imp._

      val group      = ProcGroup_.Modifiable[ S ]
      val trans      = Transport[ S, I ]( group )
      AuralPresentation.run[ S, I ]( trans, auralSys )

      val p1         = Proc[ S ]
      val p2         = Proc[ S ]
      val p1out      = p1.scans.add( "out" )
      val p2in       = p2.scans.add( "freq" )

      p1.graph_=( SynthGraph {
         graph.scan( "out" ) := SinOsc.ar( 0.1 ).linexp( -1, 1, 200, 2000 )
      })

      p2.graph_=( SynthGraph {
         val freq = 441 // graph.scan( "freq" ).ar( 441 )
         val sig  = RLPF.ar( Pulse.ar( freq ), freq * 2, 0.1 )
         Out.ar( 0, Pan2.ar( sig ))
      })

      p2in.source_=( Some( Scan.Link.Scan( p1out )))

      group.add( Span.from( 0L ), p1 )
      group.add( Span.from( 5 * 44100L ), p2 )


      trans.play()
   }
}