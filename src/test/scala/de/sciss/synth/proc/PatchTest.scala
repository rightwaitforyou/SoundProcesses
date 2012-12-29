package de.sciss.synth
package proc

import de.sciss.lucre.stm
import stm.store.BerkeleyDB
import ugen._
import de.sciss.lucre.bitemp.Span
import expr.ExprImplicits

object PatchTest extends App {

   {
      type S   = Confluent
      type I   = stm.InMemory
      val sys  = Confluent( BerkeleyDB.tmp() )
      val (_, cursor) = sys.cursorRoot( _ => () )( tx => _ => tx.newCursor() )
      implicit val _cursor: stm.Cursor[ S ] = cursor
      cursor.step { implicit tx =>
         val auralSys = AuralSystem.start[ S ]()
         auralSys.whenStarted( implicit tx => { _ =>
            println( "Aural System started." )
            run[ S, I ]( auralSys ) // ( tx, cursor )
         })
      }
   }

   def run[ S <: Sys[ S ], I <: stm.Sys[ I ]]( auralSys: AuralSystem[ S ])
                                             ( implicit tx: S#Tx, cursor: stm.Cursor[ S ], bridge: S#Tx => I#Tx ) {

      val imp = ExprImplicits[ S ]
      import imp._

//confluent.showLog = true
//proc.showTransportLog = true
proc.showAuralLog = true

      val group      = ProcGroup_.Modifiable[ S ]
      val trans      = Transport[ S, I ]( group )
      implicit val artifactStore = ArtifactStore.tmp()
      val ap = AuralPresentation.run[ S, I ]( trans, auralSys )
ap.group.foreach { _.server.peer.dumpOSC() }

      val p1         = Proc[ S ]
      p1.name_=( "p1" )
      val p2         = Proc[ S ]
      p2.name_=( "p2" )

      val p1out      = p1.scans.add( "out" )
      val p2in       = p2.scans.add( "freq" )

      p1.graph_=( SynthGraph {
         graph.scan( "out" ) := SinOsc.ar( 0.1 ).linexp( -1, 1, 200, 2000 )
      })

      p2.graph_=( SynthGraph {
         val freq = graph.scan( "freq" ).ar( 441 )
         val sig  = RLPF.ar( Pulse.ar( freq ), freq * 2, 0.1 )
         Out.ar( 0, Pan2.ar( sig ))
      })

      p2in.source_=( Some( Scan.Link.Scan( p1out )))

      group.add( Span.from( 0L ), p1 )
      group.add( Span.from( (2.5 * 44100L).toLong ), p2 )   // begin when sine wave is at positive peak

      trans.play()
   }
}