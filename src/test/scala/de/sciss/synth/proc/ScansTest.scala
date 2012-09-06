package de.sciss.synth.proc

import de.sciss.lucre.stm.{InMemory, Sys}
import de.sciss.synth.expr.ExprImplicits
import de.sciss.lucre.bitemp.Span

object ScansTest {
   type S = InMemory
   val imp  = new ExprImplicits[ S ]
   import imp._

   def run() {
      implicit val sys = InMemory()
      lazy val server: AuralSystem = AuralSystem().start().whenStarted { _ =>
         sys.step { implicit tx =>
            val group   = ProcGroup_.Modifiable[ S ]
            val transp  = Transport( group )
            /* val view = */ AuralPresentation.run( transp, server )
            test( group )
            transp.playing_=( true )
         }
      }
   }

   def test /* [ S <: Sys[ S ]] */( group: ProcGroup_.Modifiable[ S ])( implicit tx: S#Tx ) {
      val p1 = Proc[ S ]()
      val p2 = Proc[ S ]()

      group.add( Span.from( 0L ), p1 )
      group.add( Span.from( 0L ), p2 )

//      def ??? : Nothing = sys.error( "TODO" )

      val fScan1: Scan[ S ] = Scan_.Modifiable[ S ]
      p1.scans.add( "freq", fScan1 )
      val fScan2: Scan[ S ] = Scan_.Modifiable[ S ]
      p2.scans.add( "freq", fScan2 )
      for( s1 <- p1.scans.get( "freq" ); Scan_.Modifiable( s2 ) <- p2.scans.get( "freq" )) {
         s2.add( 0L, Scan_.Embedded( s1, 0L ))
      }
   }
}
