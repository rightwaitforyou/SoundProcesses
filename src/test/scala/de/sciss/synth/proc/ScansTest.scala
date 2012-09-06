package de.sciss.synth.proc

import de.sciss.lucre.stm.Sys
import de.sciss.synth.expr.ExprImplicits

object ScansTest {
   def test[ S <: Sys[ S ]]( implicit tx: S#Tx ) {
      val p1 = Proc[ S ]()
      val p2 = Proc[ S ]()

      def ??? : Nothing = sys.error( "TODO" )

      val imp  = new ExprImplicits[ S ]
      import imp._

      val fScan1: Scan[ S ] = ???
      p1.scans.add( "freq", fScan1 )
      val fScan2: Scan[ S ] = ???
      p2.scans.add( "freq", fScan2 )
      for( s1 <- p1.scans.get( "freq" ); s2 <- p2.scans.get( "freq" )) { s2 match {
         case Scan_.Modifiable( scan2 ) => scan2.add( 0L, Scan_.Embedded( s1, 0L ))
         case _ =>
      }}
   }
}
