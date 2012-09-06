package de.sciss.synth.proc

import de.sciss.lucre.stm.Sys

object ScansTest {
   def test[ S <: Sys[ S ]]( implicit tx: S#Tx ) {
      val p1 = Proc[ S ]()
      val p2 = Proc[ S ]()

      val fScan: Scan[ S ] = ???
      p1.scans.add( "freq", fScan )
   }
}
