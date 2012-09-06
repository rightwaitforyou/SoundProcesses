package de.sciss.synth.proc

import de.sciss.lucre.stm.Sys

object Scans {
   trait Modifiable[ S <: Sys[ S ]] extends Scans[ S ] {
      def add( key: String, scan: Scan[ S ])( implicit tx: S#Tx ) : Unit
      def remove( key: String )( implicit tx: S#Tx ) : Boolean
   }
}
trait Scans[ S <: Sys[ S ]] {
   def get( key: String )( implicit tx: S#Tx ) : Option[ Scan[ S ]]
   def keys( implicit tx: S#Tx ): Set[ String ]
}
