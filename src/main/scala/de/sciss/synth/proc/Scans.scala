package de.sciss.synth.proc

import de.sciss.lucre.stm.Sys

object Scans {
   trait Modifiable[ S <: Sys[ S ]] extends Scans[ S ] {
      def add( key: String )( implicit tx: S#Tx ) : Scan[ S ]
      def remove( key: String )( implicit tx: S#Tx ) : Boolean
   }
}
trait Scans[ S <: Sys[ S ]] {
   def get( key: String )( implicit tx: S#Tx ) : Option[ Scan[ S ]]
   def keys( implicit tx: S#Tx ): Set[ String ]
//   def valueAt( key: String, time: Long )( implicit tx: S#Tx ) : Option[ Grapheme.Value[ S ]]
}
