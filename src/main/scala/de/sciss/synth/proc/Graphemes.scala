package de.sciss.synth.proc

import de.sciss.lucre.stm.Sys

object Graphemes {
   trait Modifiable[ S <: Sys[ S ]] extends Graphemes[ S ] {
      def add( key: String, grapheme: Grapheme[ S ])( implicit tx: S#Tx ) : Unit
      def remove( key: String )( implicit tx: S#Tx ) : Boolean
   }
}
trait Graphemes[ S <: Sys[ S ]] {
   def get( key: String )( implicit tx: S#Tx ) : Option[ Grapheme[ S ]]
   def keys( implicit tx: S#Tx ): Set[ String ]
   def valueAt( key: String, time: Long )( implicit tx: S#Tx ) : Option[ Grapheme.Value ]
}
