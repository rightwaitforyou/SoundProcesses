package de.sciss.lucre.expr

import de.sciss.lucre.stm.{Source, Sys}

object TimeSource {
   def apply[ S <: Sys[ S ]]( t: Expr[ S, Long ]) : TimeSource[ S ] = new Wrap( t )
//   def wrap[ S <: Sys[ S ]]( t: Expr[ S, Long ]) : TimeSource[ S ] = new Wrap( t )

   private final case class Wrap[ S <: Sys[ S ]]( time: Expr[ S, Long ]) extends TimeSource[ S ] {
//      def get( implicit tx: S#Tx ) : Expr[ S, Long ] = t
   }
}
trait TimeSource[ S <: Sys[ S ]] {
//   def tx: S#Tx
   def time: Expr[ S, Long ]
}