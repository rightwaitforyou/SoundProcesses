package de.sciss.lucre.expr

import de.sciss.lucre.stm.Sys

object Chronos {
//   def apply[ S <: Sys[ S ]]( t: Expr[ S, Long ]) : Chronos[ S ] = new Wrap( t )
   def apply[ S <: Sys[ S ]]( t: Long ) : Chronos[ S ] = new Wrap( t )

//   private final case class Wrap[ S <: Sys[ S ]]( time: Expr[ S, Long ]) extends Chronos[ S ]
   private final case class Wrap[ S <: Sys[ S ]]( tim: Long ) extends Chronos[ S ] {
      def time( implicit tx: S#Tx ) : Long = tim
   }
}
trait Chronos[ S <: Sys[ S ]] {
   def time( implicit tx: S#Tx ) : Long // Expr[ S, Long ]
}