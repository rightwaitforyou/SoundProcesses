package de.sciss.lucre.expr

import de.sciss.lucre.stm.Sys

object Chronos {
   def apply[ S <: Sys[ S ]]( t: Expr[ S, Long ]) : Chronos[ S ] = new Wrap( t )

   private final case class Wrap[ S <: Sys[ S ]]( time: Expr[ S, Long ]) extends Chronos[ S ]
}
trait Chronos[ S <: Sys[ S ]] {
   def time: Expr[ S, Long ]
}