//package de.sciss.synth.proc
//
//import de.sciss.lucre.expr.Expr
//import de.sciss.lucre.stm.{Source, Sys}
//
//object Chronos {
//   def wrap[ S <: Sys[ S ]]( t: Expr[ S, Long ]) : Chronos[ S ] = new Wrap( t )
//
//   private final class Wrap[ S <: Sys[ S ]]( t: Expr[ S, Long ]) extends Chronos[ S ] {
//      def get( implicit tx: S#Tx ) : Expr[ S, Long ] = t
//   }
//}
//trait Chronos[ S <: Sys[ S ]] extends Source[ S#Tx, Expr[ S, Long ]]