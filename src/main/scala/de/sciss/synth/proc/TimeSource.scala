//package de.sciss.synth.proc
//
//import de.sciss.lucre.expr.Expr
//import de.sciss.lucre.stm.{Source, Sys}
//
//object TimeSource {
//   def wrap[ S <: Sys[ S ]]( t: Expr[ S, Long ]) : TimeSource[ S ] = new Wrap( t )
//
//   private final class Wrap[ S <: Sys[ S ]]( t: Expr[ S, Long ]) extends TimeSource[ S ] {
//      def get( implicit tx: S#Tx ) : Expr[ S, Long ] = t
//   }
//}
//trait TimeSource[ S <: Sys[ S ]] extends Source[ S#Tx, Expr[ S, Long ]]