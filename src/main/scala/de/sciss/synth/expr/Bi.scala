package de.sciss.synth.expr

import de.sciss.lucre.expr.Expr
import de.sciss.lucre.event.Event
import de.sciss.lucre.stm.Sys

object Bi {
   def newVar[ S <: Sys[ S ], A ]( init: Expr[ S, A ])( implicit tx: S#Tx ) : Var[ S, A ] = new VarImpl[ S, A ]( tx )

   trait Var[ S <: Sys[ S ], A ] extends Bi[ S, A ] {
      def set( time: Expr[ S, Long ], value: Expr[ S, A ]) : Unit
   }

   private final class VarImpl[ S <: Sys[ S ], A ]( tx0: S#Tx ) extends Var[ S, A ] {
      def get( time: Long )( implicit tx: S#Tx ) : Expr[ S, A ] = sys.error( "TODO" )

      def at( time: Expr[ S, Long ])( implicit tx: S#Tx ) : Expr[ S, A ] = sys.error( "TODO" )

      def changed : Event[ S, (Span, A), Bi[ S, A ]] = sys.error( "TODO" )

      def set( time: Expr[ S, Long ], value: Expr[ S, A ]) {
         sys.error( "TODO" )
      }
   }
}
trait Bi[ S <: Sys[ S ], A ] {
   def get( time: Long )( implicit tx: S#Tx ) : Expr[ S, A ]
   def at( time: Expr[ S, Long ])( implicit tx: S#Tx ) : Expr[ S, A ]
   def changed : Event[ S, (Span, A), Bi[ S, A ]]
}
