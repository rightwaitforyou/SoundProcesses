package de.sciss.synth.expr

import de.sciss.lucre.expr.Expr
import de.sciss.lucre.event.Event
import de.sciss.lucre.stm.Sys

object Bi {
   trait Var[ S <: Sys[ S ], A ] extends Bi[ S, A ] {
      def set( time: Expr[ S, Long ], value: Expr[ S, A ]) : Unit
   }
}
trait Bi[ S <: Sys[ S ], A ] {
   def get( t: Long ) : Expr[ S, A ]
   def changed: Event[ S, (Span, A), Bi[ S, A ]]
}
