package de.sciss.synth

import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.Sys

package object expr {
   implicit def stringConst[ S <: Sys[ S ]]( s: String ) : Expr[ S, String ] = Strings.newConst( s )
   implicit def stringOps[ S <: Sys[ S ], A <% Expr[ S, String ]]( ex: A ) : Strings.Ops[ S ] = new Strings.Ops( ex )
}