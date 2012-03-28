package de.sciss.synth

import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.Sys

package object expr {
   implicit def stringConst[ S <: Sys[ S ]]( s: String ) : Expr[ S, String ] = Strings.newConst( s )
   implicit def stringOps[ S <: Sys[ S ], A <% Expr[ S, String ]]( ex: A ) : Strings.Ops[ S ] = new Strings.Ops( ex )

   implicit def booleanConst[ S <: Sys[ S ]]( b: Boolean ) : Expr[ S, Boolean ] = Booleans.newConst( b )
   implicit def booleanOps[ S <: Sys[ S ], A <% Expr[ S, Boolean ]]( ex: A ) : Booleans.Ops[ S ] = new Booleans.Ops( ex )
}