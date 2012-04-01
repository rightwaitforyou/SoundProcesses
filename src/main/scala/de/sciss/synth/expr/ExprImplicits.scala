package de.sciss.synth.expr

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.expr.Expr

object ExprImplicits {
   implicit def stringConst[ S <: Sys[ S ]]( s: String ) : Expr[ S, String ] = Strings.newConst( s )
   implicit def booleanConst[ S <: Sys[ S ]]( b: Boolean ) : Expr[ S, Boolean ] = Booleans.newConst( b )
   implicit def doubleConst[ S <: Sys[ S ]]( d: Double ) : Expr[ S, Double ] = Doubles.newConst( d )
}
class ExprImplicits[ S <: Sys[ S ]] {
   implicit def stringConst( s: String ) : Expr[ S, String ] = Strings.newConst( s )
   implicit def stringOps[ A <% Expr[ S, String ]]( ex: A ) : Strings.Ops[ S ] = new Strings.Ops( ex )

   implicit def booleanConst( b: Boolean ) : Expr[ S, Boolean ] = Booleans.newConst( b )
   implicit def booleanOps[ A <% Expr[ S, Boolean ]]( ex: A ) : Booleans.Ops[ S ] = new Booleans.Ops( ex )

   implicit def doubleConst( d: Double ) : Expr[ S, Double ] = Doubles.newConst( d )
   implicit def doubleOps[ A <% Expr[ S, Double ]]( ex: A ) : Doubles.Ops[ S ] = new Doubles.Ops( ex )
}