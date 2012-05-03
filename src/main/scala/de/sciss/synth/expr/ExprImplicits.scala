package de.sciss.synth.expr

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.expr.Expr

object ExprImplicits {
   implicit def stringConst[  S <: Sys[ S ]]( s: String )  : Expr[ S, String  ] = Strings.newConst(  s )
   implicit def booleanConst[ S <: Sys[ S ]]( b: Boolean ) : Expr[ S, Boolean ] = Booleans.newConst( b )
   implicit def doubleConst[  S <: Sys[ S ]]( d: Double )  : Expr[ S, Double  ] = Doubles.newConst(  d )
   implicit def longConst[    S <: Sys[ S ]]( n: Long )    : Expr[ S, Long    ] = Longs.newConst(    n )
}
class ExprImplicits[ S <: Sys[ S ]] {
   implicit def stringConst( s: String ) : Expr[ S, String ] = Strings.newConst( s )
//   implicit def stringOps[ A ]( ex: A )( implicit tx: S#Tx, view: A => Expr[ S, String ]) : Strings.Ops[ S ] =
//      new Strings.Ops( ex )
   implicit def stringOps( ex: Expr[ S, String ])( implicit tx: S#Tx ) : Strings.Ops[ S ] = new Strings.Ops( ex )

   implicit def booleanConst( b: Boolean ) : Expr[ S, Boolean ] = Booleans.newConst( b )
   implicit def booleanOps[ A <% Expr[ S, Boolean ]]( ex: A ) : Booleans.Ops[ S ] = new Booleans.Ops( ex )

   implicit def doubleConst( d: Double ) : Expr[ S, Double ] = Doubles.newConst( d )
   implicit def doubleOps1[ A ]( ex: A )( implicit tx: S#Tx, view: A => Expr[ S, Double ]) : Doubles.RichOps[ S ] =
      new Doubles.RichOps( ex )
   implicit def doubleOps2( ex: Expr[ S, Double ])( implicit tx: S#Tx ) : Doubles.Ops[ S ] = new Doubles.Ops( ex )

   implicit def longConst( n: Long ) : Expr[ S, Long ] = Longs.newConst( n )
   implicit def longOps1[ A ]( ex: A )( implicit tx: S#Tx, view: A => Expr[ S, Long ]) : Longs.RichOps[ S ] =
      new Longs.RichOps( ex )
   implicit def longOps2( ex: Expr[ S, Long ])( implicit tx: S#Tx ) : Longs.Ops[ S ] = new Longs.Ops( ex )
}