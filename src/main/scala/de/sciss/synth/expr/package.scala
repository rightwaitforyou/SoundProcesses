//package de.sciss.synth
//
//import de.sciss.lucre.expr.Expr
//import de.sciss.lucre.stm.Sys
//
////package expr {
////   abstract private[synth] sealed class LowPriExprImplicit {
////      implicit def doubleOps1[ S <: Sys[ S ]]( ex: Expr[ S, Double ]) : Doubles.Ops[ S ] = new Doubles.Ops( ex )
////   }
////}
//
//package object expr /* extends LowPriExprImplicit */ {
//   implicit def stringConst[ S <: Sys[ S ]]( s: String ) : Expr[ S, String ] = Strings.newConst( s )
//   implicit def stringOps[ S <: Sys[ S ], A <% Expr[ S, String ]]( ex: A ) : Strings.Ops[ S ] = new Strings.Ops( ex )
//
//   implicit def booleanConst[ S <: Sys[ S ]]( b: Boolean ) : Expr[ S, Boolean ] = Booleans.newConst( b )
//   implicit def booleanOps[ S <: Sys[ S ], A <% Expr[ S, Boolean ]]( ex: A ) : Booleans.Ops[ S ] = new Booleans.Ops( ex )
//
//   implicit def doubleConst[ S <: Sys[ S ]]( d: Double ) : Expr[ S, Double ] = Doubles.newConst( d )
//   implicit def doubleOps[ S <: Sys[ S ], A <% Expr[ S, Double ]]( ex: A ) : Doubles.Ops[ S ] = new Doubles.Ops( ex )
//}