package de.sciss.synth.expr

import de.sciss.lucre.{stm, expr, bitemp, event => evt}
import bitemp.{BiExpr, BiType, SpanLike, Span}
import evt.Sys
import expr.Expr

object ExprImplicits {
   implicit def stringConst[   S <: stm.Sys[ S ]]( s: String )   : Expr[ S, String   ] = Strings.newConst(   s )
   implicit def booleanConst[  S <: stm.Sys[ S ]]( b: Boolean )  : Expr[ S, Boolean  ] = Booleans.newConst(  b )
   implicit def doubleConst[   S <: stm.Sys[ S ]]( d: Double )   : Expr[ S, Double   ] = Doubles.newConst(   d )
   implicit def intConst[      S <: stm.Sys[ S ]]( i: Int )      : Expr[ S, Int      ] = Ints.newConst(      i )
   implicit def longConst[     S <: stm.Sys[ S ]]( n: Long )     : Expr[ S, Long     ] = Longs.newConst(     n )
   implicit def spanConst[     S <: stm.Sys[ S ]]( s: Span )     : Expr[ S, Span     ] = Spans.newConst(     s )
   implicit def spanLikeConst[ S <: stm.Sys[ S ]]( s: SpanLike ) : Expr[ S, SpanLike ] = SpanLikes.newConst( s )

//   trait LowPriority[ S <: Sys[ S ]] {
//      implicit def lowSpanLikeConst( s: Span ) : Expr[ S, SpanLike ] = SpanLikes.newConst( s )
////      implicit def lowSpanLikeOps( ex: Expr[ S, Span ])( implicit tx: S#Tx ) : SpanLikes.Ops[ S ] = new SpanLikes.Ops( ex )
////      implicit def lowDoubleConst( d: Float ) : Expr[ S, Double ] = Doubles.newConst( d )
//   }

   def apply[ S <: Sys[ S ]] : ExprImplicits[ S ] = Imp.asInstanceOf[ ExprImplicits[ S ]]

   private object Imp extends ExprImplicits[ evt.InMemory ]
}

/**
 * This class helps overcome the limitation of scala's implicit resolution -- by resolving an expression
 * system's type parameter `S`. There are two types of conversions, those that can be run from the underlying
 * expression type (e.g. `longOps1` provides operations which can be invoked with a plain `Long`), and those
 * that require an existing expression (e.g. `longOps2`). This is so that primitive standard operations remain
 * outside the implicit scope (e.g. addition on longs).
 */
class ExprImplicits[ S <: Sys[ S ]] /* extends ExprImplicits.LowPriority[ S ] */ {
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

   implicit def intConst( i: Int ) : Expr[ S, Int ] = Ints.newConst( i )
   implicit def intOps1[ A ]( ex: A )( implicit tx: S#Tx, view: A => Expr[ S, Int ]) : Ints.RichOps[ S ] =
      new Ints.RichOps( ex )
   implicit def intOps2( ex: Expr[ S, Int ])( implicit tx: S#Tx ) : Ints.Ops[ S ] = new Ints.Ops( ex )

   implicit def longConst( n: Long ) : Expr[ S, Long ] = Longs.newConst( n )
   implicit def longOps1[ A ]( ex: A )( implicit tx: S#Tx, view: A => Expr[ S, Long ]) : Longs.RichOps[ S ] =
      new Longs.RichOps( ex )
   implicit def longOps2( ex: Expr[ S, Long ])( implicit tx: S#Tx ) : Longs.Ops[ S ] = new Longs.Ops( ex )

   implicit def spanConst( s: Span ) : Expr[ S, Span ] = Spans.newConst( s )
   implicit def spanOps( ex: Expr[ S, Span ])( implicit tx: S#Tx ) : Spans.Ops[ S ] = new Spans.Ops( ex )
   implicit def spanLikeConst( s: SpanLike ) : Expr[ S, SpanLike ] = SpanLikes.newConst( s )
   implicit def spanLikeOps( ex: Expr[ S, SpanLike ])( implicit tx: S#Tx ) : SpanLikes.Ops[ S ] = new SpanLikes.Ops( ex )

   // ---- biexpr ----
   implicit def biExpr[ A, A1, T ]( tuple: (T, A1) )
                                  ( implicit tx: S#Tx, magType: BiType[ A ],
                                    timeView: T => Expr[ S, Long ],
                                    magView: A1 => Expr[ S, A ]) : BiExpr[ S, A ] =
      BiExpr[ S, A ]( timeView( tuple._1 ), magView( tuple._2 ))

   implicit def biExprSer[ A ]( implicit biType: BiType[ A ]) = BiExpr.serializer[ S, A ]
}