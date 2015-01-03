/*
 *  ExprImplicits.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth
package expr

import de.sciss.lucre
import de.sciss.lucre.{bitemp, event => evt}
import bitemp.BiExpr
import de.sciss.lucre.expr.{ExprType1, Expr}
import language.implicitConversions
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth

object ExprImplicits {
  def apply[S <: evt.Sys[S]]: ExprImplicits[S] = Imp.asInstanceOf[ExprImplicits[S]]
  private[this] val Imp = new ExprImplicits[evt.InMemory]
}

/** This class helps overcome the limitation of Scala's implicit resolution -- by resolving an expression
  * system's type parameter `S`. There are two types of conversions, those that can be run from the underlying
  * expression type (e.g. `longOps1` provides operations which can be invoked with a plain `Long`), and those
  * that require an existing expression (e.g. `longOps2`). This is so that primitive standard operations remain
  * outside the implicit scope (e.g. addition on longs).
  */
class ExprImplicits[S <: evt.Sys[S]] protected /* extends ExprImplicits.LowPriority[ S ] */ {
  implicit final def stringConst(s: String): Expr[S, String] = lucre.expr.String.newConst(s)
  //   implicit def stringOps[ A ]( ex: A )( implicit view: A => Expr[ S, String ]) : lucre.expr.String.Ops[ S ] =
  //      new lucre.expr.String.Ops( ex )
  implicit final def stringOps(ex: Expr[S, String]): StringExtensions.Ops[S] = new StringExtensions.Ops(ex)

  implicit final def booleanConst(b: Boolean): Expr[S, Boolean] = lucre.expr.Boolean.newConst(b)
  implicit final def booleanOps1[A](ex: A)(implicit view: A => Expr[S, Boolean]): BooleanExtensions.Ops[S] =
    new BooleanExtensions.Ops(ex)

  implicit final def doubleConst(d: Double): Expr[S, Double] = lucre.expr.Double.newConst(d)
  implicit final def doubleOps1[A](ex: A)(implicit view: A => Expr[S, Double]): DoubleExtensions.RichOps[S] =
    new DoubleExtensions.RichOps(ex)
  implicit final def doubleOps2(ex: Expr[S, Double]): DoubleExtensions.Ops[S] = new DoubleExtensions.Ops(ex)

  implicit final def intConst(i: Int): Expr[S, Int] = lucre.expr.Int.newConst(i)
  implicit final def intOps1[A](ex: A)(implicit view: A => Expr[S, Int]): IntExtensions.RichOps[S] =
    new IntExtensions.RichOps(ex)
  implicit final def intOps2(ex: Expr[S, Int]): IntExtensions.Ops[S] = new IntExtensions.Ops(ex)

  implicit final def longConst(n: Long): Expr[S, Long] = lucre.expr.Long.newConst(n)
  implicit final def longOps1[A](ex: A)(implicit view: A => Expr[S, Long]): LongExtensions.RichOps[S] =
    new LongExtensions.RichOps(ex)
  implicit final def longOps2(ex: Expr[S, Long]): LongExtensions.Ops[S] = new LongExtensions.Ops(ex)

  implicit final def spanConst(s: Span): Expr[S, Span] = bitemp.Span.newConst(s)
  implicit final def spanOps (ex: Expr[S, Span]): SpanExtensions.Ops[S] = new SpanExtensions.Ops(ex)
  implicit final def spanOps2(s: Span.type): SpanExtensions.Ops2 = new SpanExtensions.Ops2(s)
  implicit final def spanLikeConst(s: SpanLike): Expr[S, SpanLike] = bitemp.SpanLike.newConst(s)
  implicit final def spanLikeOps(ex: Expr[S, SpanLike]): SpanLikeExtensions.Ops[S] = new SpanLikeExtensions.Ops(ex)

  // ...disabled in this sub project
  // implicit def synthGraphConst(s: SynthGraph): Expr[S, SynthGraph] = SynthGraphs.newConst(s)

  implicit final def curveConst(c: synth.Curve): Expr[S, synth.Curve] = Curve.newConst(c)

  // ---- biexpr ----
  implicit final def biExpr[A, A1, T](tuple: (T, A1))
                               (implicit tx: S#Tx, magType: ExprType1[A],
                                timeView: T => Expr[S, Long],
                                magView: A1 => Expr[S, A]): BiExpr[S, A] =
    BiExpr[S, A](timeView(tuple._1), magView(tuple._2))

  //  implicit def biExprSer[A](implicit biType: BiType[A]) = BiExpr.serializer[S, A]

  // ...disabled in this sub project
  // implicit def graphemeConst(v: Grapheme.Value): Expr[S, Grapheme.Value] = Grapheme.Elem.newConst(v)
}