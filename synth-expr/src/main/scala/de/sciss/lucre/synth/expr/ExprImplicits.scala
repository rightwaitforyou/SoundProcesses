/*
 *  ExprImplicits.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth
package expr

import de.sciss.lucre.{expr, bitemp, event => evt}
import bitemp.{BiExpr, BiType}
import expr.Expr
import language.implicitConversions
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth

object ExprImplicits {
  def apply[S <: evt.Sys[S]]: ExprImplicits[S] = Imp.asInstanceOf[ExprImplicits[S]]
  private object Imp extends ExprImplicits[evt.InMemory]
}

/**
 * This class helps overcome the limitation of scala's implicit resolution -- by resolving an expression
 * system's type parameter `S`. There are two types of conversions, those that can be run from the underlying
 * expression type (e.g. `longOps1` provides operations which can be invoked with a plain `Long`), and those
 * that require an existing expression (e.g. `longOps2`). This is so that primitive standard operations remain
 * outside the implicit scope (e.g. addition on longs).
 */
class ExprImplicits[S <: evt.Sys[S]] protected /* extends ExprImplicits.LowPriority[ S ] */ {
  implicit def stringConst(s: String): Expr[S, String] = Strings.newConst(s)
  //   implicit def stringOps[ A ]( ex: A )( implicit tx: S#Tx, view: A => Expr[ S, String ]) : Strings.Ops[ S ] =
  //      new Strings.Ops( ex )
  implicit def stringOps(ex: Expr[S, String])(implicit tx: S#Tx): Strings.Ops[S] = new Strings.Ops(ex)

  implicit def booleanConst(b: Boolean): Expr[S, Boolean] = Booleans.newConst(b)
  implicit def booleanOps[A <% Expr[S, Boolean]](ex: A): Booleans.Ops[S] = new Booleans.Ops(ex)

  implicit def doubleConst(d: Double): Expr[S, Double] = Doubles.newConst(d)
  implicit def doubleOps1[A](ex: A)(implicit tx: S#Tx, view: A => Expr[S, Double]): Doubles.RichOps[S] =
    new Doubles.RichOps(ex)
  implicit def doubleOps2(ex: Expr[S, Double])(implicit tx: S#Tx): Doubles.Ops[S] = new Doubles.Ops(ex)

  implicit def intConst(i: Int): Expr[S, Int] = Ints.newConst(i)
  implicit def intOps1[A](ex: A)(implicit tx: S#Tx, view: A => Expr[S, Int]): Ints.RichOps[S] =
    new Ints.RichOps(ex)
  implicit def intOps2(ex: Expr[S, Int])(implicit tx: S#Tx): Ints.Ops[S] = new Ints.Ops(ex)

  implicit def longConst(n: Long): Expr[S, Long] = Longs.newConst(n)
  implicit def longOps1[A](ex: A)(implicit tx: S#Tx, view: A => Expr[S, Long]): Longs.RichOps[S] =
    new Longs.RichOps(ex)
  implicit def longOps2(ex: Expr[S, Long])(implicit tx: S#Tx): Longs.Ops[S] = new Longs.Ops(ex)

  implicit def spanConst(s: Span): Expr[S, Span] = Spans.newConst(s)
  implicit def spanOps(ex: Expr[S, Span])(implicit tx: S#Tx): Spans.Ops[S] = new Spans.Ops(ex)
  implicit def spanLikeConst(s: SpanLike): Expr[S, SpanLike] = SpanLikes.newConst(s)
  implicit def spanLikeOps(ex: Expr[S, SpanLike])(implicit tx: S#Tx): SpanLikes.Ops[S] = new SpanLikes.Ops(ex)

  // ...disabled in this sub project
  // implicit def synthGraphConst(s: SynthGraph): Expr[S, SynthGraph] = SynthGraphs.newConst(s)

  implicit def curveConst(c: synth.Curve): Expr[S, synth.Curve] = Curves.newConst(c)

  // ---- biexpr ----
  implicit def biExpr[A, A1, T](tuple: (T, A1))
                               (implicit tx: S#Tx, magType: BiType[A],
                                timeView: T => Expr[S, Long],
                                magView: A1 => Expr[S, A]): BiExpr[S, A] =
    BiExpr[S, A](timeView(tuple._1), magView(tuple._2))

  implicit def biExprSer[A](implicit biType: BiType[A]) = BiExpr.serializer[S, A]

  // ...disabled in this sub project
  // implicit def graphemeConst(v: Grapheme.Value): Expr[S, Grapheme.Value] = Grapheme.Elem.newConst(v)
}