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

package de.sciss.synth.proc

import de.sciss.lucre.stm.{NoSys, Sys}
import de.sciss.synth.SynthGraph
import de.sciss.lucre.expr.Expr
import language.implicitConversions

object ExprImplicits {
  def apply[S <: Sys[S]]: ExprImplicits[S] = Imp.asInstanceOf[ExprImplicits[S]]
  private[this] val Imp = new ExprImplicits[NoSys]
}

/** This class helps overcome the limitation of scala's implicit resolution -- by resolving an expression
  * system's type parameter `S`. There are two types of conversions, those that can be run from the underlying
  * expression type (e.g. `longOps1` provides operations which can be invoked with a plain `Long`), and those
  * that require an existing expression (e.g. `longOps2`). This is so that primitive standard operations remain
  * outside the implicit scope (e.g. addition on longs).
  */
class ExprImplicits[S <: Sys[S]] protected /* extends de.sciss.lucre.synth.expr.ExprImplicits[S] */ {
  implicit final def synthGraphConst(s: SynthGraph    ): Expr[S, SynthGraph    ] = ??? // RRR SynthGraphs  .newConst(s)
  implicit final def graphemeConst  (v: Grapheme.Value): Expr[S, Grapheme.Value] = ??? // RRR Grapheme.Expr.newConst(v)
}