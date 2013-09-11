package de.sciss.synth.proc

import de.sciss.lucre.{event => evt}
import de.sciss.synth.SynthGraph
import de.sciss.lucre.expr.Expr
import language.implicitConversions

object ExprImplicits {
  def apply[S <: evt.Sys[S]]: ExprImplicits[S] = Imp.asInstanceOf[ExprImplicits[S]]
  private object Imp extends ExprImplicits[evt.InMemory]
}

/** This class helps overcome the limitation of scala's implicit resolution -- by resolving an expression
  * system's type parameter `S`. There are two types of conversions, those that can be run from the underlying
  * expression type (e.g. `longOps1` provides operations which can be invoked with a plain `Long`), and those
  * that require an existing expression (e.g. `longOps2`). This is so that primitive standard operations remain
  * outside the implicit scope (e.g. addition on longs).
  */
class ExprImplicits[S <: evt.Sys[S]] protected extends de.sciss.lucre.synth.expr.ExprImplicits[S] {
  implicit def synthGraphConst(s: SynthGraph    ): Expr[S, SynthGraph    ] = SynthGraphs  .newConst(s)
  implicit def graphemeConst  (v: Grapheme.Value): Expr[S, Grapheme.Value] = Grapheme.Elem.newConst(v)
}