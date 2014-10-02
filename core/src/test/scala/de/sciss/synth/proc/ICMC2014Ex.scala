package de.sciss.synth.proc

import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.Expr
import de.sciss.span.Span
import de.sciss.lucre.synth.expr
import de.sciss.lucre.bitemp

trait ICMC2014Ex[S <: Sys[S]] {
  val imp = ExprImplicits[S]
  import imp._

  def placeAfter(pred: Expr.Var[S, Span],
                 succ: Expr.Var[S, Span],
                 gap : Expr    [S, Long])
                (implicit tx: S#Tx): Unit = {
    val newStart = pred.stop + gap
    val newStop  = newStart + succ().length
    succ()       = Span(newStart, newStop)
  }
}
