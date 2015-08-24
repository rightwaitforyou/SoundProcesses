package de.sciss
package synth
package proc

import de.sciss.lucre.expr
import de.sciss.lucre.expr.Expr

import collection.immutable.{IndexedSeq => Vec}
import span.Span
import de.sciss.synth.Curve.step

/*
 To test only this suite:

 test-only de.sciss.synth.proc.GraphemeSerializationSpec

 */
class GraphemeSerializationSpec extends ConfluentEventSpec {
  "Grapheme" should "serialize and deserialize" in { system =>
    val gH = system.step { implicit tx =>
      val g = Grapheme[S](1)
      tx.newHandle(g)(Grapheme.Modifiable.serializer[S])
    }

    // import imp._
    import expr.Ops._
    // import ExprImplicits._

    type LE = Expr[S, Long]
    type GE = Expr[S, Grapheme.Value]

    system.step { implicit tx =>
      val g = gH()
      ??? // RRR g.add((1234L: LE) -> Grapheme.Expr.Curve[S](Grapheme.Value.Curve(5678.9 -> step)))
    }

    system.step { implicit tx =>
      val g = gH()
      assert(g.segment(0L) === None)
      assert(g.segment(2222L) === Some(Grapheme.Segment.Const(Span.from(1234L), Vec(5678.9))))
    }
  }
}
