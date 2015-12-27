//package de.sciss
//package synth
//package proc
//
//import de.sciss.lucre.expr.{DoubleObj, LongObj}
//import de.sciss.span.Span
//import de.sciss.synth.Curve.step
//
//import scala.collection.immutable.{IndexedSeq => Vec}
//
///*
// To test only this suite:
//
// test-only de.sciss.synth.proc.GraphemeSerializationSpec
//
// */
//class GraphemeSerializationSpec extends ConfluentEventSpec {
//  "Grapheme" should "serialize and deserialize" in { system =>
//    val gH = system.step { implicit tx =>
//      val g = Grapheme[S](1)
//      tx.newHandle(g)
//    }
//
//    // import imp._
//    // import ExprImplicits._
//
//    type LE = LongObj[S]
//    type DE = DoubleObj[S]
//    type GE = Grapheme.Expr[S]
//
//    system.step { implicit tx =>
//      val g = gH()
//      val time = 1234L: LE
//      val mag  = Grapheme.Expr.Curve[S]((5678.9: DE) -> step)
//      g.add(time, mag)
//    }
//
//    system.step { implicit tx =>
//      val g = gH()
//      assert(g.segment(0L) === None)
//      assert(g.segment(2222L) === Some(Grapheme.Segment.Const(Span.from(1234L), Vec(5678.9))))
//    }
//  }
//}
