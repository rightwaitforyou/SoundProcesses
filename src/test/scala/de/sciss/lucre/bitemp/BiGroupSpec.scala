package de.sciss.lucre.bitemp

import de.sciss.ConfluentEventSpec
import de.sciss.lucre.expr.Expr
import de.sciss.span.Span
import de.sciss.lucre.event
import de.sciss.synth.expr.SpanLikes
import collection.immutable.{IndexedSeq => Vec}
import scala.annotation.tailrec

/**
 * To run only this suite:
 *
 * test-only de.sciss.lucre.bitemp.BiGroupSpec
 */
class BiGroupSpec extends ConfluentEventSpec {
  type IntEx = Expr[S, Int]

  import imp._

  // "OutOfMemoryError"
  //  Span( 332000, 892000),
  //  Span( 341000, 943000),
  //  Span( 498000, 859000),
  //  Span( 562000,1066000)

  "BiGroup" should "report correct event successions" in { system =>
    val spans = Vec(
      // Span.All,
      // Span(  150000, 7020000),
      Span( 3300000, 8900000),
      //      Span( 3370000, 9420000),
      Span( 3400000, 9400000),
      Span( 4900000, 8500000),
      Span( 5600000,10600000)
      //      Span(7794219,10440219),
      //      Span(10088174,14870035),
      //      Span(10510112,11246044),
      //      Span(10723389,10998126)
      //      Span(10912234,11091796),
      //      Span(11772841,15633738),
      //      Span(12275921,14190692),
      //      Span(12571937,19584395),
      //      Span(12571937,19618564),
      //      Span(13781584,15317197),
      //      Span(14149413,14958486),
      //      Span(14892439,17574299),
      //      Span(15317197,15365248),
      //      Span(15365248,15645544),
      //      Span(15531230,17574299),
      //      Span(15645544,15842019),
      //      Span(15842019,15955205),
      //      Span(15955205,16180259),
      //      Span(16180259,18049306),
      //      Span(17576436,19193246),
      //      Span(17578570,19193246),
      //      Span(18774670,20236860),
      //      Span(18774670,24270017),
      //      Span(18798496,24335649),
      //      Span(18798496,24372145),
      //      Span(24042282,24796636),
      //      Span(24667937,25060737),
      //      Span(28604671,32149986),
      //      Span(28604671,32187329)
    )

    @tailrec def calcManual(res: Vec[Long] = Vec.empty): Vec[Long] = {
      val frame = res.lastOption.getOrElse(-1L) + 1
      val fut   = spans.filter(_.stop >= frame)
      if (fut.isEmpty) res else {
        val next  = fut.minBy(span => if (span.start >= frame) span.start else span.stop)
        val nextF = if (next.start >= frame) next.start else next.stop
        // println(s"Given $frame, next is $nextF")
        calcManual(res :+ nextF)
      }
    }

    val manual = calcManual()

    val dummy = event.Dummy[S, Unit]
    implicit val sl = SpanLikes

    system.step { implicit tx =>
      val g = BiGroup.Modifiable[S, Unit, Unit](_ => dummy)
      // g.add(Span.All, ())
      spans.foreach(g.add(_, ()))

      println(g.debugPrint)

      (-1L +: manual).sliding(2, 1).foreach { case Vec(pred, succ) =>
        println(s"Querying ${pred+1} - expecting $succ")
        val query = g.nearestEventAfter(pred + 1)
        assert(query === Some(succ))
      }
    }
  }
}
