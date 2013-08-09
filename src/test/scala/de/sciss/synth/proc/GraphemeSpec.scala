package de.sciss
package synth
package proc

import lucre.expr.Expr
import collection.immutable.{IndexedSeq => Vec}
import expr.{Doubles, Longs}
import span.Span
import de.sciss.synth.Curve.{parametric, step, welch, sine, exponential, linear}

/**
 * To run only this suite:
 *
 * test-only de.sciss.synth.proc.GraphemeSpec
 */
class GraphemeSpec extends ConfluentEventSpec {
  import imp._

  import Grapheme.{Value, Modifiable, Update, Segment, Elem, TimedElem}

  "Grapheme" should "notify observers about all relevant events" in { system =>
    val obs = new Observation[S]
    val gH  = system.step { implicit tx =>
      val g   = Modifiable[S]
      g.changed.react(obs.register)
      val res = tx.newHandle(g)(Modifiable.serializer[S])
      obs.assertEmpty()
      res
    }

    val (e1, e2, e3, e4, e5) = system.step { implicit tx =>
      ((    0L -> Value.Curve(441.0 -> linear))               : TimedElem[S],
       (10000L -> Value.Curve(882.0 -> exponential))          : TimedElem[S],
       (20000L -> Value.Curve(123.4 -> sine , 567.8 -> sine)) : TimedElem[S],
       (30000L -> Value.Curve(987.6 -> welch, 543.2 -> step)) : TimedElem[S],
       (20000L -> Value.Curve(500.0 -> parametric(-4f)))      : TimedElem[S]
      )
    }

    // adding constants
      system.step { implicit tx =>
         val g = gH()

         g.add( e1 )
         obs.assertEquals(
            Update( g, Vec( Segment.Const( Span.from( 0L ), Vec( 441.0 ))))
         )
         obs.clear()

         g.add( e2 )
         val s0_10000 = Segment.Curve( Span( 0L, 10000L ), Vec( (441.0, 882.0, exponential) ))
         obs.assertEquals(
            Update( g, Vec( s0_10000,
                                Segment.Const( Span.from( 10000L ), Vec( 882.0 ))))
         )
         obs.clear()

         g.add( e3 )
         obs.assertEquals(
            Update( g, Vec( Segment.Const( Span( 10000L, 20000L ), Vec( 882.0 )), // no curve if channel mismatch
                                Segment.Const( Span.from( 20000L ), Vec( 123.4, 567.8 ))))
         )
         obs.clear()

         g.add( e4 )
         obs.assertEquals(
            Update( g, Vec( Segment.Curve( Span( 20000L, 30000L ), Vec( (123.4, 987.6, welch), (567.8, 543.2, step) )),
                                Segment.Const( Span.from( 30000L ), Vec( 987.6, 543.2 ))))
         )
         obs.clear()

         // override a stereo signal with a mono signal
         g.add( e5 )
         obs.assertEquals(
            Update( g, Vec( Segment.Curve( Span( 10000L, 20000L ), Vec( (882.0, 500.0, parametric(-4f)))),
                                Segment.Const( Span( 20000L, 30000L ), Vec( 500.0 ))))
         )
         obs.clear()

         assert(  g.segment(    -1L ) === None )
         assert(  g.segment(     0L ) === Some( s0_10000 ))
         assert(  g.segment(  9999L ) === Some( s0_10000 ))
         assert( (g.segment( 10000L ) === Some( s0_10000 )).isDefined )

         assert( g.debugList() === List(
            s0_10000,
            Segment.Curve( Span( 10000L, 20000L ), Vec( (882.0, 500.0, parametric( -4f )))),
            Segment.Const( Span( 20000L, 30000L ), Vec( 500.0 )),
            Segment.Const( Span.from( 30000L ), Vec( 987.6, 543.2 ))
         ))
      }

      // removals
      system.step { implicit tx =>
         val g = gH()
//         println( g.debugList() )
         assert( g.remove( e3 ))  // assert it was found
         obs.assertEmpty() // ... but it was hidden

         assert( !g.remove( e5.timeValue - 1, e5.mag )) // assert it was not found
         assert(  g.remove( e5 )) // assert it was found
         obs.assertEquals(
            Update( g, Vec( Segment.Const( Span( 10000L, 30000L ), Vec( 882.0 ))))
         )
         obs.clear()

         // removing first element should dispatch an undefined segment
         g.remove( e1 )
         obs.assertEquals(
            Update( g, Vec( Segment.Undefined( Span( 0L, 10000L ))))
         )
         obs.clear()

         g.remove( e4 )
         obs.assertEquals(
            Update( g, Vec( Segment.Const( Span.from( 10000L ), Vec( 882.0 ))))
         )
         obs.clear()

         g.remove( e2 )
         obs.assertEquals(
            Update( g, Vec( Segment.Undefined( Span.from( 10000L ))))
         )
         obs.clear()

         assert( g.debugList() === Nil )
      }

      // ok, now test with non-constant expressions
      system.step { implicit tx =>
         val g       = gH()
         val time1   = Longs.newVar[ S ](      0L)
         val mag1    = Doubles.newVar[ S ]( 1234.5)
         val value1  = Elem.Curve( mag1 -> linear)
         val elem1: TimedElem[ S ] = time1 -> value1

         val time2   = Longs.newVar[ S ]( 10000L )
         val mag2    = Doubles.newVar[ S ]( 6789.0 )
         val value2  = Elem.Curve( mag2 -> linear )
         val elem2: TimedElem[ S ] = time2 -> value2

         val time3   = time2 + 1000L
         val mag3    = mag1 + 1000.0
         val value3  = Elem.Curve( mag3 -> linear )
         val elem3: TimedElem[ S ] = time3 -> value3

         g.add( elem1 )
         g.add( elem2 )
         g.add( elem3 )

        obs.assertEquals(
          Update(g, Vec(
            Segment.Const(Span.from(0L), Vector(1234.5))
          )),
          Update(g, Vec(
            Segment.Curve(Span(0L, 10000L), Vector((1234.5, 6789.0, linear))),
            Segment.Const(Span.from(10000L), Vector(6789.0))
          )),
          Update(g, Vec(
            Segment.Curve(Span(10000L, 11000L), Vector((6789.0, 2234.5, linear))),
            Segment.Const(Span.from(11000L), Vector(2234.5))
          ))
        )
        obs.clear()

         time1() = 2000L
         obs.assertEquals(
            Update( g, Vec(
               Segment.Undefined( Span( 0L, 2000L )),
               Segment.Curve( Span( 2000L, 10000L ), Vector( (1234.5, 6789.0, linear) ))
            ))
         )
//         obs.print()
         obs.clear()

         mag1() = 666.6
         obs.assertEquals(
            Update( g, Vec (
               Segment.Curve( Span( 2000L, 10000L ), Vector( (666.6, 6789.0, linear) )),
               Segment.Curve( Span( 10000L, 11000L ), Vector( (6789.0, 1666.6, linear) )),
               Segment.Const( Span.from( 11000L ), Vector( 1666.6 ))
            ))
         )
         obs.clear()

         time2() = 11000L
         obs.assertEquals(
            Update( g, Vec (
               Segment.Curve( Span( 2000L, 11000L ), Vector( (666.6, 6789.0, linear) )),
               Segment.Curve( Span( 11000L, 12000L ), Vector( (6789.0, 1666.6, linear) )),
               Segment.Const( Span.from( 12000L ), Vector( 1666.6 ))
            ))
         )
         obs.clear()
      }
   }
}
