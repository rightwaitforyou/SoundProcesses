package de.sciss
package synth
package proc

import lucre.expr.Expr
import lucre.bitemp.Span
import collection.immutable.{IndexedSeq => IIdxSeq}

/**
 * To run only this suite:
 *
 * test-only de.sciss.synth.proc.GraphemeSpec
 */
class GraphemeSpec extends ConfluentEventSpec {
   type IntEx = Expr[ S, Int ]
   import imp._

   def ??? : Nothing = sys.error( "TODO" )

   import Grapheme.{Value, Modifiable, Update, Segment}

   "Grapheme" should "notify observers about all relevant events" in { system =>
      val obs  = new Observation[ S ]
      val gH   = system.step { implicit tx =>
         val g = Modifiable[ S ]
         g.changed.reactTx( obs.register )
         val res = tx.newHandle( g )( Modifiable.serializer[ S ])
         obs.assertEmpty()
         res
      }

      // adding constants
      system.step { implicit tx =>
         val g = gH.get

         g.add( 0L -> Value.Curve( 441.0 -> linShape ))
         obs.assertEquals(
            Update( g, IIdxSeq( Segment.Const( Span.from( 0L ), IIdxSeq( 441.0 ))))
         )
         obs.clear()

         g.add( 10000L, Value.Curve( 882.0 -> expShape ))
         val s0_10000 = Segment.Curve( Span( 0L, 10000L ), IIdxSeq( (441.0, 882.0, expShape) ))
         obs.assertEquals(
            Update( g, IIdxSeq( s0_10000,
                                Segment.Const( Span.from( 10000L ), IIdxSeq( 882.0 ))))
         )
         obs.clear()

         g.add( 20000L, Value.Curve( 123.4 -> sinShape, 567.8 -> sinShape ))
         obs.assertEquals(
            Update( g, IIdxSeq( Segment.Const( Span( 10000L, 20000L ), IIdxSeq( 882.0 )), // no curve if channel mismatch
                                Segment.Const( Span.from( 20000L ), IIdxSeq( 123.4, 567.8 ))))
         )
         obs.clear()

         g.add( 30000L, Value.Curve( 987.6 -> welchShape, 543.2 -> stepShape ))
         obs.assertEquals(
            Update( g, IIdxSeq( Segment.Curve( Span( 20000L, 30000L ), IIdxSeq( (123.4, 987.6, welchShape), (567.8, 543.2, stepShape) )),
                                Segment.Const( Span.from( 30000L ), IIdxSeq( 987.6, 543.2 ))))
         )
         obs.clear()

         // override a stereo signal with a mono signal
         g.add( 20000L, Value.Curve( 500.0 -> curveShape( -4f )))
         obs.assertEquals(
            Update( g, IIdxSeq( Segment.Curve( Span( 10000L, 20000L ), IIdxSeq( (882.0, 500.0, curveShape( -4f )))),
                                Segment.Const( Span( 20000L, 30000L ), IIdxSeq( 500.0 ))))
         )
         obs.clear()

         assert(  g.segment(    -1L ) === None )
         assert(  g.segment(     0L ) === Some( s0_10000 ))
         assert(  g.segment(  9999L ) === Some( s0_10000 ))
         assert( (g.segment( 10000L ) === Some( s0_10000 )).isDefined )

         assert( g.debugList() === List(
            s0_10000,
            Segment.Curve( Span( 10000L, 20000L ), IIdxSeq( (882.0, 500.0, curveShape( -4f )))),
            Segment.Const( Span( 20000L, 30000L ), IIdxSeq( 500.0 )),
            Segment.Const( Span.from( 30000L ), IIdxSeq( 987.6, 543.2 ))
         ))
      }

      // removals
      system.step { implicit tx =>

      }
   }
}
