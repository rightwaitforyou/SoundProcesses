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

      system.step { implicit tx =>
         val g = gH.get

         g.add( 0L -> Value.Curve( 441.0 -> linShape ))
         obs.assertEquals(
            Update( g, IIdxSeq( Segment.Const( Span.from( 0L ), IIdxSeq( 441.0 ))))
         )
         obs.clear()

         g.add( 10000L, Value.Curve( 882.0 -> expShape ))
         obs.assertEquals(
            Update( g, IIdxSeq( Segment.Curve( Span( 0L, 10000L ), IIdxSeq( (441.0, 882.0, expShape) )),
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

//         obs.print()

//         bip.add( tup1 )
//         obs.assertEquals(
//            BiPin.Added[ S, Int ]( bip, tup1, tup1 )
//         )
//         obs.clear()
//         assert( bip.valueAt( tup1._1 - 1 ) === None )
//         assert( bip.valueAt( tup1._1     ) === Some( tup1._2 ))
//         assert( bip.valueAt( tup1._1 + 1 ) === Some( tup1._2 ))
//
//         bip.add( tup2 )
//         obs.assertEquals(
//            BiPin.Added[ S, Int ]( bip, tup2, tup2 )
//         )
//         obs.clear()
//
//         bip.add( tup3 )
////         println( "at 10000 : " + bip.at( 10000L ))
//         // note: the shrunken regions are _not_ fired!
//         obs.assertEquals(
//            BiPin.Added[ S, Int ]( bip, tup3, tup3 )
//         )
//         obs.clear()
//
//         bip.add( tup4 )
//         obs.assertEquals(
//            BiPin.Added[ S, Int ]( bip, tup4, tup4 )
//         )
//         obs.clear()
//
//         assert( bip.valueAt( tup3._1 ) === Some( tup3._2 ))
//         bip.add( tup5 ) // should override the `3`
//         assert( bip.valueAt( tup3._1 ) === Some( tup5._2 ))
//         bip.add( tup6 ) // should override the `5`
//         assert( bip.valueAt( tup3._1 ) === Some( tup6._2 ))
//
//         assert( bip.intersect( tup3._1 ) === IIdxSeq[ BiExpr[ S, Int ]]( tup6, tup5, tup3 )) // recent values first
//
//         obs.assertEquals(
//            BiPin.Added[ S, Int ]( bip, tup5, tup5 ),
//            BiPin.Added[ S, Int ]( bip, tup6, tup6 )
//         )
//         obs.clear()
//
//         bip.remove( tup5 ) // should not be noticable
//         assert( bip.valueAt( tup3._1 ) === Some( tup6._2 ))
//         assert( bip.intersect( tup3._1 ) === IIdxSeq[ BiExpr[ S, Int ]]( tup6, tup3 ))
//
//         bip.remove( tup6 ) // should fall back to `3`
//         assert( bip.valueAt( tup3._1 ) === Some( tup3._2 ))
//         assert( bip.intersect( tup3._1 ) === IIdxSeq[ BiExpr[ S, Int ]]( tup3 ))
//
//         // tup5 removal not noticable!
//         obs.assertEquals(
//            BiPin.Removed[ S, Int ]( bip, tup6, tup6 )
//         )
//         obs.clear()
//
//         bip.remove( 15000L -> 11 )   // should be ignored
//         bip.remove( 15001L -> 3 )   // should be ignored
//         obs.assertEmpty()
//         assert( bip.valueAt( tup3._1 ) === Some( tup3._2 ))
//
//         bip.remove( tup3 )
//         obs.assertEquals(
//            BiPin.Removed[ S, Int ]( bip, tup3, tup3 )
//         )
//         obs.clear()
//         assert( bip.valueAt( tup3._1 ) === Some( tup1._2 ))
//
//         bip.remove( tup4 )
//         obs.assertEquals(
//            BiPin.Removed[ S, Int ]( bip, tup4, tup4 )
//         )
//         obs.clear()
//
//         bip.remove( tup2 )
//         bip.remove( tup1 )
//         obs.assertEquals(
//            BiPin.Removed[ S, Int ]( bip, tup2, tup2 ),
//            BiPin.Removed[ S, Int ]( bip, tup1, tup1 )
//         )
//         obs.clear()
//
//         assert( bip.intersect( 0L ).isEmpty && bip.intersect( 20000L ).isEmpty )
      }
   }
}
