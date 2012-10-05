package de.sciss
package lucre
package bitemp

import collection.immutable.{IndexedSeq => IIdxSeq}
import expr.Expr

/**
 * To run only this suite:
 *
 * test-only de.sciss.lucre.bitemp.BiPinSpec
 */
class BiPinSpec extends ConfluentEventSpec {
   "BiPin" should "notify observers about all relevant events" in { system =>
      val obs  = new Observation[ S ]
      val bipH = system.step { implicit tx =>
         val bip = BiPin.Expr.Modifiable[ S, Int ]
         bip.changed.reactTx( obs.register )
         val res = tx.newHandle( bip )( BiPin.Expr.Modifiable.serializer[ S, Int ])
         obs.assertEmpty()
         res
      }

      type IntEx = Expr[ S, Int ]

      import imp._

      system.step { implicit tx =>
         val bip = bipH.get
         bip.add( 10000L, 1 )
         obs.assertEquals(
            BiPin.Collection( bip, IIdxSeq( Span.from( 10000L ) -> (1: IntEx) ))
         )
         obs.clear()

         bip.add( 5000L, 2 )
         obs.assertEquals(
            BiPin.Collection( bip, IIdxSeq( Span( 5000L, 10000L ) -> (2: IntEx) ))
         )
         obs.clear()

         bip.add( 15000L, 3 )

//         println( "at 10000 : " + bip.at( 10000L ))

         obs.assertEquals(
            BiPin.Collection( bip, IIdxSeq( Span( 10000L, 15000L ) -> (1: IntEx),
                                            Span.from(    15000L ) -> (3: IntEx) ))
         )
      }
   }
}
