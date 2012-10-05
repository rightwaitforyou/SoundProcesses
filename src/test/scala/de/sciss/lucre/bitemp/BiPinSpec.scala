package de.sciss
package lucre
package bitemp

import collection.immutable.{IndexedSeq => IIdxSeq}
import expr.Expr
import synth.expr.{Ints, Longs}
import event.Change

/**
 * To run only this suite:
 *
 * test-only de.sciss.lucre.bitemp.BiPinSpec
 */
class BiPinSpec extends ConfluentEventSpec {
   type IntEx = Expr[ S, Int ]
   import imp._

   "BiPin" should "notify observers about all relevant collection events" in { system =>
      val obs  = new Observation[ S ]
      val bipH = system.step { implicit tx =>
         val bip = BiPin.Expr.Modifiable[ S, Int ]
         bip.changed.reactTx( obs.register )
         val res = tx.newHandle( bip )( BiPin.Expr.Modifiable.serializer[ S, Int ])
         obs.assertEmpty()
         res
      }

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
         // note: the shrunken regions are _not_ fired!
         obs.assertEquals(
            BiPin.Collection( bip, IIdxSeq( /* Span( 10000L, 15000L ) -> (1: IntEx), */
                                            Span.from( 15000L ) -> (3: IntEx) ))
         )
         obs.clear()

         bip.add( 20000L, 4 )
         obs.assertEquals(
            BiPin.Collection( bip, IIdxSeq( Span.from( 20000L ) -> (4: IntEx) ))
         )
         obs.clear()

         bip.add( 15000L, 5 ) // should override the `3`
         bip.add( 15000L, 6 ) // should override the `5`
         obs.assertEquals(
            BiPin.Collection( bip, IIdxSeq( Span( 15000L, 20000L ) -> (5: IntEx) )),
            BiPin.Collection( bip, IIdxSeq( Span( 15000L, 20000L ) -> (6: IntEx) ))
         )
         obs.clear()

         bip.remove( 15000L, 5 ) // should not be noticable
         bip.remove( 15000L, 6 ) // should fall back to `3`
         obs.assertEquals(
            BiPin.Collection( bip, IIdxSeq( Span( 15000L, 20000L ) -> (3: IntEx) ))
         )
         obs.clear()

         bip.remove( 15000L, 11 )   // should be ignored
         bip.remove( 15001L,  3 )   // should be ignored
         obs.assertEmpty()

         bip.remove( 15000L, 3 )
         obs.assertEquals(
            BiPin.Collection( bip, IIdxSeq( Span( 10000L, 20000L ) -> (1: IntEx) ))
         )
         obs.clear()

         bip.remove( 20000L, 4 )
         obs.assertEquals(
            BiPin.Collection( bip, IIdxSeq( Span.from( 10000L ) -> (1: IntEx) ))
         )
         obs.clear()

         // since there is no previous pin, should not fire
         // (this is problematic for the curve usage, but
         //  for the moment, let's just keep it simple)
         bip.remove( 5000L, 2 )
         bip.remove( 10000L, 1 )
         obs.assertEmpty()

         assert( bip.intersect( 0L ).isEmpty && bip.intersect( 20000L ).isEmpty )
      }
   }

   "BiPin" should "notify observers about all relevant element events" in { system =>
      val obs  = new Observation[ S ]
      val bipH = system.step { implicit tx =>
         val bip = BiPin.Expr.Modifiable[ S, Int ]
         bip.changed.reactTx( obs.register )
         val res = tx.newHandle( bip )( BiPin.Expr.Modifiable.serializer[ S, Int ])
         obs.assertEmpty()
         res
      }

      implicit val intVarSer  = Ints.varSerializer[ S ]
      implicit val longVarSer = Longs.varSerializer[ S ]

//      confluent.showLog = true

      val (timeH, exprH) = system.step { implicit tx =>
         // partial currently broken
//         val time = Longs.newVar[ S ]( 10000L )
//         val expr = Ints.newVar[ S ]( 4 )
         val time = Longs.newConfluentVar[ S ]( 10000L )
         val expr = Ints.newConfluentVar[ S ]( 4 )
         tx.newHandle( time ) -> tx.newHandle( expr )
      }

//      confluent.showLog = false

      system.step { implicit tx =>
         val bip  = bipH.get
         bip.add(     0L, 1 )
         bip.add( 20000L, 2 )
         obs.assertEquals(
            BiPin.Collection( bip, IIdxSeq( Span.from(     0L ) -> (1: IntEx) )),
            BiPin.Collection( bip, IIdxSeq( Span.from( 20000L ) -> (2: IntEx) ))
         )
         obs.clear()

         val time = timeH.get
         val expr = exprH.get
         bip.add( time, 3 )
         bip.add( 30000L, expr )
         obs.assertEquals(
            BiPin.Collection( bip, IIdxSeq( Span( 10000L, 20000L ) -> (3: IntEx) )),
            BiPin.Collection( bip, IIdxSeq( Span.from( 30000L ) -> expr ))
         )
         obs.clear()
      }

      system.step { implicit tx =>
         val bip  = bipH.get
         val time = timeH.get
         val expr = exprH.get

         expr.set( 5 )
         obs.assertEquals(
            BiPin.Element( bip, IIdxSeq( expr -> Change( 4, 5 )))
         )
         obs.clear()

         time.set( 15000L )
         obs.assertEquals(
            BiPin.Element( bip, IIdxSeq( expr -> Change( 4, 5 )))
         )
         obs.clear()

//         time.set( -5000L )
//         time.set( 25000L )
//         time.set( 35000L )
//         expr.set( 6 )
      }
   }
}
