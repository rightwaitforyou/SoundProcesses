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

   def ??? : Nothing = sys.error( "TODO" )

   "BiPin" should "notify observers about all relevant collection events" in { system =>
      val obs  = new Observation[ S ]
      val bipH = system.step { implicit tx =>
         val bip = BiPin.Modifiable[ S, Int ]
         bip.changed.reactTx( obs.register )
         val res = tx.newHandle( bip )( BiPin.Modifiable.serializer[ S, Int ])
         obs.assertEmpty()
         res
      }

      val tup1 = 10000L -> 1
      val tup2 =  5000L -> 2
      val tup3 = 15000L -> 3
      val tup4 = 20000L -> 4
      val tup5 = 15000L -> 5
      val tup6 = 15000L -> 6

      system.step { implicit tx =>
         val bip = bipH.get
         bip.add( tup1 )
         obs.assertEquals(
//            BiPin.Collection( bip, IIdxSeq( Span.from( 10000L ) -> (1: IntEx) ))
            BiPin.Added[ S, Int ]( bip, tup1, tup1 )
         )
         obs.clear()
         assert( bip.valueAt( tup1._1 - 1 ) === None )
         assert( bip.valueAt( tup1._1     ) === Some( tup1._2 ))
         assert( bip.valueAt( tup1._1 + 1 ) === Some( tup1._2 ))

         bip.add( tup2 )
         obs.assertEquals(
//            BiPin.Collection( bip, IIdxSeq( Span( 5000L, 10000L ) -> (2: IntEx) ))
            BiPin.Added[ S, Int ]( bip, tup2, tup2 )
         )
         obs.clear()

         bip.add( tup3 )
//         println( "at 10000 : " + bip.at( 10000L ))
         // note: the shrunken regions are _not_ fired!
         obs.assertEquals(
//            BiPin.Collection( bip, IIdxSeq( /* Span( 10000L, 15000L ) -> (1: IntEx), */
//                                            Span.from( 15000L ) -> (3: IntEx) ))
            BiPin.Added[ S, Int ]( bip, tup3, tup3 )
         )
         obs.clear()

         bip.add( tup4 )
         obs.assertEquals(
//            BiPin.Collection( bip, IIdxSeq( Span.from( 20000L ) -> (4: IntEx) ))
            BiPin.Added[ S, Int ]( bip, tup4, tup4 )
         )
         obs.clear()

         assert( bip.valueAt( tup3._1 ) === Some( tup3._2 ))
         bip.add( tup5 ) // should override the `3`
         assert( bip.valueAt( tup3._1 ) === Some( tup5._2 ))
         bip.add( tup6 ) // should override the `5`
         assert( bip.valueAt( tup3._1 ) === Some( tup6._2 ))

         assert( bip.intersect( tup3._1 ) === IIdxSeq[ BiExpr[ S, Int ]]( tup6, tup5, tup3 )) // recent values first

         obs.assertEquals(
//            BiPin.Collection( bip, IIdxSeq( Span( 15000L, 20000L ) -> (5: IntEx) )),
//            BiPin.Collection( bip, IIdxSeq( Span( 15000L, 20000L ) -> (6: IntEx) ))
            BiPin.Added[ S, Int ]( bip, tup5, tup5 ),
            BiPin.Added[ S, Int ]( bip, tup6, tup6 )
         )
         obs.clear()

         bip.remove( tup5 ) // should not be noticable
         assert( bip.valueAt( tup3._1 ) === Some( tup6._2 ))
         assert( bip.intersect( tup3._1 ) === IIdxSeq[ BiExpr[ S, Int ]]( tup6, tup3 ))

         bip.remove( tup6 ) // should fall back to `3`
         assert( bip.valueAt( tup3._1 ) === Some( tup3._2 ))
         assert( bip.intersect( tup3._1 ) === IIdxSeq[ BiExpr[ S, Int ]]( tup3 ))

         // tup5 removal not noticable!
         obs.assertEquals(
//            BiPin.Collection( bip, IIdxSeq( Span( 15000L, 20000L ) -> (3: IntEx) ))
            BiPin.Removed[ S, Int ]( bip, tup6, tup6 )
         )
         obs.clear()

         bip.remove( 15000L -> 11 )   // should be ignored
         bip.remove( 15001L -> 3 )   // should be ignored
         obs.assertEmpty()
         assert( bip.valueAt( tup3._1 ) === Some( tup3._2 ))

         bip.remove( tup3 )
         obs.assertEquals(
//            BiPin.Collection( bip, IIdxSeq( Span( 10000L, 20000L ) -> (1: IntEx) ))
            BiPin.Removed[ S, Int ]( bip, tup3, tup3 )
         )
         obs.clear()
         assert( bip.valueAt( tup3._1 ) === Some( tup1._2 ))

         bip.remove( tup4 )
         obs.assertEquals(
//            BiPin.Collection( bip, IIdxSeq( Span.from( 10000L ) -> (1: IntEx) ))
            BiPin.Removed[ S, Int ]( bip, tup4, tup4 )
         )
         obs.clear()

         bip.remove( tup2 )
         bip.remove( tup1 )
//         obs.assertEmpty()
         obs.assertEquals(
            BiPin.Removed[ S, Int ]( bip, tup2, tup2 ),
            BiPin.Removed[ S, Int ]( bip, tup1, tup1 )
         )
         obs.clear()

         assert( bip.intersect( 0L ).isEmpty && bip.intersect( 20000L ).isEmpty )
      }
   }

   "BiPin" should "notify observers about all relevant element events" in { system =>
      val obs  = new Observation[ S ]
      val bipH = system.step { implicit tx =>
         val bip = BiPin.Modifiable[ S, Int ]
         bip.changed.reactTx( obs.register )
         val res = tx.newHandle( bip )( BiPin.Modifiable.serializer[ S, Int ])
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
         val th   = tx.newHandle( (time -> 3) : BiExpr[ S, Int ])
         val eh   = tx.newHandle( (30000L -> expr) : BiExpr[ S, Int ])
         (th, eh)
      }

//      confluent.showLog = false

      val tup1 =     0L -> 1
      val tup2 = 20000L -> 2

      system.step { implicit tx =>
         val bip  = bipH.get
         bip.add( tup1 )
         bip.add( tup2 )
         obs.assertEquals(
//            BiPin.Collection( bip, IIdxSeq( Span.from(     0L ) -> (1: IntEx) )),
//            BiPin.Collection( bip, IIdxSeq( Span.from( 20000L ) -> (2: IntEx) ))
            BiPin.Added[ S, Int ]( bip, tup1, tup1 ),
            BiPin.Added[ S, Int ]( bip, tup2, tup2 )
         )
         obs.clear()

         val time = timeH.get
         val expr = exprH.get
         bip.add( time )
         bip.add( expr )

         obs.assertEquals(
//            BiPin.Collection( bip, IIdxSeq( Span( 10000L, 20000L ) -> (3: IntEx) )),
//            BiPin.Collection( bip, IIdxSeq( Span.from( 30000L ) -> expr ))
            BiPin.Added[ S, Int ]( bip, 10000L -> 3, time ),
            BiPin.Added[ S, Int ]( bip, 30000L -> 4, expr )
         )
         obs.clear()
      }

      system.step { implicit tx =>
         val bip  = bipH.get
         val time = timeH.get
         val expr = exprH.get

         val Expr.Var( exprVar ) = expr.mag

         exprVar.set( 5 )
         obs.assertEquals(
//            BiPin.Element( bip, IIdxSeq( expr -> Change( 4, 5 )))
            BiPin.Element[ S, Int ]( bip, IIdxSeq( expr -> Change( 30000L -> 4, 30000L -> 5 )))
         )
         obs.clear()

         val Expr.Var( timeVar ) = time.time
         timeVar.set( 15000L )
         println( "DEBUG " + bip.debugList() )
         println( "DEBUG " + bip.valueAt( 10000L ))
         assert( bip.valueAt( 10000L ) === Some( tup1._2 ))
         assert( bip.valueAt( 15000L ) === Some( time.magValue ))
         obs.assertEquals(
//            BiPin.Collection( bip, IIdxSeq( Span(     0L, 15000L ) -> (1: IntEx),
//                                            Span( 15000L, 20000L ) -> (3: IntEx) ))
            BiPin.Element[ S, Int ]( bip, IIdxSeq( time -> Change( 10000L -> 3, 15000L -> 3 )))
         )
         obs.clear()

         timeVar.set( -5000L )
         assert( bip.valueAt(    -1L ) === Some( time.magValue ))
         assert( bip.valueAt( 15001L ) === Some( tup1._2 ))
         obs.assertEquals(
//            BiPin.Collection( bip, IIdxSeq( Span(     0L, 20000L ) -> (1: IntEx),
//                                            Span( -5000L,     0L ) -> (3: IntEx) ))
            BiPin.Element[ S, Int ]( bip, IIdxSeq( time -> Change( 15000L -> 3, -5000L -> 3 )))
         )
         obs.clear()

//         time.set( 25000L ) // the region -5000 ... 0 is 'swallowed'
//         obs.assertEquals(
////            BiPin.Collection( bip, IIdxSeq( Span( 25000L, 30000L ) -> (3: IntEx) ))
//            ???
//         )
//         obs.clear()
//
//         time.set( 35000L )
//         obs.assertEquals(
////            BiPin.Collection( bip, IIdxSeq( Span( 20000L, 30000L ) -> (2: IntEx),
////                                            Span.from( 35000L )    -> (3: IntEx) ))
//            ???
//         )
//         obs.clear()
//
//         expr.set( 6 )
//         obs.assertEquals(
////            BiPin.Element( bip, IIdxSeq( expr -> Change( 5, 6 )))
//            ???
//         )
//         obs.clear()
      }
   }
}
