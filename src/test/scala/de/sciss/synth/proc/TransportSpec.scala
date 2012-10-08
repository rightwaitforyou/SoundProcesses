package de.sciss
package synth
package proc

import lucre.{bitemp, stm, expr}
import bitemp.Span
import expr.Expr
import collection.immutable.{IndexedSeq => IIdxSeq}

/**
 * To run only this suite:
 *
 * test-only de.sciss.synth.proc.TransportSpec
 */
class TransportSpec extends ConfluentEventSpec {
   import Transport.Advance
   import Grapheme.Segment
   import Transport.Proc.GraphemesChanged

   type I = stm.InMemory

//   import ConfluentReactive.inMemory

   import imp._

   "Transport" should "notify observers about all relevant events" in { system =>
      val obs  = new Observation[ S ]
      val (pgH, t) = system.step { implicit tx =>
         val pg   = ProcGroup_.Modifiable[ S ]
         val _t   = Transport.Offline[ S, I ]( pg, 10000.0 ) // ( tx, inMemory )
         _t.reactTx( obs.register )
         val res = tx.newHandle( pg )( ProcGroup_.Modifiable.serializer[ S ])
         obs.assertEmpty()
         (res, _t)
      }

      def curve( amp: Expr[ S, Double ], shape: Env.ConstShape = linShape )( implicit tx: S#Tx ) =
         Grapheme.Elem.Curve( amp -> shape )

      system.step { implicit tx =>
         val pg   = pgH.get
         val p1   = Proc[ S ]
         val g1   = Grapheme.Modifiable[ S ]
         p1.scans.add( "freq" ).source_=( Some( Scan.Link.Grapheme( g1 )))
         p1.scans.add( "egal" )
         g1.add( 7000L -> curve( 441.0 ))
         val p2   = Proc[ S ]
         val g2   = Grapheme.Modifiable[ S ]
         p2.scans.add( "amp" ).source_=( Some( Scan.Link.Grapheme( g2 )))
         val pt1  = pg.add( Span(    0L, 10000L ), p1 )
         val pt2  = pg.add( Span( 5000L, 20000L ), p2 )
         obs.assertEquals(
            Advance( t, time = 0L, isSeek = false, isPlaying = false, added = IIdxSeq( pt1 ))
         )
         obs.clear()

         t.play()
         obs.assertEquals(
            Transport.Play( t, 0L )
         )
         obs.clear()

         g2.add( 10000L, curve( 0.5 ))
         g2.add( 15000L, curve( 0.7 ))
         g2.add( 25000L, curve( 1.0 ))

         t.step()
         obs.assertEquals(
            Advance( t, time = 5000L, isSeek = false, isPlaying = true, added = IIdxSeq( pt2 ))
         )
         obs.clear()

         t.step()
         obs.assertEquals(
            Advance( t, time = 7000L, isSeek = false, isPlaying = true, changes =
               IIdxSeq( pt1 -> GraphemesChanged( Map( "freq" -> Segment.Const( Span.from( 7000L ), IIdxSeq( 441.0 )))))
            )
         )
         obs.clear()

         t.step()
         obs.assertEquals(
            Advance( t, time = 10000L, isSeek = false, isPlaying = true, removed = IIdxSeq( pt1 ), changes =
               IIdxSeq( pt2 -> GraphemesChanged( Map( "amp" -> Segment.Curve( Span( 10000L, 15000L ), IIdxSeq( (0.5, 0.7, linShape) )))))
            )
         )
         obs.clear()

         t.step()
         obs.assertEquals(
            Advance( t, time = 15000L, isSeek = false, isPlaying = true, changes =
               IIdxSeq( pt2 -> GraphemesChanged( Map( "amp" -> Segment.Curve( Span( 15000L, 25000L ), IIdxSeq( (0.7, 1.0, linShape) )))))
            )
         )
         obs.clear()

         t.step()
         obs.assertEquals(
            Advance( t, time = 20000L, isSeek = false, isPlaying = true, removed = IIdxSeq( pt2 ))
         )
         obs.clear()

         t.step()
         obs.assertEmpty()

         t.stop()
         obs.assertEquals(
            Transport.Stop( t, 25000L )   // will advance to grapheme stuff even beyond proc spans
         )
         obs.clear()
      }
   }
}