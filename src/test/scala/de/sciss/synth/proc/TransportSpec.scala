package de.sciss
package synth
package proc

import lucre.{event => evt, bitemp, stm}
import bitemp.Span
import collection.immutable.{IndexedSeq => IIdxSeq}

/**
 * To run only this suite:
 *
 * test-only de.sciss.synth.proc.TransportSpec
 */
class TransportSpec extends ConfluentEventSpec {
   import Transport.Advance

   type I = stm.InMemory

//   import ConfluentReactive.inMemory

   import imp._

   "Transport" should "notify observers about all relevant events" in { system =>
      val obs  = new Observation[ S ]
      val (pgH, t) = system.step { implicit tx =>
         val pg   = ProcGroup_.Modifiable[ S ]
         val _t   = Transport.Offline[ S, I ]( pg ) // ( tx, inMemory )
         _t.reactTx( obs.register )
         val res = tx.newHandle( pg )( ProcGroup_.Modifiable.serializer[ S ])
         obs.assertEmpty()
         (res, _t)
      }

      system.step { implicit tx =>
         val pg   = pgH.get
         val p1   = Proc[ S ]
         val p2   = Proc[ S ]
         val pt1  = pg.add( Span(    0L, 10000L ), p1 )
         val pt2  = pg.add( Span( 5000L, 20000L ), p2 )
         obs.assertEquals(
            Advance( t, time = 0L, isSeek = false, isPlaying = false, added = IIdxSeq( pt1 ))
         )

      }
   }
}