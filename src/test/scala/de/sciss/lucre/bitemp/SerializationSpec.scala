package de.sciss
package lucre
package bitemp

import org.scalatest.fixture
import org.scalatest.matchers.ShouldMatchers
import confluent.reactive.ConfluentReactive
import de.sciss.synth.expr.{ExprImplicits, Ints}
import expr.Expr

/**
 * To test only this suite:
 *
 * test-only de.sciss.lucre.bitemp.SerializationSpec
 */
class SerializationSpec extends ConfluentEventSpec {
   confluent.showLog = true

   "BiPin" should "serialize and deserialize" in { system =>
      val bipH = system.step { implicit tx =>
         val bip = BiPin.Expr.Modifiable[ S, Int ]
         tx.newHandle( bip )( BiPin.Expr.Modifiable.serializer[ S, Int ])
      }
      val acc = system.step { implicit tx => tx.inputAccess }
      println( "--step; bipH = " + bipH + "; cursor pos = " + acc )

//      implicit val bipSer = BiPin.Expr.Modifiable.serializer[ S, Int ]
      import imp._

      val key: Expr[ S, Long ]   = 1234L
      val value: Expr[ S, Int ]  = 5678

      system.step { implicit tx =>
         val bip = bipH.get // tx.refresh( acc, bip0 )
         bip.add( key, value )
      }

      system.step { implicit tx =>
         val bip = bipH.get // tx.refresh( acc, bip0 )
         assert( bip.intersect( key.value - 1 ).isEmpty, "BiPin should be empty before the first pin" )
         val res = bip.intersect( key.value ).toIndexedSeq
         assert( res == IndexedSeq( key -> value ), "Unexpected retrieval " + res )
      }
   }
}
