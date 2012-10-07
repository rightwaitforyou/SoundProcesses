package de.sciss
package lucre
package bitemp

import expr.Expr

/**
 * To test only this suite:
 *
 * test-only de.sciss.lucre.bitemp.BiPinSerializationSpec
 */
class BiPinSerializationSpec extends ConfluentEventSpec {
   confluent.showLog = true

   "BiPin" should "serialize and deserialize" in { system =>
      val bipH = system.step { implicit tx =>
         val bip = BiPin.Modifiable[ S, Int ]
         tx.newHandle( bip )( BiPin.Modifiable.serializer[ S, Int ])
      }
//      val acc = system.step { implicit tx => tx.inputAccess }
//      println( "--step; bipH = " + bipH + "; cursor pos = " + acc )

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
         val res        = bip.intersect( key.value ).toIndexedSeq
         val expected   = IndexedSeq[ BiExpr[ S, Int ]]( key -> value )
         assert( res === expected ) // , "Unexpected retrieval " + res )
      }
   }
}
