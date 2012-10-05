package de.sciss

import lucre.confluent.reactive.ConfluentReactive
import lucre.stm
import org.scalatest.fixture
import org.scalatest.matchers.ShouldMatchers
import synth.expr.{ExprImplicits, Ints}
import concurrent.stm.TxnLocal
import collection.immutable.{IndexedSeq => IIdxSeq}

trait ConfluentEventSpec extends fixture.FlatSpec with ShouldMatchers {
   final type FixtureParam = ConfluentReactive
   final type S = FixtureParam

   implicit final val IntType = Ints

   final val imp = new ExprImplicits[ S ]

   final def withFixture( test: OneArgTest ) {
      val system = ConfluentReactive.tmp()
      try {
         system.root( _ => () )
         test( system )
      }
      finally {
         system.close()
      }
   }

   final class Observation[ S <: stm.Sys[ S ]] {
      private val seqRef = TxnLocal( init = IIdxSeq.empty[ Any ])

      def register( tx: S#Tx )( upd: Any ) {
         seqRef.transform( _ :+ upd )( tx.peer )
      }

      def assertEquals( expected: Any* )( implicit tx: S#Tx ) {
         val ob = seqRef.get( tx.peer )
         assert( ob === expected.toIndexedSeq, "Expected\n   " + expected.mkString( "[", ", ", "]" )
            + "\n...but observed\n   " + ob.mkString( "[", ", ", "]" ))
      }

      def clear()( implicit tx: S#Tx ) {
         seqRef.set( IIdxSeq.empty )( tx.peer )
      }

      def assertEmpty()( implicit tx: S#Tx ) { assertEquals() }
   }
}
