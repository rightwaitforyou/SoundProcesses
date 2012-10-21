package de.sciss

import lucre.confluent.reactive.ConfluentReactive
import lucre.stm
import org.scalatest.fixture
import org.scalatest.matchers.ShouldMatchers
import synth.expr.{Longs, ExprImplicits, Ints}
import concurrent.stm.TxnLocal
import collection.immutable.{IndexedSeq => IIdxSeq}

trait ConfluentEventSpec extends fixture.FlatSpec with ShouldMatchers {
   final type S = ConfluentReactive
   final type FixtureParam = lucre.confluent.Cursor[ S ]

   implicit final protected val IntType   = Ints
   implicit final protected val LongType  = Longs
   final protected val imp = ExprImplicits[ S ]

   final def withFixture( test: OneArgTest ) {
      val system = ConfluentReactive.tmp()
      try {
         val (_, cursor) = system.cursorRoot( _ => () )( tx => _ => tx.newCursor() )
         test( cursor )
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

      def print()( implicit tx: S#Tx ) {
         println( seqRef.get( tx.peer ).mkString( "[", ", ", "]" ))
      }
   }
}
