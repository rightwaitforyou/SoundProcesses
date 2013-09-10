package de.sciss.lucre.synth.expr

import concurrent.stm.TxnLocal
import collection.immutable.{IndexedSeq => Vec}
import org.scalatest.fixture
import org.scalatest.matchers.ShouldMatchers
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.{confluent, stm}
import de.sciss.lucre.confluent.reactive.ConfluentReactive

trait ConfluentEventSpec extends fixture.FlatSpec with ShouldMatchers {
  type S = ConfluentReactive
  type D = S#D
  type FixtureParam = confluent.Cursor[S, D]

  implicit final protected val IntType  = Ints
  implicit final protected val LongType = Longs
  final protected val imp = ExprImplicits[S]

  final def withFixture(test: OneArgTest) {
    val system = ConfluentReactive(BerkeleyDB.tmp())
    try {
      val (_, cursor) = system.cursorRoot(_ => ())(implicit tx => _ => system.newCursor())
      test(cursor)
    }
    finally {
      system.close()
    }
  }

  final class Observation[S <: stm.Sys[S]] {
    private val seqRef = TxnLocal(init = Vec.empty[Any])

    def register(tx: S#Tx)(upd: Any) {
      seqRef.transform(_ :+ upd)(tx.peer)
    }

    def assertEquals(expected: Any*)(implicit tx: S#Tx) {
      val ob = seqRef.get(tx.peer)
      assert(ob === expected.toIndexedSeq, "Expected\n   " + expected.mkString("[", ", ", "]")
        + "\n...but observed\n   " + ob.mkString("[", ", ", "]"))
    }

    def clear()(implicit tx: S#Tx) {
      seqRef.set(Vector.empty)(tx.peer)
    }

    def assertEmpty()(implicit tx: S#Tx) {
      assertEquals()
    }

    def print()(implicit tx: S#Tx) {
      println(seqRef.get(tx.peer).mkString("[", ", ", "]"))
    }
  }
}
