package de.sciss

import lucre.stm
import org.scalatest.{Outcome, Matchers, fixture}
import concurrent.stm.TxnLocal
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.synth.proc.{ExprImplicits, Confluent}
import de.sciss.lucre.synth.expr.{Longs, Ints}
import de.sciss.lucre.stm.store.BerkeleyDB

trait ConfluentEventSpec extends fixture.FlatSpec with Matchers {
  type S = Confluent
  type D = S#D
  type FixtureParam = lucre.confluent.Cursor[S, D]

  implicit final protected val IntType  = Ints
  implicit final protected val LongType = Longs
  final protected val imp = ExprImplicits[S]

  final def withFixture(test: OneArgTest): Outcome = {
    val system = Confluent(BerkeleyDB.tmp())
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
