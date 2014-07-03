package de.sciss.lucre.synth.expr

import concurrent.stm.TxnLocal
import collection.immutable.{IndexedSeq => Vec}
import org.scalatest.{Outcome, Matchers, fixture}
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.{confluent, stm}
import de.sciss.lucre.confluent.reactive.ConfluentReactive
import de.sciss.lucre

trait ConfluentEventSpec extends fixture.FlatSpec with Matchers {
  type S = ConfluentReactive
  type D = S#D
  type FixtureParam = confluent.Cursor[S, D]

  implicit final protected val IntType  = lucre.expr.Int
  implicit final protected val LongType = lucre.expr.Long
  final protected val imp = ExprImplicits[S]

  final def withFixture(test: OneArgTest): Outcome = {
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

    def register(tx: S#Tx)(upd: Any): Unit =
      seqRef.transform(_ :+ upd)(tx.peer)

    def assertEquals(expected: Any*)(implicit tx: S#Tx): Unit = {
      val ob = seqRef.get(tx.peer)
      assert(ob === expected.toIndexedSeq, "Expected\n   " + expected.mkString("[", ", ", "]")
        + "\n...but observed\n   " + ob.mkString("[", ", ", "]"))
    }

    def clear()(implicit tx: S#Tx): Unit =
      seqRef.set(Vector.empty)(tx.peer)

    def assertEmpty()(implicit tx: S#Tx): Unit = assertEquals()

    def print()(implicit tx: S#Tx): Unit =
      println(seqRef.get(tx.peer).mkString("[", ", ", "]"))
  }
}
