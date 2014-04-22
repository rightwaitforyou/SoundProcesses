package de.sciss

import lucre.stm
import org.scalatest.{Outcome, Matchers, fixture}
import concurrent.stm.TxnLocal
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.synth.proc.{ExprImplicits, Confluent}
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.expr

trait ConfluentEventSpec extends fixture.FlatSpec with Matchers {
  type S = Confluent
  type D = S#D
  type FixtureParam = lucre.confluent.Cursor[S, D]

  implicit final protected val IntType  = lucre.expr.Int
  implicit final protected val LongType = lucre.expr.Long
  final protected val imp = ExprImplicits[S]

  expr.initTypes()

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

    def register(tx: S#Tx)(upd: Any): Unit = {
      seqRef.transform(_ :+ upd)(tx.peer)
    }

    def assertEquals(expected: Any*)(implicit tx: S#Tx): Unit = {
      val ob = seqRef.get(tx.peer)
      assert(ob === expected.toIndexedSeq, "Expected\n   " + expected.mkString("[", ", ", "]")
        + "\n...but observed\n   " + ob.mkString("[", ", ", "]"))
    }

    def clear()(implicit tx: S#Tx): Unit = {
      seqRef.set(Vector.empty)(tx.peer)
    }

    def assertEmpty()(implicit tx: S#Tx): Unit = {
      assertEquals()
    }

    def print()(implicit tx: S#Tx): Unit = {
      println(seqRef.get(tx.peer).mkString("[", ", ", "]"))
    }
  }
}
