package de.sciss.synth.proc

import org.scalatest.{Outcome, Matchers, fixture}
import de.sciss.lucre
import de.sciss.lucre.synth.InMemory
import de.sciss.lucre.{expr, stm}

trait InMemoryEventSpec extends fixture.FlatSpec with Matchers {
  type S = InMemory
  type FixtureParam = stm.Cursor[S]

  implicit final protected val IntType  = lucre.expr.Int
  implicit final protected val LongType = lucre.expr.Long
  final protected val imp = ExprImplicits[S]

  expr.init()

  final def withFixture(test: OneArgTest): Outcome = {
    val system = InMemory()
    try {
      test(system)
    }
    finally {
      system.close()
    }
  }
}
