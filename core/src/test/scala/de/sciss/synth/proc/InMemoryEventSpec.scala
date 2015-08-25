package de.sciss.synth.proc

import de.sciss.lucre.stm
import de.sciss.lucre.synth.InMemory
import org.scalatest.{Matchers, Outcome, fixture}

trait InMemoryEventSpec extends fixture.FlatSpec with Matchers {
  type S = InMemory
  type FixtureParam = stm.Cursor[S]

  SoundProcesses.init()

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
