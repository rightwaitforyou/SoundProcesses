//package de.sciss.synth.proc
//
//import de.sciss.lucre.event.{EventImpl, StandaloneLike}
//import de.sciss.lucre.stm.InMemory
//
//object EqualsTest {
//   class Test
//   extends StandaloneLike[ InMemory, Unit, Test ]
//   with EventImpl[ InMemory, Unit, Unit, Test ]
//
//   val t = new Test {
//      val i = hashCode()
//   }
//}
