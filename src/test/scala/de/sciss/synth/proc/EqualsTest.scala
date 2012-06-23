//package de.sciss.synth.proc
//
//import de.sciss.lucre.event.{EventImpl, StandaloneLike}
//import de.sciss.lucre.stm.InMemory
//
//object EqualsTest {
//   class Test extends EventImpl[ InMemory, Unit, Unit, Test ]
//   with StandaloneLike[ InMemory, Unit, Test ]
//
//   val t = new Test {
//      val i = hashCode()
//   }
//}
