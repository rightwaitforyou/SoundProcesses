package de.sciss.synth.proc

import de.sciss.lucre.{event => evt, stm}
import impl.{InMemoryImpl => Impl}

object InMemory {
   def apply() : InMemory = Impl()
}
trait InMemory extends stm.InMemoryLike[ InMemory ] with Sys[ InMemory ] {
   final type Tx = Sys.Txn[ InMemory ]
}