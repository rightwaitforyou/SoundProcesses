package de.sciss.synth.proc

import de.sciss.lucre.{stm, confluent, event => evt}
import confluent.reactive.ConfluentReactiveLike
import de.sciss.synth.{osc => sosc, proc}
import de.sciss.osc

object Sys {
   trait Txn[ S <: Sys[ S ]] extends evt.Txn[ S ] with proc.Txn
}
trait Sys[ S <: Sys[ S ]] extends evt.Sys[ S ] {
   type Tx <: Sys.Txn[ S ]

   private[proc] def resources( server: Server ): ResourceManagement
}

//object System {
//   type S = System
//
//   trait Txn extends Sys.Txn[ S ] {
//      private[proc] def durable  : stm.Durable#Tx
//      private[proc] def inMemory : stm.InMemory#Tx
//   }
//}
//trait System extends Sys[ System ] {
//   final protected type S  = System
//   final type D            = stm.Durable
//   final type I            = stm.InMemory
//   final type Tx           = System.Txn
// }