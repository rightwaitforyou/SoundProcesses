package de.sciss.synth.proc

import de.sciss.lucre.{event => evt}
import de.sciss.synth.proc

object Sys {
   trait Txn[ S <: Sys[ S ]] extends evt.Txn[ S ] with proc.Txn
}
trait Sys[ S <: Sys[ S ]] extends evt.Sys[ S ] {
   type Tx <: Sys.Txn[ S ]

//   private[proc] def resources( server: Server ): ResourceManagement
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