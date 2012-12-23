package de.sciss.synth.proc

import de.sciss.lucre.{stm, confluent}
import confluent.reactive.ConfluentReactiveLike
import de.sciss.synth.{osc => sosc}
import de.sciss.osc

object SysLike {
   trait Txn[ S <: SysLike[ S ]] extends ConfluentReactiveLike.Txn[ S ] {
      private[proc] def addMessage( msg: osc.Message with sosc.Send,
                                    change: Option[ (Sys.FilterMode, RichState, Boolean) ], audible: Boolean,
                                    dependencies: Map[ RichState, Boolean ] = Map.empty, noErrors: Boolean = false ) : Unit
   }
}
trait SysLike[ S <: SysLike[ S ]] extends ConfluentReactiveLike[ S ] {
   type Tx <: SysLike.Txn[ S ]
}

object Sys {
   type S = Sys

   sealed abstract class FilterMode
   case object Always extends FilterMode
   case object IfChanges extends FilterMode
   case object RequiresChange extends FilterMode

   trait Txn extends SysLike.Txn[ S ] {
      private[proc] def durable : stm.Durable#Tx
      private[proc] def inMemory : stm.InMemory#Tx
   }
}
trait Sys extends SysLike[ Sys ] {
   final protected type S  = Sys
   final type D            = stm.Durable
   final type I            = stm.InMemory
   final type Tx           = Sys.Txn
}