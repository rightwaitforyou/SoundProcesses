package de.sciss.synth.proc

import de.sciss.osc
import de.sciss.synth.{osc => sosc}
import concurrent.stm.InTxn

trait Txn {
//   protected type S <: evt.

   def peer: InTxn
//   def beforeCommit( fun: Txn => Unit ) : Unit
   private[proc] def addMessage( resource: Resource, msg: osc.Message with sosc.Send,
                                 audible: Boolean, dependencies: Seq[ Resource ] = Nil, noErrors: Boolean = false ) : Unit

//   def addMessage( msg: osc.Message with sosc.Send, change: Option[ (FilterMode, State, Boolean) ], audible: Boolean,
//                   dependencies: Map[ State, Boolean ], noError: Boolean = false ) {
}