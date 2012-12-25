package de.sciss.synth.proc

import de.sciss.osc
import de.sciss.synth.{osc => sosc}
import concurrent.stm.InTxn

trait Txn {
   def peer: InTxn
   private[proc] def addMessage( server: Server, msg: osc.Message with sosc.Send,
                                 change: State.Change, audible: Boolean,
                                 dependencies: Map[ State, Boolean ] = Map.empty, noErrors: Boolean = false ) : Unit

//   def addMessage( msg: osc.Message with sosc.Send, change: Option[ (FilterMode, State, Boolean) ], audible: Boolean,
//                   dependencies: Map[ State, Boolean ], noError: Boolean = false ) {
}