package de.sciss.synth.proc

import de.sciss.osc
import de.sciss.synth.{osc => sosc}
import concurrent.stm.InTxn
import collection.immutable.{IndexedSeq => IIdxSeq}

object Txn {
   /**
    * A data type encapsulating all the outgoing OSC bundles for this transaction.
    *
    * @param firstCnt   the counter value of the first bundle in the payload
    * @param payload    the succession of bundles, represented as a sequence of a sequence of messages
    */
   final case class Bundles( firstCnt: Int, payload: IIdxSeq[ IIdxSeq[ osc.Message with sosc.Send ]])
}
trait Txn {
//   protected type S <: evt.

   def peer: InTxn
//   def beforeCommit( fun: Txn => Unit ) : Unit
   private[proc] def addMessage( resource: Resource, message: osc.Message with sosc.Send,
                                 audible: Boolean, dependencies: Seq[ Resource ] = Nil, noErrors: Boolean = false ) : Unit

//   def addMessage( msg: osc.Message with sosc.Send, change: Option[ (FilterMode, State, Boolean) ], audible: Boolean,
//                   dependencies: Map[ State, Boolean ], noError: Boolean = false ) {
}