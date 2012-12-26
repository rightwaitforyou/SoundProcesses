package de.sciss.synth.proc
package impl

import de.sciss.synth.{SynthDef => SSynthDef}

private[proc] final case class SynthDefImpl( server: Server, peer: SSynthDef ) extends ResourceImpl with SynthDef {
//   val isOnline = State( this, "isOnline", init = false )

   override def toString = "SynthDef(" + peer.name + ")"

   def name : String = peer.name

   /**
    *    Actually checks if the def is already online.
    *    Only if that is not the case, the receive message
    *    will be queued.
    */
   def recv()( implicit tx: Txn ) {
      tx.addMessage( this, peer.recvMsg, audible = false )
   }



//   def play( target: Node, args: Seq[ ControlSetMap ] = Nil,
//             addAction: AddAction = addToHead, buffers: Seq[ Buffer ] = Nil )( implicit tx: Txn ) : Synth = {
////      recv()  // make sure it is online
//      val rs = Synth( this )
//      rs.play( target, args, addAction, buffers )
//      rs
//   }

   def dispose()( implicit tx: Txn ) {
      require( isOnline )
      tx.addMessage( this, peer.freeMsg, audible = false )
      disposed()
   }
}