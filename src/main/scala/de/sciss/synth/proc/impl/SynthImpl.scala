package de.sciss.synth.proc
package impl

import de.sciss.synth.{ControlSetMap, AddAction, Synth => SSynth}

private[proc] final case class SynthImpl( peer: SSynth, definition: SynthDef ) extends NodeImpl with Synth {
   override def toString = "Synth(id=" + peer.id + ", def=" + definition.name + ")"

   def server: Server = definition.server

   def play( target: Node, args: Seq[ ControlSetMap ], addAction: AddAction, dependencies: List[ Resource ])
           ( implicit tx: Txn ) {

      val s = server
      require( target.server == s && target.isOnline )
      if( dependencies.nonEmpty ) {
         dependencies.foreach( r => require( r.server == s && r.isOnline ))
      }
      tx.addMessage( this, peer.newMsg( definition.name, target.peer, args, addAction ),
                     audible = true,
                     dependencies = target :: definition :: dependencies )

//      peer.register()   // ok to call multiple times
   }
}