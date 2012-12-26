package de.sciss.synth.proc
package impl

import de.sciss.synth.{Group => SGroup, addToHead, AddAction}

private[proc] class GroupImpl( server: Server, peer: SGroup ) extends Group with NodeImpl {
   override def toString = "Group(" + peer.toString + ")"

   def play( target: Node, addAction: AddAction )( implicit tx: Txn ) {
      require( target.server == server && target.isDisposed )

      // THERE IS CURRENTLY A PROBLEM EXHIBITED BY TEST3: BASICALLY --
      // since newMsg is not audible, it might be placed in the first bundle, but then
      // since moveAfterMsg is audible, the target of this group's newMsg might be
      // moved, ending up in moveAfterMsg following the g_new message, leaving this
      // group in the wrong place of the graph.
      //
      // We thus try out a workaround by declaring a group's newMsg also audible...
//      tx.add( group.newMsg( target.node, addAction ), Some( (RequiresChange, isOnline, true) ), false,
//              Map( target.isOnline -> true ))
      tx.addMessage( this, peer.newMsg( target.peer, addAction ), audible = true,
                     dependencies = target :: Nil )
   }
}