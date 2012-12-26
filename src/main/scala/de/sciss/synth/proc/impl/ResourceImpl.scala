package de.sciss.synth.proc
package impl

import concurrent.stm.Ref

private[proc] trait ResourceImpl extends Resource {
   private val onlineRef   = Ref( initialValue = true )
   final def isOnline( implicit tx: Txn ) : Boolean = onlineRef.get( tx.peer )
   final protected def disposed()( implicit tx: Txn ) {
      onlineRef.set( false )( tx.peer )
   }
}