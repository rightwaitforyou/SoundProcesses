package de.sciss.synth.proc
package impl

import concurrent.stm.{TSet, Ref}

private[proc] trait ResourceImpl extends Resource {
   import Resource.TimeStamp

   private val timeStampRef   = Ref( 0 )
   private val dependentsRef  = TSet.empty[ Resource ]

   final def isOnline( implicit tx: Txn ) : Boolean = timeStamp >= 0

   final protected def disposed()( implicit tx: Txn ) {
      require( dependentsRef.isEmpty( tx.peer ), "Disposing a resource which still has dependents : " + this )
      timeStamp_=( -1 )
   }

   final def timeStamp( implicit tx: Txn ) : TimeStamp = timeStampRef.get( tx.peer )
   final def timeStamp_=( value: TimeStamp )( implicit tx: Txn ) {
      timeStampRef.set( value )( tx.peer )
   }

   final def addDependent( dependent: Resource )( implicit tx: Txn ) {
      dependentsRef.add( dependent )( tx.peer )
   }

   final def removeDependent( dependent: Resource )( implicit tx: Txn ) {
      dependentsRef.remove( dependent )( tx.peer )
   }
}