package de.sciss.synth.proc

import de.sciss.lucre.stm.Disposable

object Resource {
   type TimeStamp = Int
}
trait Resource extends Disposable[ Txn ] {
   import Resource.TimeStamp

   def isOnline( implicit tx: Txn ) : Boolean
   def server: Server

   private[proc] def timeStamp( implicit tx: Txn ) : TimeStamp
   private[proc] def timeStamp_=( value: TimeStamp )( implicit tx: Txn ) : Unit

   private[proc] def addDependent(    dependent: Resource )( implicit tx: Txn ) : Unit
   private[proc] def removeDependent( dependent: Resource )( implicit tx: Txn ) : Unit
}