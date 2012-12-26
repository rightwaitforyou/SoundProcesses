package de.sciss.synth.proc

import de.sciss.lucre.stm.Disposable

trait Resource extends Disposable[ Txn ] {
   def isOnline( implicit tx: Txn ) : Boolean
   def server: Server
}