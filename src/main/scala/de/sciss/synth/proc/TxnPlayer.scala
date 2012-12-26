package de.sciss.synth.proc

trait TxnPlayer {
   def play( implicit tx: Txn ) : Unit
   def stop( implicit tx: Txn ) : Unit
   def isPlaying( implicit tx: Txn ) : Boolean
}