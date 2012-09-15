package de.sciss.synth
package proc

trait TxnPlayer {
   def play( implicit tx: ProcTxn ) : Unit
   def stop( implicit tx: ProcTxn ) : Unit
   def isPlaying( implicit tx: ProcTxn ) : Boolean
}