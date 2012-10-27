package de.sciss.synth
package proc

object RichServer {
   def apply( peer: Server ) : RichServer = ???
}
trait RichServer {
   def peer: Server
   def allocControlBus( numChannels: Int )( implicit tx: ProcTxn ) : Int
   def allocAudioBus(   numChannels: Int )( implicit tx: ProcTxn ) : Int
   def freeControlBus( index: Int, numChannels: Int )( implicit tx: ProcTxn ) : Unit
   def freeAudioBus(   index: Int, numChannels: Int )( implicit tx: ProcTxn ) : Unit
   def allocBuffer( numConsecutive: Int = 1 )( implicit tx: ProcTxn ) : Int
   def freeBuffer( index: Int, numConsecutive: Int = 1 )( implicit tx: ProcTxn ) : Unit

   def defaultGroup : RichGroup
}