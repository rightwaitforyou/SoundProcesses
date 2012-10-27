package de.sciss.synth
package proc
package impl

object RichServerImpl {
   def apply( peer: Server ) : RichServer = new Impl( peer )

   private case class Impl( peer: Server ) extends RichServer {
      private val controlBusAllocator  = proc.BlockAllocator( peer.config.controlBusChannels )
      private val audioBusAllocator    = proc.BlockAllocator( peer.config.audioBusChannels, peer.config.internalBusIndex )
      private val bufferAllocator      = proc.BlockAllocator( peer.config.audioBuffers )

      val defaultGroup : RichGroup = RichGroup.default( this )

      override def toString = peer.toString

      def allocControlBus( numChannels: Int )( implicit tx: ProcTxn ) : Int = {
         val res = controlBusAllocator.alloc( numChannels )( tx.peer )
         if( res < 0 ) throw new AllocatorExhaustedException( "Control buses exhausted for " + this )
         res
      }
      def allocAudioBus( numChannels: Int )( implicit tx: ProcTxn ) : Int = {
         val res = audioBusAllocator.alloc( numChannels )( tx.peer )
         if( res < 0 ) throw new AllocatorExhaustedException( "Audio buses exhausted for " + this )
         res
      }

      def freeControlBus( index: Int, numChannels: Int )( implicit tx: ProcTxn ) {
         controlBusAllocator.free( index, numChannels )( tx.peer )
      }

      def freeAudioBus( index: Int, numChannels: Int )( implicit tx: ProcTxn ) {
         audioBusAllocator.free( index, numChannels )( tx.peer )
      }

      def allocBuffer( numConsecutive: Int )( implicit tx: ProcTxn ) : Int = {
         val res = bufferAllocator.alloc( numConsecutive )( tx.peer )
         if( res < 0 ) throw new AllocatorExhaustedException( "Buffers exhausted for " + this )
         res
      }

      def freeBuffer( index: Int, numConsecutive: Int )( implicit tx: ProcTxn ) {
         bufferAllocator.free( index, numConsecutive )( tx.peer )
      }
   }
}