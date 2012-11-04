/*
 *  RichServerImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2012 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package proc
package impl

object RichServerImpl {
   def apply( peer: Server ) : RichServer = new Impl( peer )

   private case class Impl( peer: Server ) extends RichServer {
      private val controlBusAllocator  = proc.BlockAllocator( "control", peer.config.controlBusChannels )
      private val audioBusAllocator    = proc.BlockAllocator( "audio", peer.config.audioBusChannels, peer.config.internalBusIndex )
      private val bufferAllocator      = proc.BlockAllocator( "buffer", peer.config.audioBuffers )

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