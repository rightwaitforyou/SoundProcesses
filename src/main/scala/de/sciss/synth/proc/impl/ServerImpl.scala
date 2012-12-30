/*
 *  ServerImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2013 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.synth.proc
package impl

import de.sciss.synth.{AllocatorExhaustedException, Server => SServer}

object ServerImpl {
   def apply( peer: SServer ) : Server = new Impl( peer )

   private case class Impl( peer: SServer ) extends Server {
      private val controlBusAllocator  = BlockAllocator( "control", peer.config.controlBusChannels )
      private val audioBusAllocator    = BlockAllocator( "audio",   peer.config.audioBusChannels, peer.config.internalBusIndex )
      private val bufferAllocator      = BlockAllocator( "buffer",  peer.config.audioBuffers )

      val defaultGroup : Group = Group.wrap( this, peer.defaultGroup ) // .default( this )

      override def toString = peer.toString()

      def allocControlBus( numChannels: Int )( implicit tx: Txn ) : Int = {
         val res = controlBusAllocator.alloc( numChannels )( tx.peer )
         if( res < 0 ) throw new AllocatorExhaustedException( "Control buses exhausted for " + this )
         res
      }
      def allocAudioBus( numChannels: Int )( implicit tx: Txn ) : Int = {
         val res = audioBusAllocator.alloc( numChannels )( tx.peer )
         if( res < 0 ) throw new AllocatorExhaustedException( "Audio buses exhausted for " + this )
         res
      }

      def freeControlBus( index: Int, numChannels: Int )( implicit tx: Txn ) {
         controlBusAllocator.free( index, numChannels )( tx.peer )
      }

      def freeAudioBus( index: Int, numChannels: Int )( implicit tx: Txn ) {
         audioBusAllocator.free( index, numChannels )( tx.peer )
      }

      def allocBuffer( numConsecutive: Int )( implicit tx: Txn ) : Int = {
         val res = bufferAllocator.alloc( numConsecutive )( tx.peer )
         if( res < 0 ) throw new AllocatorExhaustedException( "Buffers exhausted for " + this )
         res
      }

      def freeBuffer( index: Int, numConsecutive: Int )( implicit tx: Txn ) {
         bufferAllocator.free( index, numConsecutive )( tx.peer )
      }
   }
}