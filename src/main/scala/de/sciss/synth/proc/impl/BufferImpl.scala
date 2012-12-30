/*
 *  BufferImpl.scala
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

import de.sciss.synth.{Buffer => SBuffer }
import de.sciss.synth.io.{SampleFormat, AudioFileType}

private[proc] final case class BufferImpl( server: Server, peer: SBuffer )( closeOnDisposal: Boolean )
extends ResourceImpl with Buffer.Modifiable {
//   val isAlive:    State = State(                this, "isAlive", init = true )
//   val isOnline:   State = State.and( isAlive )( this, "isOnline", init = false )
//   val hasContent: State = State(                this, "hasContent", init = false )

   def id: Int = peer.id

   def alloc( numFrames: Int, numChannels: Int = 1 )( implicit tx: Txn ) {
      require( isOnline )
      require( numFrames >= 0 && numChannels >= 0 )
      tx.addMessage( this, peer.allocMsg( numFrames = numFrames, numChannels = numChannels ), audible = false )
   }

   def allocRead( path: String, startFrame: Long, numFrames: Int )( implicit tx: Txn ) {
      require( isOnline )
      require( startFrame <= 0x7FFFFFFFL, "Cannot encode start frame >32 bit (" + startFrame + ")" )
      require( numFrames >= 0 )
      val frameI = startFrame.toInt
      tx.addMessage( this, peer.allocReadMsg( path, startFrame = frameI, numFrames = numFrames ), audible = false )
   }

   def record( path: String, fileType: AudioFileType, sampleFormat: SampleFormat )( implicit tx: Txn ) {
      write( path, fileType, sampleFormat, numFrames = 0, leaveOpen = true )
   }

   def write( path: String, fileType: AudioFileType, sampleFormat: SampleFormat, numFrames: Int = -1,
              startFrame: Int = 0, leaveOpen: Boolean = false )( implicit tx: Txn ) {
      require( isOnline )
      require( leaveOpen == closeOnDisposal )
      require( startFrame >= 0 )
      tx.addMessage( this, peer.writeMsg( path, fileType, sampleFormat, numFrames = numFrames, startFrame = startFrame,
                     leaveOpen = leaveOpen ), audible = false )
   }

   def read( path: String, fileStartFrame: Long, numFrames: Int, bufStartFrame: Int )( implicit tx: Txn ) {
      require( isOnline )
      require( fileStartFrame <= 0x7FFFFFFFL, "Cannot encode start frame >32 bit (" + fileStartFrame + ")" )
      require( numFrames >= 0 && bufStartFrame >= 0 )
      val frameI = fileStartFrame.toInt
      tx.addMessage( this, peer.readMsg( path, fileStartFrame = frameI, numFrames = numFrames, leaveOpen = false ),
                     audible = false )
   }

   def zero()( implicit tx: Txn ) {
      require( isOnline )
      tx.addMessage( this, peer.zeroMsg, audible = false )
   }

   def dispose()( implicit tx: Txn ) {
      require( isOnline )
      if( closeOnDisposal ) {
         tx.addMessage( this, peer.closeMsg, audible = false )
      }
      tx.addMessage( this, peer.freeMsg( release = false ), audible = false )
      server.freeBuffer( peer.id )
      disposed()
   }
}