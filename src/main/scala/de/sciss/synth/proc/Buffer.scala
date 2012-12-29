/*
 *  Buffer.scala
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

import de.sciss.synth.{Buffer => SBuffer}
import de.sciss.synth.io.{AudioFileType, SampleFormat}
import impl.{BufferImpl => Impl}

object Buffer {
   def diskIn( server: Server, path: String, startFrame: Long = 0L, numFrames: Int = SoundProcesses.cueBufferSize,
               numChannels: Int = 1 )( implicit tx: Txn ) : Buffer = {
      SoundProcesses.validateCueBufferSize( numFrames )
      val peer = allocPeer( server )
      val res  = new Impl( server, peer )( closeOnDisposal = true )
      res.allocRead( path, startFrame = startFrame, numFrames = numFrames )
      res
   }

   def diskOut( server: Server, path: String, fileType: AudioFileType = AudioFileType.AIFF,
                sampleFormat: SampleFormat = SampleFormat.Float,
                numFrames: Int = SoundProcesses.cueBufferSize, numChannels: Int = 1 ) : Buffer = {
      SoundProcesses.validateCueBufferSize( numFrames )
      ???
   }

   def fft( server: Server, size: Int ) : Modifiable = {
      require( size >= 2 && SoundProcesses.isPowerOfTwo( size ), "Must be a power of two and >= 2 : " + size )
      ???
   }

   def apply( server: Server, numFrames: Int = SoundProcesses.cueBufferSize, numChannels: Int = 1 ) : Modifiable = {
      ???
   }

   private def allocPeer( server: Server )( implicit tx: Txn ) : SBuffer = {
      val id   = server.allocBuffer()
      SBuffer( server.peer, id )
   }

   trait Modifiable extends Buffer {
      def zero()( implicit tx: Txn ) : Unit
      def read( path: String, fileStartFrame: Long = 0L, numFrames: Int = -1, bufStartFrame: Int = 0 )
              ( implicit tx: Txn ) : Unit
   }
}
trait Buffer extends Resource {
   def peer: SBuffer
   def id: Int
}