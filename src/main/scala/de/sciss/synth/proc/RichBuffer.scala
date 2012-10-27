/*
 *  RichBuffer.scala
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

import io.{SampleFormat, AudioFileType}
import proc.ProcTxn.{Always, RequiresChange}

object RichBuffer {
   def apply( server: RichServer )( implicit tx: ProcTxn ) : RichBuffer = {
      val id   = server.allocBuffer()
      val b    = Buffer( server.peer, id )
      new RichBuffer( server, b )
   }
}
final case class RichBuffer private( server: RichServer, peer: Buffer ) {
   val isAlive:    RichState = RichState(                this, "isAlive", init = true )
   val isOnline:   RichState = RichState.and( isAlive )( this, "isOnline", init = false )
   val hasContent: RichState = RichState(                this, "hasContent", init = false )

   def id: Int = peer.id

   def alloc( numFrames: Int, numChannels: Int = 1 )( implicit tx: ProcTxn ) {
      tx.add( msg          = peer.allocMsg( numFrames, numChannels ),
              change       = Some( (RequiresChange, isOnline, true) ),
              dependencies = Map( isAlive -> true ),
              audible      = false 
      )
   }

   def cue( path: String, startFrame: Long = 0L )( implicit tx: ProcTxn ) {
      require( startFrame < 0x7FFFFFFFL, "Cannot encode start frame >32 bit" )
      tx.add( msg          = peer.cueMsg( path, startFrame.toInt ),
              change       = Some( (Always, hasContent, true) ),
              dependencies = Map( isOnline -> true ),   
              audible      = false
      )
   }

   def record( path: String, fileType: AudioFileType, sampleFormat: SampleFormat )( implicit tx: ProcTxn ) {
      tx.add( msg          = peer.writeMsg( path, fileType, sampleFormat, 0, 0, leaveOpen = true ),
              change       = Some( (Always, hasContent, true) ), 
              dependencies = Map( isOnline -> true ), // hasContent is a bit misleading...
              audible      = false
      )
   }

   def zero()( implicit tx: ProcTxn ) {
      tx.add( msg          = peer.zeroMsg,
              change       = Some( (Always, hasContent, true) ),
              dependencies = Map( isOnline -> true ),
              audible      = false
      )
   }

   def closeAndFree()( implicit tx: ProcTxn ) {
      tx.add( msg          = peer.closeMsg( peer.freeMsg( release = false )),   // release = false is crucial!
              change       = Some( (RequiresChange, isAlive, false) ),
              audible      = false
      )
      server.freeBuffer( peer.id )
   }
}