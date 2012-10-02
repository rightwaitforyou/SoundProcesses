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

package de.sciss.synth.proc

import de.sciss.synth.Buffer
import de.sciss.synth.io.{SampleFormat, AudioFileType}
import ProcTxn.{Always, RequiresChange}

final case class RichBuffer( buf: Buffer ) /* extends RichObject */ {
   val isOnline: RichState   = new RichState( this, "isOnline", false )
   val hasContent: RichState = new RichState( this, "hasContent", false )

   def server = buf.server

   def alloc( numFrames: Int, numChannels: Int = 1 )( implicit tx: ProcTxn ) {
      tx.add( buf.allocMsg( numFrames, numChannels ), change = Some( (RequiresChange, isOnline, true) ), audible = false )
   }

   def cue( path: String, startFrame: Int = 0 )( implicit tx: ProcTxn ) {
      tx.add( buf.cueMsg( path, startFrame ), change = Some( (Always, hasContent, true) ),
         audible = false, dependencies = Map( isOnline -> true ))
   }

   def record( path: String, fileType: AudioFileType, sampleFormat: SampleFormat )( implicit tx: ProcTxn ) {
      tx.add( buf.writeMsg( path, fileType, sampleFormat, 0, 0, leaveOpen = true ),
         change = Some( (Always, hasContent, true) ), audible = false, dependencies = Map( isOnline -> true )) // hasContent is a bit misleading...
   }

   def zero( implicit tx: ProcTxn ) {
      tx.add( buf.zeroMsg, change = Some( (Always, hasContent, true) ), audible = false, dependencies = Map( isOnline -> true ))
   }
}