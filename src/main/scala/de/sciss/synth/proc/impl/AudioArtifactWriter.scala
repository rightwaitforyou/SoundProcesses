/*
 *  AudioArtifactWriter.scala
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

package de.sciss
package synth
package proc
package impl

import concurrent.stm.Ref
import java.io.File
import lucre.bitemp.Span

final class AudioArtifactWriter( segm: Grapheme.Segment.Audio, file: File, server: Server, sampleRate: Double )
extends DynamicBusUser /* DynamicAudioBusUser */ /* with RichAudioBus.User */ {
   private val synthRef = Ref( Option.empty[ Synth ])
   val bus              = RichBus.audio( server, segm.numChannels )

   def add()( implicit tx: Txn ) {
//      val bufPeer       = Buffer( server )
      val numChannels   = bus.numChannels
//      val rb            = Buffer( server )

      val sg = SynthGraph {
         import ugen._
         val buf  = "buf".ir
         val dur  = "dur".ir( 1 )
         val out  = "out".kr
         val sig  = DiskIn.ar( numChannels, buf )
         Line.kr( start = 0, end = 0, dur = dur, doneAction = freeSelf )
         Out.ar( out, sig )
      }

//      val rd         = SynthDef( server, sg, nameHint = Some( "audio-artifact" ))

      val audioVal   = segm.value
//      val path       = audioVal.artifact.toFile.getAbsolutePath
      val path       = file.getAbsolutePath
      val fileStart  = audioVal.offset
      val target     = server.defaultGroup
      val dur        = segm.span match {
         case sp @ Span( _, _ )  => sp.length / sampleRate
         case _                  => audioVal.spec.numFrames / audioVal.spec.sampleRate
      }

      val rb = Buffer.diskIn( server )( path, startFrame = fileStart, numChannels = numChannels )
      val args: Seq[ ControlSetMap ] = Seq( "buf" -> rb.id, "dur" -> dur )

//      val rs = rd.play( target = target, args = args, buffers = rb :: Nil )
      val rs = Synth( sg, nameHint = Some( "audio-artifact" ))( target = target, args = args, dependencies = rb :: Nil )

      rs.onEndTxn { implicit tx =>
//         bufPeer.close( bufPeer.freeMsg )
         rb.dispose()
      }

//      rs.play( target = target, args = args, buffers = rb :: Nil )
      rs.write( bus -> "out" )

      val oldSynth = synthRef.swap( Some( rs ))( tx.peer )
//      bus.addWriter( this )

      require( oldSynth.isEmpty, "AudioArtifactWriter.add() : old synth still playing" )
   }

   def remove()( implicit tx: Txn ) {
      val rs = synthRef.swap( None )( tx.peer ).getOrElse(
         sys.error( "AudioArtifactWriter.remove() : there was no synth playing" )
      )
      rs.free()

//      bus.removeWriter( this )
   }

//   def migrateTo( newBus: RichAudioBus )( implicit tx: ProcTxn ) = ...
}