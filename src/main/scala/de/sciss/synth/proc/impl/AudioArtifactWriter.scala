package de.sciss
package synth
package proc
package impl

import concurrent.stm.Ref
import java.io.File

final class AudioArtifactWriter( segm: Grapheme.Segment.Audio, file: File, val bus: RichAudioBus )
extends DynamicAudioBusUser with RichAudioBus.User {
   private val synthRef = Ref( Option.empty[ RichSynth ])

   def add()( implicit tx: ProcTxn ) {
      val server        = bus.server
//      val bufPeer       = Buffer( server )
      val rb            = RichBuffer( server )
      val numChannels   = segm.numChannels

      val sg = SynthGraph {
         import ugen._
         val buf  = "buf".ir
         val dur  = "dur".ir( 1 )
         val out  = "out".kr
         val sig  = DiskIn.ar( numChannels, buf )
         Line.kr( start = 0, end = 0, dur = dur, doneAction = freeSelf )
         Out.ar( out, sig )
      }

      val rd      = RichSynthDef( server, sg )
      val synPeer = Synth( server )
      val rs      = RichSynth( synPeer, rd )

      rs.onEndTxn { implicit tx =>
//         bufPeer.close( bufPeer.freeMsg )
         rb.closeAndFree()
      }

      val audioVal   = segm.value
//      val path       = audioVal.artifact.toFile.getAbsolutePath
      val path       = file.getAbsolutePath
      val fileStart  = audioVal.offset
      val target     = RichGroup.default( server ) // XXX

      val args: Seq[ ControlSetMap ] = Nil

      rb.cue( path, fileStart )
      rs.play( target = target, args = args, buffers = rb :: Nil )

      val oldSynth = synthRef.swap( Some( rs ))( tx.peer )
      bus.addWriter( this )

      require( oldSynth.isEmpty, "AudioArtifactWriter.add() : old synth still playing" )
   }

   def remove()( implicit tx: ProcTxn ) {
      bus.removeWriter( this )
   }

   def migrateTo( newBus: RichAudioBus )( implicit tx: ProcTxn ) = sys.error("TODO"): DynamicAudioBusUser

   def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) {
      sys.error( "TODO" )
   }
}