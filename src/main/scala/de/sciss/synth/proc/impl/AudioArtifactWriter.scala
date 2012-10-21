package de.sciss
package synth
package proc
package impl

import concurrent.stm.Ref
import java.io.File
import lucre.bitemp.Span

final class AudioArtifactWriter( segm: Grapheme.Segment.Audio, file: File, server: Server, sampleRate: Double )
extends DynamicBusUser /* DynamicAudioBusUser */ /* with RichAudioBus.User */ {
   private val synthRef = Ref( Option.empty[ RichSynth ])
   val bus              = RichBus.audio( server, segm.numChannels )

   def add()( implicit tx: ProcTxn ) {
//      val bufPeer       = Buffer( server )
      val rb                  = RichBuffer( server )

      val sg = SynthGraph {
         import ugen._
         val buf  = "buf".ir
         val dur  = "dur".ir( 1 )
         val out  = "out".kr
         val sig  = DiskIn.ar( bus.numChannels, buf )
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
      val dur        = segm.span match {
         case sp @ Span( _, _ )  => sp.length / sampleRate
         case _                  => audioVal.spec.numFrames / audioVal.spec.sampleRate
      }

      val args: Seq[ ControlSetMap ] = Seq( "buf" -> rb.buf.id, "dur" -> dur )

      rb.cue( path, fileStart )
      rs.play( target = target, args = args, buffers = rb :: Nil )
      rs.write( bus -> "out" )

      val oldSynth = synthRef.swap( Some( rs ))( tx.peer )
//      bus.addWriter( this )

      require( oldSynth.isEmpty, "AudioArtifactWriter.add() : old synth still playing" )
   }

   def remove()( implicit tx: ProcTxn ) {
      val rs = synthRef.swap( None )( tx.peer ).getOrElse(
         sys.error( "AudioArtifactWriter.remove() : there was no synth playing" )
      )
      rs.free()

//      bus.removeWriter( this )
   }

//   def migrateTo( newBus: RichAudioBus )( implicit tx: ProcTxn ) = sys.error("TODO"): DynamicAudioBusUser
}