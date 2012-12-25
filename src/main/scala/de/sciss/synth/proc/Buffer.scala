package de.sciss.synth.proc

import de.sciss.synth.io.{AudioFileType, SampleFormat}

object Buffer {
   def diskIn( server: Server, path: String, startFrame: Long = 0L,
               numFrames: Int = SoundProcesses.cueBufferSize, numChannels: Int = 1 ) : Buffer = {
      SoundProcesses.validateCueBufferSize( numFrames )
      ???
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

   trait Modifiable extends Buffer {
      def zero()( implicit tx: Txn ) : Unit
      def read( path: String, fileStartFrame: Long = 0L, numFrames: Int = -1, bufStartFrame: Int = 0 )
              ( implicit tx: Txn ) : Unit
   }
}
trait Buffer {
   def id: Int
   def server: Server
   def dispose()( implicit tx: Txn ) : Unit
}