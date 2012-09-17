package de.sciss.synth.proc.impl

import de.sciss.lucre.stm.ImmutableSerializer
import de.sciss.synth.io.{SampleFormat, AudioFileType, AudioFileSpec => AFS }
import de.sciss.lucre.{DataOutput, DataInput}
import java.nio.ByteOrder
import annotation.switch
import de.sciss.synth.{cubShape, sqrShape, curveShape, stepShape, linShape, expShape, sinShape, welchShape, Env}

object CommonSerializers {
   implicit object AudioFileSpec extends ImmutableSerializer[ AFS ] {
      def write( spec: AFS, out: DataOutput ) {
         val fid = spec.fileType match {
            case AudioFileType.AIFF    => 0
            case AudioFileType.Wave    => 1
            case AudioFileType.Wave64  => 2
            case AudioFileType.IRCAM   => 3
            case AudioFileType.NeXT    => 4
            case other                 => sys.error( "Unexpected audio file type " + other )
         }
         out.writeUnsignedByte( fid )
         val sid = spec.sampleFormat match {
            case SampleFormat.Int16    => 0
            case SampleFormat.Int24    => 1
            case SampleFormat.Float    => 2
            case SampleFormat.Int32    => 3
            case SampleFormat.Double   => 4
            case SampleFormat.UInt8    => 5
            case SampleFormat.Int8     => 6
            case other                 => sys.error( "Unexpected sample format " + other )
         }
         out.writeUnsignedByte( sid )
         out.writeInt( spec.numChannels )
         out.writeDouble( spec.sampleRate )
         val bid = spec.byteOrder match {
            case None                              => 0
            case Some( ByteOrder.LITTLE_ENDIAN )   => 1
            case Some( ByteOrder.BIG_ENDIAN )      => 2
            case other                             => sys.error( "Unexpected byte order " + other )
         }
         out.writeUnsignedByte( bid )
         out.writeLong( spec.numFrames )
      }

      def read( in: DataInput ) : AFS = {
         val fileType = (in.readUnsignedByte(): @switch) match {
            case 0   => AudioFileType.AIFF
            case 1   => AudioFileType.Wave
            case 2   => AudioFileType.Wave64
            case 3   => AudioFileType.IRCAM
            case 4   => AudioFileType.NeXT
            case other => sys.error( "Unexpected audio file type ID " + other )
         }
         val sampleFormat = (in.readUnsignedByte(): @switch) match {
            case 0   => SampleFormat.Int16
            case 1   => SampleFormat.Int24
            case 2   => SampleFormat.Float
            case 3   => SampleFormat.Int32
            case 4   => SampleFormat.Double
            case 5   => SampleFormat.UInt8
            case 6   => SampleFormat.Int8
            case other => sys.error( "Unexpected sample format ID " + other )
         }
         val numChannels   = in.readInt()
         val sampleRate    = in.readDouble()
         val byteOrder     = (in.readUnsignedByte(): @switch) match {
            case 0   => None
            case 1   => Some( ByteOrder.LITTLE_ENDIAN )
            case 2   => Some( ByteOrder.BIG_ENDIAN )
            case other => sys.error( "Unexpected byte order ID " + other )
         }
         val numFrames     = in.readLong()
         AFS( fileType, sampleFormat, numChannels, sampleRate, byteOrder, numFrames )
      }
   }

   implicit object EnvConstShape extends ImmutableSerializer[ Env.ConstShape ] {
      def write( shape: Env.ConstShape, out: DataOutput ) {
         out.writeInt( shape.id )
      }

      def read( in: DataInput ) : Env.ConstShape = {
         (in.readInt(): @switch) match {
            case stepShape.id    => stepShape
            case linShape.id     => linShape
            case expShape.id     => expShape
            case sinShape.id     => sinShape
            case welchShape.id   => welchShape
            case curveShape.id   => curveShape( in.readFloat() )
            case sqrShape.id     => sqrShape
            case cubShape.id     => cubShape
            case other           => sys.error( "Unexpected envelope shape ID " + other )
         }
      }
   }
}
