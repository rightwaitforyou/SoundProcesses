package de.sciss
package synth
package proc
package impl

import java.io.File
import lucre.{DataOutput, stm, DataInput}
import stm.ImmutableSerializer

object ArtifactImpl {
   private final val SER_VERSION = 1

   def apply( path: String ) : Artifact = new Impl( path )
   def read( in: DataInput ) : Artifact = {
      val cookie = in.readUnsignedByte()
      require( cookie == SER_VERSION, "Version mismatch. Expected " + SER_VERSION + " but found " + cookie )
      val path = in.readString()
      ArtifactImpl( path )
   }

   implicit object serializer extends ImmutableSerializer[ Artifact ] {
      def write( v: Artifact, out: DataOutput ) { v.write( out )}
      def read( in: DataInput ) : Artifact = ArtifactImpl.read( in )
   }

   private final case class Impl( path: String ) extends Artifact {
      override def toString = "Artifact(" + path + ")"

      def toFile( implicit store: ArtifactStore[ _ ]) : File = {
         // XXX TODO: in the future we could have a better resolution scheme
         new File( store.baseDirectory, path )
      }

      def write( out: DataOutput ) {
         out.writeString( path )
      }
   }
}