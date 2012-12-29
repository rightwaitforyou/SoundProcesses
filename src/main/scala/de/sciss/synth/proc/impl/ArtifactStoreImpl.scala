/*
 *  ArtifactStoreImpl.scala
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

import java.io.File
import lucre.{DataInput, DataOutput, stm, expr, data, event => evt}
import expr.LinkedList

object ArtifactStoreImpl {
   private final val SER_VERSION = 1

   def apply[ S <: evt.Sys[ S ]]( baseDirectory: File )( implicit tx: S#Tx ) : ArtifactStore[ S ] = {
      val ll = LinkedList.Modifiable[ S, Artifact ]
      new Impl[ S ]( ll, baseDirectory )
   }

   def serializer[ S <: evt.Sys[ S ]] : stm.Serializer[ S#Tx, S#Acc, ArtifactStore[ S ]] = new Ser[ S ]

   def read[ S <: evt.Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : ArtifactStore[ S ] = {
      val cookie  = in.readUnsignedByte()
      require( cookie == SER_VERSION, "Version mismatch. Expected " + SER_VERSION + " but found " + cookie )

      val ll            = LinkedList.Modifiable.read[ S, Artifact ]( in, access )
      val baseDirectory = new File( in.readString() )
      new Impl[ S ]( ll, baseDirectory )
   }

   private final class Ser[ S <: evt.Sys[ S ]]
   extends stm.Serializer[ S#Tx, S#Acc, ArtifactStore[ S ]] {
      def write( v: ArtifactStore[ S ], out: DataOutput) {
         v.write( out )
      }

      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : ArtifactStore[ S ] = {
         ArtifactStoreImpl.read( in, access )
      }
   }

   private final class Impl[ S <: stm.Sys[ S ]]( ll: LinkedList.Modifiable[ S, Artifact, Unit ], val baseDirectory: File )
   extends ArtifactStore[ S ] {
//      implicit protected val artiSer = Artifact.serializer[ S ]( this )
//      implicit def self: ArtifactStore[ S ] = this

      /**
       * Creates a new artifact.
       */
      def create() : Artifact = {
         baseDirectory.mkdirs()
         val f = File.createTempFile( "artifact", ".bin", baseDirectory )
         Artifact( f.getName )
      }

      /**
       * Registers a significant artifact with the system.
       */
      def register( artifact: Artifact )( implicit tx: S#Tx ) {
         ll.addLast( artifact )
      }

      def iterator( implicit tx: S#Tx ) : data.Iterator[ S#Tx, Artifact ] = ll.iterator

      def write( out: DataOutput ) {
         out.writeUnsignedByte( SER_VERSION )
         ll.write( out )
         out.writeString( baseDirectory.getPath )
      }
   }
}