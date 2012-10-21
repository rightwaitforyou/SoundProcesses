/*
 *  ArtifactStore.scala
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

import de.sciss.lucre.{event => evt, Writable, stm, data}
import java.io.File
import impl.{ArtifactStoreImpl => Impl}

object ArtifactStore {
   def apply[ S <: evt.Sys[ S ]]( baseDirectory: File )( implicit tx: S#Tx ) : ArtifactStore[ S ] = Impl[ S ]( baseDirectory )

   def serializer[ S <: evt.Sys[ S ]] : stm.Serializer[ S#Tx, S#Acc, ArtifactStore[ S ]] = Impl.serializer[ S ]
}
trait ArtifactStore[ S <: stm.Sys[ S ]] extends Writable {
   /**
    * Creates a new artifact. This is a side-effect and
    * thus should be called outside of a transaction.
    * The artifact is not in any way registered with the system.
    * Once the artifact has meaningful content, it may be
    * registered by calling the `register` method.
    *
    * @return  the new artifact.
    */
   def create() : Artifact

   /**
    * Registers a significant artifact with the system. That is,
    * stores the artifact, which should have a real resource
    * association, as belonging to the system.
    *
    * @param artifact   the artifact to register
    */
   def register( artifact: Artifact )( implicit tx: S#Tx ) : Unit

   def iterator( implicit tx: S#Tx ) : data.Iterator[ S#Tx, Artifact ]

   def baseDirectory : File
}
