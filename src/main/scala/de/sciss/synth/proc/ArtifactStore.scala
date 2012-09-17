package de.sciss.synth.proc

import de.sciss.lucre.stm.Sys

trait ArtifactStore[ S <: Sys[ S ]] {
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
}
