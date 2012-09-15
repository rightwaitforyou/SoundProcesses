package de.sciss.synth
package proc

import de.sciss.lucre.stm.Sys
import impl.{UGenGraphBuilderImpl => Impl}

private[proc] object UGenGraphBuilder {
   sealed trait BuildResult
//   case object Halted extends BuildResult
//   case object Advanced extends BuildResult
   case class Finished( graph: UGenGraph, scanIns: Set[ String ], scanOuts: Map[ String, Int ]) extends BuildResult
   case class Partial( missingScanIns: Set[ String ], advanced: Boolean ) extends BuildResult

   /**
    * '''Note''': The resulting object is mutable, therefore must not be shared across threads and also must be
    * created and consumed within the same transaction. That is to say, to be transactionally safe, it may only
    * be stored in a `TxnLocal`, but not a full STM ref.
    */
   def apply[ S <: Sys[ S ]]( aural: AuralPresentation.Running[ S ], timed: TimedProc[ S ], time: Long )
                            ( implicit tx: S#Tx ) : UGenGraphBuilder =
      Impl( aural, timed, time )
}
private[proc] trait UGenGraphBuilder extends UGenGraph.Builder {
   def addScanIn( key: String ) : Int
   def addScanOut( key: String, numChannels: Int ) : Unit

   /**
    * Builds or continuous to build the ugen graph. Since the builder is mutable, `tryBuild` should be called
    * repeatably on the same object as long as a `Partial` result is obtained, until either the transaction is aborted,
    * or a `Finished` result is obtained.
    */
   def tryBuild() : UGenGraphBuilder.BuildResult
}
