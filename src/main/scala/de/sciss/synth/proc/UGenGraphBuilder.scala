package de.sciss.synth
package proc

import de.sciss.lucre.stm.Sys
import impl.{UGenGraphBuilderImpl => Impl}
import util.control.ControlThrowable

private[proc] object UGenGraphBuilder {
   sealed trait BuildResult[ S <: Sys[ S ]]
//   case object Halted extends BuildResult
//   case object Advanced extends BuildResult
   final case class Finished[ S <: Sys[ S ]]( graph: UGenGraph /*, scanIns: Set[ String ], scanOuts: Map[ String, Int ]*/)
   extends BuildResult[ S ]

   final case class Partial[ S <: Sys[ S ]]( missingScanIns: Set[ MissingIn[ S ]], advanced: Boolean )
   extends BuildResult[ S ]

   final case class MissingIn[ S <: Sys[ S ]]( timed: TimedProc[ S ], key: String ) extends ControlThrowable

   /**
    * '''Note''': The resulting object is mutable, therefore must not be shared across threads and also must be
    * created and consumed within the same transaction. That is to say, to be transactionally safe, it may only
    * be stored in a `TxnLocal`, but not a full STM ref.
    */
   def apply[ S <: Sys[ S ]]( aural: AuralPresentation.Running[ S ], timed: TimedProc[ S ], time: Long )
                            ( implicit tx: S#Tx ) : UGenGraphBuilder[ S ] =
      Impl( aural, timed, time )
}
private[proc] trait UGenGraphBuilder[ S <: Sys[ S ]] extends UGenGraph.Builder {
   def addScanIn( key: String ) : Int
   def addScanOut( key: String, numChannels: Int ) : Unit

   def scanIns : Set[ String ]
   def scanOuts : Map[ String, Int ]

   /**
    * Builds or continuous to build the ugen graph. Since the builder is mutable, `tryBuild` should be called
    * repeatably on the same object as long as a `Partial` result is obtained, until either the transaction is aborted,
    * or a `Finished` result is obtained.
    */
   def tryBuild() : UGenGraphBuilder.BuildResult[ S ]
}
