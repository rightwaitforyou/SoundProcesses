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
   /**
    * This method should only be invoked by the `graph.scan.Elem` instances. It requests a scan input, and
    * the method returns the corresponding number of channels, or throws a `MissingIn` exception which
    * is then caught by the main builder body.
    */
   def addScanIn( key: String ) : Int

   /**
    * This method should only be invoked by the `graph.scan.Elem` instances. It declares a scan output along
    * with the number of channels written to it.
    */
   def addScanOut( key: String, numChannels: Int ) : Unit

   /**
    * Current set of used inputs. This is guaranteed to only grow during incremental building, never shrink.
    */
   def scanIns : Set[ String ]
   /**
    * Current set of used outputs. This is guaranteed to only grow during incremental building, never shrink.
    */
   def scanOuts : Map[ String, Int ]

   /**
    * Builds or continuous to build the ugen graph. Since the builder is mutable, `tryBuild` should be called
    * repeatably on the same object as long as a `Partial` result is obtained, until either the transaction is aborted,
    * or a `Finished` result is obtained.
    */
   def tryBuild() : UGenGraphBuilder.BuildResult[ S ]
}
