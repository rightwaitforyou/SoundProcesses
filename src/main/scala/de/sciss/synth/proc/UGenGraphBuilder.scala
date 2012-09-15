package de.sciss.synth
package proc

import de.sciss.lucre.stm.Sys
import impl.AuralPresentationImpl

private[proc] object UGenGraphBuilder {
   sealed trait BuildResult
//   case object Halted extends BuildResult
//   case object Advanced extends BuildResult
   case class Finished( graph: UGenGraph, scanIns: Set[ String ], scanOuts: Map[ String, Int ]) extends BuildResult
   case class Partial( missingScanIns: Set[ String ], advanced: Boolean ) extends BuildResult

   def apply[ S <: Sys[ S ]]( aural: AuralPresentationImpl.Running[ S ], proc: Proc[ S ], time: Long )( implicit tx: S#Tx ) : UGenGraphBuilder =
      impl.UGenGraphBuilderImpl( aural, proc, time )
}
private[proc] trait UGenGraphBuilder extends UGenGraph.Builder {
   def addScanIn( key: String ) : Int
   def addScanOut( key: String, numChannels: Int ) : Unit

   def tryBuild() : UGenGraphBuilder.BuildResult
}
