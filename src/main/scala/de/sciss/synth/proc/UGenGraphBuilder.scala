package de.sciss.synth
package proc

private[proc] object UGenGraphBuilder {
   sealed trait BuildResult
   case object Halted extends BuildResult
   case object Advanced extends BuildResult
   case class Finished( graph: UGenGraph ) extends BuildResult
}
private[proc] trait UGenGraphBuilder extends UGenGraph.Builder {
   def addScanIn( key: String ) : Int
   def addScanOut( key: String, numChannels: Int ) : Unit

   def tryBuild() : UGenGraphBuilder.BuildResult
}
