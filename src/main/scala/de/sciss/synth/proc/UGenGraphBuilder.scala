package de.sciss.synth.proc

private[proc] trait UGenGraphBuilder {
   def addScanIn( key: String ) : Int
   def addScanOut( key: String, numChannels: Int ) : Unit
}
