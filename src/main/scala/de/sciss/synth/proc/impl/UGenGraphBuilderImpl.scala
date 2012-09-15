package de.sciss.synth
package proc
package impl

import de.sciss.synth.impl.BasicUGenGraphBuilder
import collection.immutable.{IndexedSeq => IIdxSeq, Set => ISet}
import de.sciss.lucre.stm.Sys

//private[proc] final case class MissingInfo( key: String ) extends ControlThrowable

private[proc] object UGenGraphBuilderImpl {
   def apply[ S <: Sys[ S ]]( aural: AuralPresentation.Running[ S ], timed: TimedProc[ S ], time: Long )
                            ( implicit tx: S#Tx ) : UGenGraphBuilder[ S ] =
      new Impl( aural, timed, time, timed.value.graph.value, tx )

   private final class Impl[ S <: Sys[ S ]]( aural: AuralPresentation.Running[ S ],
                                             timed: TimedProc[ S ], time: Long, g: SynthGraph, tx: S#Tx )
   extends BasicUGenGraphBuilder with UGenGraphBuilder[ S ] {
      builder =>

      import UGenGraphBuilder._

      override def toString = "proc.UGenGraph.Builder@" + hashCode.toHexString

      private var remaining: IIdxSeq[ Lazy ]                   = g.sources
      private var controlProxies: ISet[ ControlProxyLike[ _ ]] = g.controlProxies
      var scanOuts                                             = Map.empty[ String, Int ]
      var scanIns                                              = Set.empty[ String ] // , Scan_.Value[ S ]]
//      private var missingScanIns                               = Set.empty[ String ]

//      @inline private def getTxn : ProcTxn = ProcTxn()( Txn.findCurrent.getOrElse( sys.error( "Cannot find transaction" )))

      def addScanIn( key: String ) : Int = {
         val res = aural.scanInNumChannels( timed, time, key )( tx )
         scanIns += key // -> value
         res
      }

      def addScanOut( key: String, numChannels: Int ) {
//         aural.addScanOut( proc, time, key, numChannels )( tx )
         scanOuts.get( key ) match {
            case Some( prevChans ) =>
               require( numChannels == prevChans, "Cannot write multiple times to the same scan (" + key +
                  ") using different number of channels (" + prevChans + ", " + numChannels + ")" )
            case _ =>
               scanOuts += key -> numChannels
         }
      }

      def tryBuild() : BuildResult[ S ] = {
         var missingElems  = IIdxSeq.empty[ Lazy ]
         var missingIns    = Set.empty[ MissingIn[ S ]]
         var someSucceeded = false
         while( remaining.nonEmpty ) {
            val g = SynthGraph {
               remaining.foreach { elem =>
                  // save rollback information -- not very elegant; should figure out how scala-stm nesting works
                  val savedSourceMap      = sourceMap
                  val savedControlNames   = controlNames
                  val savedControlValues  = controlValues
                  val savedUGens          = ugens
                  val savedScanOuts       = scanOuts
                  val savedScanIns        = scanIns
                  try {
                     elem.force( builder )
                     someSucceeded        = true
                  } catch {
                     case miss @ MissingIn( _, _ ) =>
                        sourceMap         = savedSourceMap
                        controlNames      = savedControlNames
                        controlValues     = savedControlValues
                        ugens             = savedUGens
                        scanOuts          = savedScanOuts
                        scanIns           = savedScanIns
                        missingElems     :+= elem
                        missingIns        += miss.asInstanceOf[ MissingIn[ S ]]  // XXX TODO yukk
                  }
               }
            }
            if( g.nonEmpty ) {
               remaining = g.sources
               controlProxies ++= g.controlProxies
            } else {
               remaining = IIdxSeq.empty
            }
         }

         if( missingElems.isEmpty ) {
            Finished( build( controlProxies ))
         } else {
            remaining = missingElems
            Partial( missingIns, advanced = someSucceeded )
         }
      }
   }
}