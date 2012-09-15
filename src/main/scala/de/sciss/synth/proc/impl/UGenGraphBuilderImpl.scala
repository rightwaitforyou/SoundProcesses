package de.sciss.synth
package proc
package impl

import de.sciss.synth.impl.BasicUGenGraphBuilder
import collection.immutable.{IndexedSeq => IIdxSeq, Set => ISet}
import util.control.ControlThrowable
import concurrent.stm.{InTxn, Txn}
import de.sciss.lucre.stm.Sys

private[proc] final case class MissingInfo( key: String ) extends ControlThrowable

private[proc] object UGenGraphBuilderImpl {
   def apply[ S <: Sys[ S ]]( aural: AuralPresentationImpl.Running[ S ], proc: Proc[ S ], time: Long )( implicit tx: S#Tx ) : UGenGraphBuilder =
      new Impl( aural, proc, time, proc.graph.value, tx )

   private final class Impl[ S <: Sys[ S ]]( aural: AuralPresentationImpl.Running[ S ], proc: Proc[ S ], time: Long,
                                             g: SynthGraph, tx: S#Tx )
   extends BasicUGenGraphBuilder with UGenGraphBuilder {
      builder =>

      import UGenGraphBuilder._

      override def toString = "proc.UGenGraph.Builder@" + hashCode.toHexString

      private var remaining: IIdxSeq[ Lazy ]                   = g.sources
      private var controlProxies: ISet[ ControlProxyLike[ _ ]] = g.controlProxies
      private var scanOuts                                     = Map.empty[ String, Int ]
      private var scanIns                                      = Set.empty[ String ] // , Scan_.Value[ S ]]
//      private var missingScanIns                               = Set.empty[ String ]

//      @inline private def getTxn : ProcTxn = ProcTxn()( Txn.findCurrent.getOrElse( sys.error( "Cannot find transaction" )))

      def addScanIn( key: String ) : Int = {
         aural.scanInValue( proc, time, key )( tx ) match {
            case Some( value ) =>
               scanIns += key // -> value
               value.numChannels
            case _ =>
//               missingScanIns += key
               throw MissingInfo( key )
         }
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

      def tryBuild() : BuildResult = {
         var missingElems  = IIdxSeq.empty[ Lazy ]
         var missingKeys   = Set.empty[ String ]
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
                     case MissingInfo( key ) =>
                        sourceMap         = savedSourceMap
                        controlNames      = savedControlNames
                        controlValues     = savedControlValues
                        ugens             = savedUGens
                        scanOuts          = savedScanOuts
                        scanIns           = savedScanIns
                        missingElems     :+= elem
                        missingKeys       += key
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
            Finished( build( controlProxies ), scanIns, scanOuts )
         } else {
            remaining = missingElems
            Partial( missingKeys, advanced = someSucceeded )
         }
      }
   }
}