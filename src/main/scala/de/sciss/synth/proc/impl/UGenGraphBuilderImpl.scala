package de.sciss.synth
package proc
package impl

import de.sciss.synth.impl.BasicUGenGraphBuilder
import collection.immutable.{IndexedSeq => IIdxSeq, Set => ISet}
import util.control.ControlThrowable
import concurrent.stm.{InTxn, Txn}
import de.sciss.lucre.stm.Sys

private[proc] class MissingInfo extends ControlThrowable

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

//      @inline private def getTxn : ProcTxn = ProcTxn()( Txn.findCurrent.getOrElse( sys.error( "Cannot find transaction" )))

      def addScanIn( key: String ) : Int = {
         aural.addScanIn( proc, time, key )( tx )
      }

      def addScanOut( key: String, numChannels: Int ) {
         aural.addScanOut( proc, time, key, numChannels )( tx )
      }

      def tryBuild() : BuildResult = {
         var missing       = IIdxSeq.empty[ Lazy ]
         var someSucceeded = false
         while( remaining.nonEmpty ) {
            val g = SynthGraph {
               remaining.foreach { elem =>
                  // save rollback information -- not very elegant :-(
                  val savedSourceMap      = sourceMap
                  val savedControlNames   = controlNames
                  val savedControlValues  = controlValues
                  val savedUGens          = ugens
                  try {
                     elem.force( builder )
                     someSucceeded        = true
                  } catch {
                     case e: MissingInfo =>
                        sourceMap         = savedSourceMap
                        controlNames      = savedControlNames
                        controlValues     = savedControlValues
                        ugens             = savedUGens
                        missing         :+= elem
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

         if( missing.isEmpty ) {
            Finished( build( controlProxies ))
         } else {
            remaining = missing
            if( someSucceeded ) Advanced else Halted
         }
      }
   }
}