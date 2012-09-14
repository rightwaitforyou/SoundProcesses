package de.sciss.synth
package proc
package impl

import de.sciss.synth.impl.BasicUGenGraphBuilder
import collection.immutable.{IndexedSeq => IIdxSeq, Set => ISet}
import util.control.ControlThrowable
import annotation.tailrec

private[proc] case object MissingInfo extends ControlThrowable

private[proc] object UGenGraphBuilderImpl {
   sealed trait BuildResult
   case object Halted extends BuildResult
   case object Advanced extends BuildResult
   case class Finished( graph: UGenGraph ) extends BuildResult
}
private[proc] final class UGenGraphBuilderImpl( g: SynthGraph )
extends BasicUGenGraphBuilder with UGenGraphBuilder {
   builder =>

   import UGenGraphBuilderImpl._

   override def toString = "proc.UGenGraph.Builder@" + hashCode.toHexString

   private var remaining: IIdxSeq[ Lazy ]                   = g.sources
   private var controlProxies: ISet[ ControlProxyLike[ _ ]] = g.controlProxies

   def addScanIn( key: String ) : Int = {
      throw MissingInfo
      sys.error( "TODO" )
   }

   def addScanOut( key: String, numChannels: Int ) {
      sys.error( "TODO" )
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
                  case MissingInfo =>
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
