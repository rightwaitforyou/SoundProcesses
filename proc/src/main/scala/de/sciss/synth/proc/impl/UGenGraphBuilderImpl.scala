/*
 *  UGenGraphBuilderImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.synth.impl.BasicUGenGraphBuilder
import collection.immutable.{IndexedSeq => Vec, Set => ISet}
import de.sciss.synth.{UGenGraph, Lazy, SynthGraph}
import de.sciss.synth.ugen.ControlProxyLike
import de.sciss.lucre.synth.Sys

private[proc] object UGenGraphBuilderImpl {
  def apply[S <: Sys[S]](aural: AuralPresentation.Running[S], timed: TimedProc[S], time: Long)
                        (implicit tx: S#Tx): UGenGraphBuilder[S] =
    new Impl(aural, timed, time, timed.value.graph.value, tx)

  private final class Impl[S <: Sys[S]](aural: AuralPresentation.Running[S],
                                        val timed: TimedProc[S], val time: Long, g: SynthGraph, val tx: S#Tx)
    extends BasicUGenGraphBuilder with UGenGraphBuilder[S] {
    builder =>

    import UGenGraphBuilder._

    override def toString = "proc.UGenGraph.Builder@" + hashCode.toHexString

    private var remaining: Vec[Lazy]               = g.sources
    private var controlProxies: ISet[ControlProxyLike] = g.controlProxies

    var scanOuts    = Map.empty[String, Int]
    var scanIns     = Map.empty[String, UGenGraphBuilder.ScanIn]
    var missingIns  = Set.empty[MissingIn[S]]
    var attributeIns= Set.empty[String]

    def addScanIn(key: String, numChannels: Int): Int = {
      val fixed = numChannels >= 0
      val res   = aural.scanInNumChannels(timed = timed, time = time, key = key, numChannels = numChannels)(tx)
      scanIns  += key -> UGenGraphBuilder.ScanIn(numChannels = res, fixed = fixed)
      res
    }

    def addScanOut(key: String, numChannels: Int): Unit =
      scanOuts.get(key) match {
        case Some(prevChans) =>
          require(numChannels == prevChans, "Cannot write multiple times to the same scan (" + key +
            ") using different number of channels (" + prevChans + ", " + numChannels + ")")
        case _ =>
          scanOuts += key -> numChannels
      }

    def addAttributeIn(key: String): Int = {
      val res       = aural.attrNumChannels(timed = timed, key = key)(tx)
      attributeIns += key
      res
    }

    def tryBuild(): Boolean = UGenGraph.use(this) {
      var missingElems  = Vector.empty[Lazy]
      missingIns        = Set.empty
      var someSucceeded = false
      while (remaining.nonEmpty) {  // XXX TODO: this can go through many exceptions. perhaps should short circuit?
        val g = SynthGraph {
          remaining.foreach { elem =>
            // save rollback information -- not very elegant; should figure out how scala-stm nesting works
            val savedSourceMap      = sourceMap
            val savedControlNames   = controlNames
            val savedControlValues  = controlValues
            val savedUGens          = ugens
            val savedScanOuts       = scanOuts
            val savedScanIns        = scanIns
            val savedAttrs          = attributeIns
            try {
              elem.force(builder)
              someSucceeded = true
            } catch {
              case miss @ MissingIn(_) =>
                sourceMap           = savedSourceMap
                controlNames        = savedControlNames
                controlValues       = savedControlValues
                ugens               = savedUGens
                scanOuts            = savedScanOuts
                scanIns             = savedScanIns
                attributeIns        = savedAttrs
                missingElems      :+= elem
                missingIns         += miss.asInstanceOf[MissingIn[S]] // XXX TODO yukk
            }
          }
        }
        if (g.nonEmpty) {
          remaining        = g.sources
          controlProxies ++= g.controlProxies
        } else {
          remaining = Vector.empty
        }
      }

      if (missingElems.isEmpty) {
        true // Finished // ( build( controlProxies ))
      } else {
        remaining = missingElems
        false // Partial // ( missingIns, advanced = someSucceeded )
      }
    }

    def isComplete = remaining.isEmpty

    def finish: UGenGraph = UGenGraph.use(this) {
      require(isComplete)
      build(controlProxies)
    }
  }
}