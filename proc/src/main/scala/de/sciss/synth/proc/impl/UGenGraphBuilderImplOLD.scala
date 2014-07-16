///*
// *  UGenGraphBuilderImpl.scala
// *  (SoundProcesses)
// *
// *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
// *
// *	This software is published under the GNU General Public License v2+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.synth.proc
//package impl
//
//import de.sciss.synth.impl.BasicUGenGraphBuilder
//import de.sciss.synth.proc.UGenGraphBuilder.{StreamIn, ScanIn}
//import collection.immutable.{IndexedSeq => Vec, Set => ISet}
//import de.sciss.synth.{ControlBus => SControlBus, UGenGraph, Lazy, SynthGraph}
//import de.sciss.synth.ugen.ControlProxyLike
//import de.sciss.lucre.synth.Sys
//
//private[proc] object UGenGraphBuilderImplOLD {
//  def apply[S <: Sys[S]](aural: AuralPresentationOLD.Running[S], timed: TimedProc[S], time: Long)
//                        (implicit tx: S#Tx): UGenGraphBuilderOLD[S] = {
//    val proc = timed.value.elem.peer
//    new Impl(aural, timed, time, proc.graph.value, tx)
//  }
//
//  private final class Impl[S <: Sys[S]](aural: AuralPresentationOLD.Running[S],
//                                        val timed: TimedProc[S], val time: Long, g: SynthGraph, val tx: S#Tx)
//    extends BasicUGenGraphBuilder with UGenGraphBuilderOLD[S] {
//    builder =>
//
//    import UGenGraphBuilderOLD._
//
//    override def toString = s"proc.UGenGraph.Builder@${hashCode.toHexString}"
//
//    private var remaining     : Vec [Lazy]              = g.sources
//    private var controlProxies: ISet[ControlProxyLike]  = g.controlProxies
//
//    var scanOuts    = Map.empty[String, Int]
//    var scanIns     = Map.empty[String, ScanIn]
//    var missingInsOLD  = Set.empty[MissingIn[S]]
//    var attributeIns= Set.empty[String]
//    var streamIns   = Map.empty[String, List[StreamIn]]
//
//    def sensorBus: SControlBus = aural.sensorBus
//
//    def addScanIn(key: String, numChannels: Int): Int = {
//      val fixed = numChannels >= 0
//      val res   = aural.scanInNumChannels(timed = timed, time = time, key = key, numChannels = numChannels)(tx)
//      scanIns  += key -> ScanIn(numChannels = res, fixed = fixed)
//      res
//    }
//
//    def addScanOut(key: String, numChannels: Int): Unit =
//      scanOuts.get(key).fold {
//        scanOuts += key -> numChannels
//      } { prevChans =>
//          if (numChannels != prevChans) {
//            val s1 = s"Cannot write multiple times to the same scan ($key)"
//            val s2 = s"using different number of channels ($prevChans, $numChannels)"
//            sys.error(s"$s1 $s2")
//          }
//      }
//
//    def addAttributeIn(key: String): Int = {
//      val res       = aural.attrNumChannels(timed = timed, key = key)(tx)
//      attributeIns += key
//      res
//    }
//
//    def addStreamIn(key: String, info: StreamIn): (Int, Int) = {
//      val numCh = aural.attrNumChannels(timed = timed, key = key)(tx)
//      val idx   = if (info.isEmpty) {
//        if (!streamIns.contains(key)) streamIns += key -> Nil
//        0
//      } else {
//        val oldValue = streamIns.getOrElse(key, Nil)
//        streamIns += key -> (info :: oldValue)
//        oldValue.size
//      }
//      (numCh, idx)
//    }
//
//    def tryBuild(): Boolean = UGenGraph.use(this) {
//      var missingElems  = Vector.empty[Lazy]
//      missingInsOLD        = Set.empty
//      var someSucceeded = false
//      while (remaining.nonEmpty) {  // XXX TODO: this can go through many exceptions. perhaps should short circuit?
//        val g = SynthGraph {
//          remaining.foreach { elem =>
//            // save rollback information -- not very elegant; should figure out how scala-stm nesting works
//            val savedSourceMap      = sourceMap
//            val savedControlNames   = controlNames
//            val savedControlValues  = controlValues
//            val savedUGens          = ugens
//            val savedScanOuts       = scanOuts
//            val savedScanIns        = scanIns
//            val savedAttrs          = attributeIns
//            val savedStreams        = streamIns
//            try {
//              elem.force(builder)
//              someSucceeded = true
//            } catch {
//              case miss @ MissingIn(_) =>
//                sourceMap           = savedSourceMap
//                controlNames        = savedControlNames
//                controlValues       = savedControlValues
//                ugens               = savedUGens
//                scanOuts            = savedScanOuts
//                scanIns             = savedScanIns
//                attributeIns        = savedAttrs
//                streamIns           = savedStreams
//                missingElems      :+= elem
//                missingInsOLD         += miss.asInstanceOf[MissingIn[S]] // XXX TODO not cool
//            }
//          }
//        }
//        if (g.nonEmpty) {
//          remaining        = g.sources
//          controlProxies ++= g.controlProxies
//        } else {
//          remaining        = Vector.empty
//        }
//      }
//
//      if (missingElems.isEmpty) {
//        true // Finished // ( build( controlProxies ))
//      } else {
//        remaining = missingElems
//        false // Partial // ( missingIns, advanced = someSucceeded )
//      }
//    }
//
//    def isComplete = remaining.isEmpty
//
//    def finish: UGenGraph = UGenGraph.use(this) {
//      require(isComplete)
//      build(controlProxies)
//    }
//  }
//}