package de.sciss.synth.proc
package impl

import de.sciss.lucre.synth.Sys
import de.sciss.synth.impl.BasicUGenGraphBuilder
import de.sciss.synth.ugen.ControlProxyLike
import de.sciss.synth.{UGen, Lazy, SynthGraph, UGenGraph}

import scala.util.control.ControlThrowable
import scala.collection.immutable.{IndexedSeq => Vec}

object UGenGraphBuilder {
  def outsideOfContext(): Nothing = sys.error("Expansion out of context")

  /** An exception thrown when during incremental build an input is required for which the underlying source
    * cannot yet be determined.
    *
    * This can be a case class because it is used only within the same transaction,
    * and thereby the `timed` argument does not become stale.
    *
    * @param scan    the scan which is the ''source'' of the required input
    */
  final case class MissingIn[S <: Sys[S]](scan: Scan[S]) extends ControlThrowable

    /** '''Note''': The resulting object is mutable, therefore must not be shared across threads and also must be
      * created and consumed within the same transaction. That is to say, to be transactionally safe, it may only
      * be stored in a `TxnLocal`, but not a full STM ref.
      */
    def apply[S <: Sys[S]](aural: AuralObj.ProcData[S], proc: Obj.T[S, Proc.Elem])
                          (implicit tx: S#Tx): State[S] = {
      val g = proc.elem.peer.graph.value
      val in = new IncompleteImpl[S](aural,
        remaining = g.sources, controlProxies = g.controlProxies,
        ugens = Vec.empty, controlValues = Vec.empty, controlNames = Vec.empty,
        sourceMap = Map.empty, scanOuts = Map.empty, scanIns = Map.empty,
        attributeIns = Set.empty, streamIns = Map.empty, missingIns = Set.empty
      )
      in.retry()
    }

  case class ScanIn(numChannels: Int, fixed: Boolean)

  object StreamIn {
    val empty = StreamIn(0.0, 0)
  }
  case class StreamIn(maxSpeed: Double, interp: Int) {
    /** Empty indicates that the stream is solely used for information
      * purposes such as `BufChannels`.
      */
    def isEmpty: Boolean = interp == 0

    /** Native indicates that the stream will be transported by the UGen
      * itself, i.e. via `DiskIn` or `VDiskIn`.
      */
    def isNative: Boolean = interp == -1
  }
  
  // ---- impl ----

  private final class IncompleteImpl[S <: Sys[S]](val aural: AuralObj.ProcData[S],
      val remaining     : Vec[Lazy],
      val controlProxies: Set[ControlProxyLike],
      val ugens         : Vec[UGen],
      val controlValues : Vec[Float],
      val controlNames  : Vec[(String, Int)],
      val sourceMap     : Map[AnyRef, Any],
      val scanOuts      : Map[String, Int],
      val scanIns       : Map[String, ScanIn],
      val attributeIns  : Set[String],
      val streamIns     : Map[String, List[StreamIn]],
      val missingIns    : Set[MissingIn[S]]
    )
    extends Incomplete[S] {

    def retry()(implicit tx: S#Tx): State[S] = new Impl[S](this, tx).tryBuild()
  }

  private final class CompleteImpl[S <: Sys[S]](val result: UGenGraph,
      val scanOuts      : Map[String, Int],
      val scanIns       : Map[String, ScanIn],
      val attributeIns  : Set[String],
      val streamIns     : Map[String, List[StreamIn]]
    )
    extends Complete[S] {
  }

  private final class Impl[S <: Sys[S]](in: IncompleteImpl[S], val tx: S#Tx)
    extends BasicUGenGraphBuilder with UGenGraphBuilder[S] {
    builder =>

    override def toString = s"proc.UGenGraph.Builder@${hashCode.toHexString}"

    private var remaining       = in.remaining
    private var controlProxies  = in.controlProxies

    private var scanOuts        = in.scanOuts
    private var scanIns         = in.scanIns
    private var missingIns      = Set.empty[MissingIn[S]]
    private var attributeIns    = in.attributeIns
    private var streamIns       = in.streamIns

    // def sensorBus: SControlBus = aural.sensorBus

    def addScanIn(key: String, numChannels: Int): Int = {
      val fixed = numChannels >= 0
      val res   = in.aural.scanInNumChannels(key = key, numChannels = numChannels)(tx)
      scanIns  += key -> ScanIn(numChannels = res, fixed = fixed)
      res
    }

    def addScanOut(key: String, numChannels: Int): Unit =
      scanOuts.get(key).fold {
        scanOuts += key -> numChannels
      } { prevChans =>
        if (numChannels != prevChans) {
          val s1 = s"Cannot write multiple times to the same scan ($key)"
          val s2 = s"using different number of channels ($prevChans, $numChannels)"
          sys.error(s"$s1 $s2")
        }
      }

    def addAttributeIn(key: String): Int = {
      val res       = in.aural.attrNumChannels(key = key)(tx)
      attributeIns += key
      res
    }

    def addStreamIn(key: String, info: StreamIn): (Int, Int) = {
      val numCh = in.aural.attrNumChannels(key = key)(tx)
      val idx   = if (info.isEmpty) {
        if (!streamIns.contains(key)) streamIns += key -> Nil
        0
      } else {
        val oldValue = streamIns.getOrElse(key, Nil)
        streamIns += key -> (info :: oldValue)
        oldValue.size
      }
      (numCh, idx)
    }

    def tryBuild(): State[S] = UGenGraph.use(this) {
      var missingElems  = Vector.empty[Lazy]
      // missingIns        = Set.empty

      //      sourceMap
      //      controlNames
      //      controlValues
      //      ugens
      //      scanOuts
      //      scanIns
      //      attributeIns
      //      streamIns

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
            val savedStreams        = streamIns
            try {
              elem.force(builder)
              someSucceeded = true
            } catch {
              case miss: MissingIn[_] =>
                sourceMap           = savedSourceMap
                controlNames        = savedControlNames
                controlValues       = savedControlValues
                ugens               = savedUGens
                scanOuts            = savedScanOuts
                scanIns             = savedScanIns
                attributeIns        = savedAttrs
                streamIns           = savedStreams
                missingElems      :+= elem
                missingIns         += miss.asInstanceOf[MissingIn[S]] // XXX TODO not cool
            }
          }
        }
        if (g.nonEmpty) {
          remaining        = g.sources
          controlProxies ++= g.controlProxies
        } else {
          remaining        = Vector.empty
        }
      }

      val newState = if (missingElems.isEmpty) {
        val result = build(controlProxies)
        new CompleteImpl[S](result,
          scanOuts = scanOuts, scanIns = scanIns,
          attributeIns = attributeIns, streamIns = streamIns
        )

      } else {
        if (someSucceeded) {
          new IncompleteImpl[S](in.aural,
            remaining = missingElems, controlProxies = controlProxies,
            ugens = ugens, controlValues = controlValues, controlNames = controlNames,
            sourceMap = sourceMap, scanOuts = scanOuts, scanIns = scanIns,
            attributeIns = attributeIns, streamIns = streamIns, missingIns = missingIns
          )
        } else in
      }

      newState
    }
  }

  trait Incomplete[S <: Sys[S]] extends State[S] {
    def retry()(implicit tx: S#Tx): State[S]

    /** Current set of missing scan inputs. This may shrink during incremental build, and will be empty when
      * `tryBuild` returns `true`.
      */
    def missingIns: Set[MissingIn[S]]
  }

  trait Complete[S <: Sys[S]] extends State[S] {
    def result: UGenGraph
  }

  sealed trait State[S <: Sys[S]] {
    /** Current set of used inputs (scan keys to number of channels).
      * This is guaranteed to only grow during incremental building, never shrink.
      */
    def scanIns: Map[String, ScanIn]

    /** Current set of used outputs (scan keys to number of channels).
      * This is guaranteed to only grow during incremental building, never shrink.
      */
    def scanOuts: Map[String, Int]

    /** Returns the attribute keys for scalar controls as seen during expansion of the synth graph. */
    def attributeIns: Set[String]

    /** Returns the attribute keys and settings for streaming buffers as seen during expansion of the synth graph.
      * The same attribute may be streamed multiple times, possibly with different settings. The settings are
      * given as maximum rate factors (re server sample rate) and interpolation settings.
      *
      * It is also possible that info-only UGens (e.g. `BufChannels`) use such an attribute. In that case, the
      * key would be contained in the map, but the value list is empty. All stream users use a named control
      * of two channels (buffer-id and gain factor), appending an incremental integer index to its name.
      */
    def streamIns: Map[String, List[StreamIn]]
  }
}
trait UGenGraphBuilder[S <: Sys[S]] extends UGenGraph.Builder {
  import UGenGraphBuilder._

  /** This method should only be invoked by the `graph.scan.Elem` instances. It requests a scan input, and
    * the method returns the corresponding number of channels, or throws a `MissingIn` exception which
    * is then caught by the main builder body.
    *
    * @param  key           the scan input key
    * @param  numChannels   a given number of channels (if `>= 0`) or `-1` in which case the number of channels
    *                       is determined using the scan.
    */
  def addScanIn(key: String, numChannels: Int): Int

  /** This method should only be invoked by the `graph.attribute.In` instances. It registers a control input. */
  def addAttributeIn(key: String): Int

  /** This method should only be invoked by the `graph.stream.X` instances. It registers a control input
    * for a streaming buffer.
    *
    * @return tuple consisting of `_1` number of channel, and `_2` control name index
    */
  def addStreamIn(key: String, info: StreamIn): (Int, Int)

  /** This method should only be invoked by the `graph.scan.Elem` instances. It declares a scan output along
    * with the number of channels written to it.
    */
  def addScanOut(key: String, numChannels: Int): Unit
}