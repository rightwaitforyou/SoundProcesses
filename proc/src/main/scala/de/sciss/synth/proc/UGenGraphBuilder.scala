package de.sciss.synth.proc

import de.sciss.lucre.synth.Sys
import de.sciss.synth.UGenGraph

import scala.util.control.ControlThrowable
import impl.{UGenGraphBuilderImpl => Impl}

object UGenGraphBuilder {
  // def outsideOfContext(): Nothing = sys.error("Expansion out of context")

  def get: UGenGraphBuilder[_] = UGenGraph.builder match {
    case b: UGenGraphBuilder[_] => b
    case _ => throw new IllegalStateException("Expansion out of context")
  }

  /** An exception thrown when during incremental build an input is required for which the underlying source
    * cannot yet be determined.
    *
    * This can be a case class because it is used only within the same transaction,
    * and thereby the `timed` argument does not become stale.
    */
  final case class MissingIn(input: Input) extends ControlThrowable

  /** '''Note''': The resulting object is mutable, therefore must not be shared across threads and also must be
    * created and consumed within the same transaction. That is to say, to be transactionally safe, it may only
    * be stored in a `TxnLocal`, but not a full STM ref.
    */
  def apply[S <: Sys[S]](context: Context[S], proc: Obj.T[S, Proc.Elem])
                        (implicit tx: S#Tx): State[S] = Impl(context, proc)

  def init[S <: Sys[S]](proc: Obj.T[S, Proc.Elem])(implicit tx: S#Tx): Incomplete[S] = Impl.init(proc)

  case class ScanIn(numChannels: Int, fixed: Boolean)

  trait Context[S <: Sys[S]] {
    // def requestInput(in: Input)(implicit tx: S#Tx): in.Value
    def requestInput[Res](req: UGenGraphBuilder.Input { type Value = Res }, state: Incomplete[S])(implicit tx: S#Tx): Res
  }

  //  object StreamIn {
  //    val empty = StreamIn(0.0, 0)
  //  }
  //  case class StreamIn(maxSpeed: Double, interp: Int) {
  //    /** Empty indicates that the stream is solely used for information
  //      * purposes such as `BufChannels`.
  //      */
  //    def isEmpty: Boolean = interp == 0
  //
  //    /** Native indicates that the stream will be transported by the UGen
  //      * itself, i.e. via `DiskIn` or `VDiskIn`.
  //      */
  //    def isNative: Boolean = interp == -1
  //  }

  sealed trait State[S <: Sys[S]] {
    def acceptedInputs: Map[Key, Input#Value]
    def rejectedInputs: Set[Key]

    //    /** Current set of used inputs (scan keys to number of channels).
    //      * This is guaranteed to only grow during incremental building, never shrink.
    //      */
    //    def scanIns: Map[String, ScanIn]

    /** Current set of used outputs (scan keys to number of channels).
      * This is guaranteed to only grow during incremental building, never shrink.
      */
    def scanOuts: Map[String, Int]

    //    /** Returns the attribute keys for scalar controls as seen during expansion of the synth graph. */
    //    def attributeIns: Set[String]

    //    /** Returns the attribute keys and settings for streaming buffers as seen during expansion of the synth graph.
    //      * The same attribute may be streamed multiple times, possibly with different settings. The settings are
    //      * given as maximum rate factors (re server sample rate) and interpolation settings.
    //      *
    //      * It is also possible that info-only UGens (e.g. `BufChannels`) use such an attribute. In that case, the
    //      * key would be contained in the map, but the value list is empty. All stream users use a named control
    //      * of two channels (buffer-id and gain factor), appending an incremental integer index to its name.
    //      */
    //    def streamIns: Map[String, List[StreamIn]]

    def isComplete: Boolean

    //    /** Current set of missing scan inputs. This may shrink during incremental build, and will be empty when
    //      * the build is complete
    //      */
    //    def missingIns: Set[String]
  }

  trait Incomplete[S <: Sys[S]] extends State[S] {
    def retry(context: Context[S])(implicit tx: S#Tx): State[S]

    final def isComplete = false
  }

  trait Complete[S <: Sys[S]] extends State[S] {
    def result: UGenGraph

    final def isComplete = true
    // final def missingIns = Set.empty[String]

    final def rejectedInputs = Set.empty[UGenGraphBuilder.Key]
  }

  // --------------------------------------------

  trait Key
  case class AttributeKey(name: String) extends Key
  case class ScanKey     (name: String) extends Key

  case class NumChannels(value: Int) extends Input.Value

  object Input {
    trait Value

    final case class Scan(name: String) extends Input {
      type Key    = ScanKey
      type Value  = NumChannels

      def key = ScanKey(name)
    }

    object Stream {
      def EmptySpec = Spec(0.0, 0)

      final case class Spec(maxSpeed: Double, interp: Int) {
        /** Empty indicates that the stream is solely used for information
          * purposes such as `BufChannels`.
          */
        def isEmpty: Boolean = interp == 0

        /** Native indicates that the stream will be transported by the UGen
          * itself, i.e. via `DiskIn` or `VDiskIn`.
          */
        def isNative: Boolean = interp == -1
      }
      final case class Value(numChannels: Int, specs: List[Spec]) extends Input.Value
    }
    final case class Stream(name: String, spec: Stream.Spec) extends Input {
      type Key    = AttributeKey
      type Value  = Stream.Value

      def key = AttributeKey(name)
    }

    object Attribute {

    }
    final case class Attribute(name: String) extends Input {
      type Key    = AttributeKey
      type Value  = NumChannels

      def key = AttributeKey(name)
    }
  }
  trait Input {
    type Key   <: UGenGraphBuilder.Key
    type Value <: Input.Value

    def key: Key
  }
}
trait UGenGraphBuilder[S <: Sys[S]] extends UGenGraph.Builder {
  import de.sciss.synth.proc.UGenGraphBuilder._

  def requestInput(input: Input): input.Value

  //  /** This method should only be invoked by the `graph.scan.Elem` instances. It requests a scan input, and
  //    * the method returns the corresponding number of channels, or throws a `MissingIn` exception which
  //    * is then caught by the main builder body.
  //    *
  //    * @param  key           the scan input key
  //    * @param  numChannels   a given number of channels (if `>= 0`) or `-1` in which case the number of channels
  //    *                       is determined using the scan.
  //    */
  //  def addScanIn(key: String, numChannels: Int): Int
  //
  //  /** This method should only be invoked by the `graph.attribute.In` instances. It registers a control input. */
  //  def addAttributeIn(key: String): Int

  //  /** This method should only be invoked by the `graph.stream.X` instances. It registers a control input
  //    * for a streaming buffer.
  //    *
  //    * @return tuple consisting of `_1` number of channel, and `_2` control name index
  //    */
  //  def addStreamIn(key: String, info: StreamIn): (Int, Int)

  /** This method should only be invoked by the `graph.scan.Elem` instances. It declares a scan output along
    * with the number of channels written to it.
    */
  def addScanOut(key: String, numChannels: Int): Unit
}