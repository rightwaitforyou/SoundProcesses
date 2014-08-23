package de.sciss.synth.proc

import de.sciss.lucre.synth.Sys
import de.sciss.synth.UGenGraph

import scala.util.control.ControlThrowable
import impl.{UGenGraphBuilderImpl => Impl}

object UGenGraphBuilder {
  def get: UGenGraphBuilder = UGenGraph.builder match {
    case b: UGenGraphBuilder => b
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
    def requestInput[Res](req: UGenGraphBuilder.Input { type Value = Res }, state: Incomplete[S])(implicit tx: S#Tx): Res
  }

  sealed trait State[S <: Sys[S]] {
    def acceptedInputs: Map[Key, Input#Value]
    def rejectedInputs: Set[Key]

    /** Current set of used outputs (scan keys to number of channels).
      * This is guaranteed to only grow during incremental building, never shrink.
      */
    def scanOuts: Map[String, Int]

    def isComplete: Boolean
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

  /** A pure marker trait to rule out some type errors. */
  trait Key
  case class AttributeKey(name: String) extends Key
  case class ScanKey     (name: String) extends Key

  case class NumChannels(value: Int) extends UGenGraphBuilder.Value

  /** A pure marker trait to rule out some type errors. */
  trait Value
  case object Unit extends Value
  type Unit = Unit.type

  object Input {
    final case class Scan(name: String) extends Input {
      type Key    = ScanKey
      type Value  = NumChannels

      def key = ScanKey(name)

      override def productPrefix = "Input.Scan"
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

        override def productPrefix = "Input.Stream.Spec"
      }
      final case class Value(numChannels: Int, specs: List[Spec]) extends UGenGraphBuilder.Value {
        override def productPrefix = "Input.Stream.Value"
      }
    }
    final case class Stream(name: String, spec: Stream.Spec) extends Input {
      type Key    = AttributeKey
      type Value  = Stream.Value

      def key = AttributeKey(name)

      override def productPrefix = "Input.Stream"
    }

    /** Specifies access to a scalar attribute.
      *
      * @param name         name (key) of the attribute
      * @param numChannels  the required number of channels or `-1` if no specific requirement
      */
    final case class Attribute(name: String, numChannels: Int) extends Input {
      type Key    = AttributeKey
      type Value  = NumChannels

      def key = AttributeKey(name)

      override def productPrefix = "Input.Attribute"
    }
  }
  trait Input {
    type Key   <: UGenGraphBuilder.Key
    type Value <: UGenGraphBuilder.Value

    def key: Key
  }
}
trait UGenGraphBuilder extends UGenGraph.Builder {
  import UGenGraphBuilder._

  def requestInput(input: Input): input.Value

  /** This method should only be invoked by the `graph.scan.Elem` instances. It declares a scan output along
    * with the number of channels written to it.
    */
  def addScanOut(key: String, numChannels: Int): scala.Unit
}