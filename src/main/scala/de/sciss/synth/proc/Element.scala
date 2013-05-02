package de.sciss
package synth
package proc

import lucre.{event => evt, stm}
import lucre.expr.Expr
import stm.{Disposable, Mutable}
import evt.{Event, EventLikeSerializer, EventLike}
import serial.{DataOutput, DataInput, Writable}
import expr.{Ints, Doubles, Strings}
import scala.annotation.switch
import language.higherKinds

object Element {
  import scala.{Int => _Int, Double => _Double}
  import java.lang.{String => _String}
  import proc.{FadeSpec => _FadeSpec}
  // import mellite.{Folder => _Folder}

  final case class Update [S <: Sys[S]](element: Element[S], change: Any) // IIdxSeq[Any])

  sealed trait Companion[E[S <: Sys[S]] <: Writable ] {
    protected[Element] def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                             (implicit tx: S#Tx): E[S] with evt.Node[S]

    protected def typeID: _Int

    implicit final def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, E[S]] = anySer.asInstanceOf[Serializer[S]]

    private val anySer = new Serializer[InMemory]
    private final class Serializer[S <: Sys[S]] extends serial.Serializer[S#Tx, S#Acc, E[S]] {
      def write(v: E[S], out: DataOutput) {
        v.write(out)
      }

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): E[S] = {
        val targets = evt.Targets.read[S](in, access)
        val cookie  = in.readInt()
        require(cookie == typeID, s"Cookie $cookie does not match expected value $typeID")
        readIdentified(in, access, targets)
      }
    }
  }

  // ----------------- Int -----------------

  object Int extends Companion[Int] {
    protected[Element] final val typeID = Ints.typeID

    protected[Element] def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                             (implicit tx: S#Tx): Int[S] with evt.Node[S] = {
      val peer = Ints.readExpr(in, access)
      new Impl(targets, peer)
    }

    def apply[S <: Sys[S]](peer: Expr[S, _Int])(implicit tx: S#Tx): Int[S] = {
      new Impl(evt.Targets[S], peer)
    }

    private final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val peer: Expr[S, _Int])
      extends ExprImpl[S, _Int] with Int[S] {
      def prefix = "Int"
      def typeID = Int.typeID
    }
  }
  sealed trait Int[S <: Sys[S]] extends Element[S] { type A = Expr[S, _Int] }

  // ----------------- Double -----------------

  object Double extends Companion[Double] {
    protected[Element] final val typeID = Doubles.typeID

    protected[Element] def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                      (implicit tx: S#Tx): Double[S] with evt.Node[S] = {
      val peer = Doubles.readExpr(in, access)
      new Impl(targets, peer)
    }

    def apply[S <: Sys[S]](peer: Expr[S, _Double])(implicit tx: S#Tx): Double[S] = {
      new Impl(evt.Targets[S], peer)
    }

    private final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val peer: Expr[S, _Double])
      extends ExprImpl[S, _Double] with Double[S] {
      def typeID = Double.typeID
      def prefix = "Double"
    }
  }
  sealed trait Double[S <: Sys[S]] extends Element[S] { type A = Expr[S, _Double] }

  // ----------------- String -----------------

  object String extends Companion[String] {
    protected[Element] final val typeID = Strings.typeID

    protected[Element] def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                      (implicit tx: S#Tx): String[S] with evt.Node[S] = {
      val peer = Strings.readExpr(in, access)
      new Impl(targets, peer)
    }

    def apply[S <: Sys[S]](peer: Expr[S, _String])(implicit tx: S#Tx): String[S] = {
      new Impl(evt.Targets[S], peer)
    }

    private final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val peer: Expr[S, _String])
      extends ExprImpl[S, _String] with String[S] {
      def typeID = String.typeID
      def prefix = "String"
    }
  }
  sealed trait String[S <: Sys[S]] extends Element[S] { type A = Expr[S, _String] }

  // ----------------- Double -----------------

  object FadeSpec extends Companion[FadeSpec] {
    protected[Element] final val typeID = _FadeSpec.Elem.typeID

    protected[Element] def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                      (implicit tx: S#Tx): FadeSpec[S] with evt.Node[S] = {
      val peer = _FadeSpec.Elem.readExpr(in, access)
      new Impl(targets, peer)
    }

    def apply[S <: Sys[S]](peer: Expr[S, _FadeSpec.Value])(implicit tx: S#Tx): FadeSpec[S] = {
      new Impl(evt.Targets[S], peer)
    }

    private final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val peer: Expr[S, _FadeSpec.Value])
      extends ExprImpl[S, _FadeSpec.Value] with FadeSpec[S] {
      def typeID = FadeSpec.typeID
      def prefix = "FadeSpec"
    }
  }
  sealed trait FadeSpec[S <: Sys[S]] extends Element[S] { type A = Expr[S, _FadeSpec.Value] }

  // ----------------- Serializer -----------------

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Element[S]] = anySer.asInstanceOf[Ser[S]]

  private final val anySer = new Ser[InMemory]

  private final class Ser[S <: Sys[S]] extends EventLikeSerializer[S, Element[S]] {
    def readConstant(in: DataInput)(implicit tx: S#Tx): Element[S] = {
      sys.error("No passive elements known")
    }

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Element[S] with evt.Node[S] = {
      val typeID = in.readInt()
      (typeID: @switch) match {
        case Int             .typeID => Int             .readIdentified(in, access, targets)
        case Double          .typeID => Double          .readIdentified(in, access, targets)
        case String          .typeID => String          .readIdentified(in, access, targets)
        // case Folder          .typeID => Folder          .readIdentified(in, access, targets)
        // case ProcGroup       .typeID => ProcGroup       .readIdentified(in, access, targets)
        // case AudioGrapheme   .typeID => AudioGrapheme   .readIdentified(in, access, targets)
        // case ArtifactLocation.typeID => ArtifactLocation.readIdentified(in, access, targets)
        case _                       => sys.error(s"Unexpected element type cookie $typeID")
      }
    }
  }

  // ---- impl ----

  private sealed trait ExprImpl[S <: Sys[S], A1] extends ActiveImpl[S] {
    self =>
    type A <: Expr[S, A1]
    final protected def peerEvent = peer.changed
  }

  private sealed trait Impl[S <: Sys[S]]
    extends Element[S] with evt.Node[S] {
    self =>

    type A <: Writable with Disposable[S#Tx]

    protected def typeID: _Int

    final protected def writeData(out: DataOutput) {
      out.writeInt(typeID)
      peer.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx) {
      peer.dispose()
    }

    protected def prefix: _String

    override def toString() = s"Element.${prefix}$id"

    // ---- events ----

    final protected def reader: evt.Reader[S, Element[S]] = serializer

    //    final protected def foldUpdate(sum: Option[Update[S]], inc: Update[S]): Option[Update[S]] = sum match {
    //      case Some(prev) => Some(prev.copy(changes = prev.changes ++ inc.changes))
    //      case _          => Some(inc)
    //    }

    //    trait EventImpl
    //      extends evt.impl.EventImpl[S, Any, Element[S]] with evt.InvariantEvent[S, Any, Element[S]] {
    //
    //      final protected def reader: evt.Reader[S, Element[S]] = self.reader
    //      final def node: Element[S] with evt.Node[S] = self
    //    }
  }

  private sealed trait ActiveImpl[S <: Sys[S]]
    extends Impl[S] /* with evt.impl.MultiEventImpl[S, Any, Any, Element[S]] */ {
    self =>

    protected def peerEvent: evt.EventLike[S, Any, _]

    def select(slot: _Int, invariant: Boolean): Event[S, Any, Any] = changed

    object changed
      extends evt.impl.EventImpl[S, Update[S], Element[S]]
      with evt.InvariantEvent   [S, Update[S], Element[S]] {

      final protected def reader: evt.Reader[S, Element[S]] = self.reader
      final def node: Element[S] with evt.Node[S] = self

      final val slot = 0

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Update[S]] = {
        peerEvent.pullUpdate(pull).map(ch => Update(self, ch))
      }

      def connect()(implicit tx: S#Tx) {
        peerEvent ---> this
      }

      def disconnect()(implicit tx: S#Tx) {
        peerEvent -/-> this
      }
    }
  }
}
sealed trait Element[S <: Sys[S]] extends Mutable[S#ID, S#Tx] {
  import Element.Update

  type A

  /** The actual object wrapped by the element. */
  def peer: A

  /** An event for tracking element changes, which can be renaming
    * the element or forwarding changes from the underlying entity.
    */
  def changed: EventLike[S, Update[S], Element[S]]
}