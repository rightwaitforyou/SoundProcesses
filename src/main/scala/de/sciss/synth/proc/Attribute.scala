package de.sciss
package synth
package proc

import lucre.{event => evt, stm}
import lucre.expr.Expr
import stm.{Disposable, Mutable}
import evt.{Sys => ESys, Event, EventLikeSerializer, EventLike}
import serial.{DataOutput, DataInput, Writable}
import expr.{Ints, Doubles, Booleans, Strings}
import scala.annotation.switch
import language.higherKinds

object Attribute {
  import scala.{Int => _Int, Double => _Double, Boolean => _Boolean}
  import java.lang.{String => _String}
  import proc.{FadeSpec => _FadeSpec}
  // import mellite.{Folder => _Folder}

  final case class Update [S <: ESys[S]](element: Attribute[S], change: Any) // IIdxSeq[Any])

  sealed trait Companion[E[S <: ESys[S]] <: Writable ] {
    protected[Attribute] def readIdentified[S <: ESys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                             (implicit tx: S#Tx): E[S] with evt.Node[S]

    protected def typeID: _Int

    implicit final def serializer[S <: ESys[S]]: serial.Serializer[S#Tx, S#Acc, E[S]] = anySer.asInstanceOf[Serializer[S]]

    private val anySer = new Serializer[InMemory]
    private final class Serializer[S <: ESys[S]] extends serial.Serializer[S#Tx, S#Acc, E[S]] {
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
    protected[Attribute] final val typeID = Ints.typeID

    protected[Attribute] def readIdentified[S <: ESys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                             (implicit tx: S#Tx): Int[S] with evt.Node[S] = {
      val peer = Ints.readExpr(in, access)
      new Impl(targets, peer)
    }

    def apply[S <: ESys[S]](peer: Expr[S, _Int])(implicit tx: S#Tx): Int[S] = {
      new Impl(evt.Targets[S], peer)
    }

    private final class Impl[S <: ESys[S]](val targets: evt.Targets[S], val peer: Expr[S, _Int])
      extends ExprImpl[S, _Int] with Int[S] {
      def prefix = "Int"
      def typeID = Int.typeID

      def mkCopy()(implicit tx: S#Tx): Int[S] = {
        val newPeer = peer match {
          case Expr.Var(vr) => Ints.newVar(vr())
          case _ => peer
        }
        Int(newPeer)
      }
    }
  }
  sealed trait Int[S <: ESys[S]] extends Attribute[S] {
    type Peer = Expr[S, _Int]
    def mkCopy()(implicit tx: S#Tx): Int[S]
  }

  // ----------------- Double -----------------

  object Double extends Companion[Double] {
    protected[Attribute] final val typeID = Doubles.typeID

    protected[Attribute] def readIdentified[S <: ESys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                      (implicit tx: S#Tx): Double[S] with evt.Node[S] = {
      val peer = Doubles.readExpr(in, access)
      new Impl(targets, peer)
    }

    def apply[S <: ESys[S]](peer: Expr[S, _Double])(implicit tx: S#Tx): Double[S] = {
      new Impl(evt.Targets[S], peer)
    }

    private final class Impl[S <: ESys[S]](val targets: evt.Targets[S], val peer: Expr[S, _Double])
      extends ExprImpl[S, _Double] with Double[S] {
      def typeID = Double.typeID
      def prefix = "Double"

      def mkCopy()(implicit tx: S#Tx): Double[S] = {
        val newPeer = peer match {
          case Expr.Var(vr) => Doubles.newVar(vr())
          case _ => peer
        }
        Double(newPeer)
      }
    }
  }
  sealed trait Double[S <: ESys[S]] extends Attribute[S] {
    type Peer = Expr[S, _Double]
    def mkCopy()(implicit tx: S#Tx): Double[S]
  }

  // ----------------- Double -----------------

  object Boolean extends Companion[Boolean] {
    protected[Attribute] final val typeID = Booleans.typeID

    protected[Attribute] def readIdentified[S <: ESys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                      (implicit tx: S#Tx): Boolean[S] with evt.Node[S] = {
      val peer = Booleans.readExpr(in, access)
      new Impl(targets, peer)
    }

    def apply[S <: ESys[S]](peer: Expr[S, _Boolean])(implicit tx: S#Tx): Boolean[S] = {
      new Impl(evt.Targets[S], peer)
    }

    private final class Impl[S <: ESys[S]](val targets: evt.Targets[S], val peer: Expr[S, _Boolean])
      extends ExprImpl[S, _Boolean] with Boolean[S] {
      def typeID = Boolean.typeID
      def prefix = "Boolean"

      def mkCopy()(implicit tx: S#Tx): Boolean[S] = {
        val newPeer = peer match {
          case Expr.Var(vr) => Booleans.newVar(vr())
          case _ => peer
        }
        Boolean(newPeer)
      }
    }
  }
  sealed trait Boolean[S <: ESys[S]] extends Attribute[S] {
    type Peer = Expr[S, _Boolean]
    def mkCopy()(implicit tx: S#Tx): Boolean[S]
  }

  // ----------------- String -----------------

  object String extends Companion[String] {
    protected[Attribute] final val typeID = Strings.typeID

    protected[Attribute] def readIdentified[S <: ESys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                      (implicit tx: S#Tx): String[S] with evt.Node[S] = {
      val peer = Strings.readExpr(in, access)
      new Impl(targets, peer)
    }

    def apply[S <: ESys[S]](peer: Expr[S, _String])(implicit tx: S#Tx): String[S] = {
      new Impl(evt.Targets[S], peer)
    }

    private final class Impl[S <: ESys[S]](val targets: evt.Targets[S], val peer: Expr[S, _String])
      extends ExprImpl[S, _String] with String[S] {
      def typeID = String.typeID
      def prefix = "String"

      def mkCopy()(implicit tx: S#Tx): String[S] = {
        val newPeer = peer match {
          case Expr.Var(vr) => Strings.newVar(vr())
          case _ => peer
        }
        String(newPeer)
      }
    }
  }
  sealed trait String[S <: ESys[S]] extends Attribute[S] {
    type Peer = Expr[S, _String]
    def mkCopy()(implicit tx: S#Tx): String[S]
  }

  // ----------------- Double -----------------

  object FadeSpec extends Companion[FadeSpec] {
    protected[Attribute] final val typeID = _FadeSpec.Elem.typeID

    protected[Attribute] def readIdentified[S <: ESys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                      (implicit tx: S#Tx): FadeSpec[S] with evt.Node[S] = {
      val peer = _FadeSpec.Elem.readExpr(in, access)
      new Impl(targets, peer)
    }

    def apply[S <: ESys[S]](peer: Expr[S, _FadeSpec.Value])(implicit tx: S#Tx): FadeSpec[S] = {
      new Impl(evt.Targets[S], peer)
    }

    private final class Impl[S <: ESys[S]](val targets: evt.Targets[S], val peer: Expr[S, _FadeSpec.Value])
      extends ExprImpl[S, _FadeSpec.Value] with FadeSpec[S] {
      def typeID = FadeSpec.typeID
      def prefix = "FadeSpec"

      def mkCopy()(implicit tx: S#Tx): FadeSpec[S] = {
        val newPeer = peer match {
          case Expr.Var(vr) => _FadeSpec.Elem.newVar(vr())
          case _ => peer
        }
        FadeSpec(newPeer)
      }
    }
  }
  sealed trait FadeSpec[S <: ESys[S]] extends Attribute[S] {
    type Peer = Expr[S, _FadeSpec.Value]
    def mkCopy()(implicit tx: S#Tx): FadeSpec[S]
  }

  // ----------------- Serializer -----------------

  implicit def serializer[S <: ESys[S]]: evt.Serializer[S, Attribute[S]] = anySer.asInstanceOf[Ser[S]]

  private final val anySer = new Ser[InMemory]

  private final class Ser[S <: ESys[S]] extends EventLikeSerializer[S, Attribute[S]] {
    def readConstant(in: DataInput)(implicit tx: S#Tx): Attribute[S] = {
      sys.error("No passive elements known")
    }

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Attribute[S] with evt.Node[S] = {
      val typeID = in.readInt()
      (typeID: @switch) match {
        case Int             .typeID => Int             .readIdentified(in, access, targets)
        case Double          .typeID => Double          .readIdentified(in, access, targets)
        case Boolean         .typeID => Boolean         .readIdentified(in, access, targets)
        case String          .typeID => String          .readIdentified(in, access, targets)
        case FadeSpec        .typeID => FadeSpec        .readIdentified(in, access, targets)
        // case Folder          .typeID => Folder          .readIdentified(in, access, targets)
        // case ProcGroup       .typeID => ProcGroup       .readIdentified(in, access, targets)
        // case AudioGrapheme   .typeID => AudioGrapheme   .readIdentified(in, access, targets)
        // case ArtifactLocation.typeID => ArtifactLocation.readIdentified(in, access, targets)
        case _                       => sys.error(s"Unexpected element type cookie $typeID")
      }
    }
  }

  // ---- impl ----

  private sealed trait ExprImpl[S <: ESys[S], A] extends ActiveImpl[S] {
    self =>
    type Peer <: Expr[S, A]
    final protected def peerEvent = peer.changed
  }

  private sealed trait Impl[S <: ESys[S]]
    extends Attribute[S] with evt.Node[S] {
    self =>

    type Peer <: Writable with Disposable[S#Tx]

    protected def typeID: _Int

    final protected def writeData(out: DataOutput) {
      out.writeInt(typeID)
      peer.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx) {
      peer.dispose()
    }

    protected def prefix: _String

    override def toString() = s"Attribute.${prefix}$id"

    // ---- events ----

    final protected def reader: evt.Reader[S, Attribute[S]] = serializer

    //    final protected def foldUpdate(sum: Option[Update[S]], inc: Update[S]): Option[Update[S]] = sum match {
    //      case Some(prev) => Some(prev.copy(changes = prev.changes ++ inc.changes))
    //      case _          => Some(inc)
    //    }

    //    trait EventImpl
    //      extends evt.impl.EventImpl[S, Any, Attribute[S]] with evt.InvariantEvent[S, Any, Attribute[S]] {
    //
    //      final protected def reader: evt.Reader[S, Attribute[S]] = self.reader
    //      final def node: Attribute[S] with evt.Node[S] = self
    //    }
  }

  private sealed trait ActiveImpl[S <: ESys[S]]
    extends Impl[S] /* with evt.impl.MultiEventImpl[S, Any, Any, Attribute[S]] */ {
    self =>

    protected def peerEvent: evt.EventLike[S, Any, _]

    def select(slot: _Int /*, invariant: _Boolean */): Event[S, Any, Any] = changed

    object changed
      extends evt.impl.EventImpl[S, Update[S], Attribute[S]]
      with evt.InvariantEvent   [S, Update[S], Attribute[S]] {

      final protected def reader: evt.Reader[S, Attribute[S]] = self.reader
      final def node: Attribute[S] with evt.Node[S] = self

      final val slot = 0

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Update[S]] = {
        pull(peerEvent).map(ch => Update(self, ch))
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
sealed trait Attribute[S <: ESys[S]] extends Mutable[S#ID, S#Tx] {
  attr =>

  import Attribute.Update

  type Peer

  /** The actual object wrapped by the element. */
  def peer: Peer

  /** An event for tracking element changes, which can be renaming
    * the element or forwarding changes from the underlying entity.
    */
  def changed: EventLike[S, Update[S], Attribute[S]]

  def mkCopy()(implicit tx: S#Tx): Attribute[S] // { type Peer = attr.Peer }
}