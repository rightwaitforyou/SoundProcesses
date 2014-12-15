package de.sciss.synth.proc
package impl

import de.sciss.lucre.{event => evt}
import de.sciss.lucre.event.{Event, EventLike, Sys}
import de.sciss.lucre.expr.{Expr => _Expr}
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.InMemory
import de.sciss.synth.proc.Elem.Update
import de.sciss.{lucre, serial}
import de.sciss.serial.{DataOutput, Writable, DataInput}

import scala.language.higherKinds

// NOTE: has to be separate from ElemImpl
// to fuck shitty scalac initializer bugs
trait ElemCompanionImpl[E[S <: Sys[S]] <: Elem[S]] extends Elem.Extension {
  implicit final def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, E[S]] =
    anySer.asInstanceOf[Serializer[S]]

  private val anySer = new Serializer[InMemory]

  override def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                          (implicit tx: S#Tx): E[S] with evt.Node[S]

  override def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): E[S]

  private final class Serializer[S <: Sys[S]] extends evt.EventLikeSerializer[S, E[S]] {
    def readConstant(in: DataInput)(implicit tx: S#Tx): E[S] = {
      readCookie(in)
      readIdentifiedConstant(in)
    }

    private def readCookie(in: DataInput): Unit = {
      val cookie  = in.readInt()
      if (cookie != typeID) sys.error(s"Cookie $cookie does not match expected value $typeID")
    }

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): E[S] with evt.Node[S] = {
      readCookie(in)
      readIdentified(in, access, targets)
    }
  }
}

trait ExprElemCompanionImpl[E[S <: Sys[S]] <: Elem[S], A] extends ElemCompanionImpl[E] {
  protected def tpe: lucre.expr.ExprType1[A]

  protected def newActive[S <: Sys[S]](targets: evt.Targets[S], peer: _Expr[S, A])
                                      (implicit tx: S#Tx): E[S] with evt.Node[S]

  protected def newConst[S <: Sys[S]](peer: _Expr.Const[S, A])(implicit tx: S#Tx): E[S]

  def typeID = tpe.typeID

  def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                 (implicit tx: S#Tx): E[S] with evt.Node[S] = {
    val peer = tpe.read(in, access)
    newActive(targets, peer)
  }

  def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): E[S] = {
    val peer = tpe.readConst[S](in)
    newConst(peer)
  }

  def apply[S <: Sys[S]](peer: _Expr[S, A])(implicit tx: S#Tx): E[S] = peer match {
    case c: _Expr.Const[S, A] => newConst(c)
    case _                    => newActive[S](evt.Targets[S], peer)
  }

  def copyExpr[S <: Sys[S]](in: _Expr[S, A])(implicit ts: S#Tx): _Expr[S, A] =
    in match {
      case _Expr.Var(vr) => tpe.newVar(vr())
      case _ => in
    }
}

trait BasicElemImpl[S <: Sys[S]]
  extends Elem[S] {
  self =>

  type Peer <: Writable with Disposable[S#Tx]

  final protected def writeData(out: DataOutput): Unit = {
    out.writeInt(typeID)
    peer.write(out)
  }

  final protected def disposeData()(implicit tx: S#Tx): Unit = peer.dispose()

  protected def prefix: String

  // ---- events ----

  final protected def reader: evt.Reader[S, Elem[S]] = Elem.serializer
}

trait PassiveElemImpl[S <: Sys[S]]
  extends BasicElemImpl[S] with evt.impl.Constant {

  final def mkCopy()(implicit tx: S#Tx): this.type = this

  final def changed: EventLike[S, Update[S, PeerUpdate]] = evt.Dummy[S, Update[S, PeerUpdate]]

  override def toString = s"Elem.$prefix($peer)"

  final def dispose()(implicit tx: S#Tx): Unit = disposeData()
}

trait ActiveElemImpl[S <: Sys[S]]
  extends BasicElemImpl[S] with evt.Node[S] {
  self =>

  type Peer <: evt.Publisher[S, PeerUpdate] with Writable with Disposable[S#Tx]
  // private def peerEvent = peer.changed

  // protected def peerEvent: evt.EventLike[S, Any]

  override def toString() = s"Elem.$prefix$id"

  def select(slot: Int): Event[S, Any, Any] = changed

  object changed
    extends evt.impl.EventImpl[S, Update[S, PeerUpdate], Elem[S]]
    with evt.InvariantEvent   [S, Update[S, PeerUpdate], Elem[S]] {

    final protected def reader: evt.Reader[S, Elem[S]] = self.reader
    final def node: Elem[S] with evt.Node[S] = self

    final val slot = 0

    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Update[S, PeerUpdate]] = {
      pull(peer.changed).map(ch => Update(self, ch))
    }

    def connect   ()(implicit tx: S#Tx): Unit = peer.changed ---> this
    def disconnect()(implicit tx: S#Tx): Unit = peer.changed -/-> this
  }
}
