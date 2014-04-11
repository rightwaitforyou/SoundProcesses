/*
 *  AttrImpl.scala
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

package de.sciss
package synth
package proc
package impl

import lucre.{event => evt, expr, stm}
import evt.Event
import serial.{DataInput, DataOutput, Writable}
import stm.Disposable
import expr.{Expr => _Expr}
import de.sciss.lucre.synth.InMemory
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.higherKinds

object AttrImpl {
  import Attr.Update
  import scala.{Int => _Int, Double => _Double, Boolean => _Boolean, Long => _Long}
  import java.lang.{String => _String}
  import proc.{FadeSpec => _FadeSpec}
  import lucre.synth.expr.{DoubleVec => _DoubleVec}

  // ---- Int ----

  object Int extends Companion[Attr.Int] {
    final val typeID = lucre.expr.Int.typeID

    def readIdentified[S <: evt.Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): Attr.Int[S] with evt.Node[S] = {
      val peer = lucre.expr.Int.read(in, access)
      new IntImpl(targets, peer)
    }

    def apply[S <: evt.Sys[S]](peer: _Expr[S, _Int])(implicit tx: S#Tx): Attr.Int[S] =
      new IntImpl(evt.Targets[S], peer)
  }

  final class IntImpl[S <: evt.Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _Int])
    extends Expr[S, _Int] with Attr.Int[S] {

    def prefix = "Int"
    def typeID = Int.typeID

    def mkCopy()(implicit tx: S#Tx): Attr.Int[S] = {
      val newPeer = peer match {
        case _Expr.Var(vr) => lucre.expr.Int.newVar(vr())
        case _ => peer
      }
      Int(newPeer)
    }
  }

  // ---- Long ----

  object Long extends Companion[Attr.Long] {
    final val typeID = lucre.expr.Long.typeID

    def readIdentified[S <: evt.Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): Attr.Long[S] with evt.Node[S] = {
      val peer = lucre.expr.Long.read(in, access)
      new LongImpl(targets, peer)
    }

    def apply[S <: evt.Sys[S]](peer: _Expr[S, _Long])(implicit tx: S#Tx): Attr.Long[S] =
      new LongImpl(evt.Targets[S], peer)
  }

  final class LongImpl[S <: evt.Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _Long])
    extends Expr[S, _Long] with Attr.Long[S] {

    def prefix = "Long"
    def typeID = Long.typeID

    def mkCopy()(implicit tx: S#Tx): Attr.Long[S] = {
      val newPeer = peer match {
        case _Expr.Var(vr) => lucre.expr.Long.newVar(vr())
        case _ => peer
      }
      Long(newPeer)
    }
  }

  // ---- Double ----

  object Double extends Companion[Attr.Double] {
    final val typeID = lucre.expr.Double.typeID

    def readIdentified[S <: evt.Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): Attr.Double[S] with evt.Node[S] = {
      val peer = lucre.expr.Double.read(in, access)
      new DoubleImpl(targets, peer)
    }

    def apply[S <: evt.Sys[S]](peer: _Expr[S, _Double])(implicit tx: S#Tx): Attr.Double[S] =
      new DoubleImpl(evt.Targets[S], peer)
  }

  final class DoubleImpl[S <: evt.Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _Double])
    extends Expr[S, _Double] with Attr.Double[S] {

    def typeID = Double.typeID
    def prefix = "Double"

    def mkCopy()(implicit tx: S#Tx): Attr.Double[S] = {
      val newPeer = peer match {
        case _Expr.Var(vr) => lucre.expr.Double.newVar(vr())
        case _ => peer
      }
      Double(newPeer)
    }
  }

  // ---- Boolean ----

  object Boolean extends Companion[Attr.Boolean] {
    final val typeID = lucre.expr.Boolean.typeID

    def readIdentified[S <: evt.Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): Attr.Boolean[S] with evt.Node[S] = {
      val peer = lucre.expr.Boolean.read(in, access)
      new BooleanImpl(targets, peer)
    }

    def apply[S <: evt.Sys[S]](peer: _Expr[S, _Boolean])(implicit tx: S#Tx): Attr.Boolean[S] =
      new BooleanImpl(evt.Targets[S], peer)
  }

  final class BooleanImpl[S <: evt.Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _Boolean])
    extends Expr[S, _Boolean] with Attr.Boolean[S] {

    def typeID = Boolean.typeID
    def prefix = "Boolean"

    def mkCopy()(implicit tx: S#Tx): Attr.Boolean[S] = {
      val newPeer = peer match {
        case _Expr.Var(vr) => lucre.expr.Boolean.newVar(vr())
        case _ => peer
      }
      Boolean(newPeer)
    }
  }

  // ---- String ----

  object String extends Companion[Attr.String] {
    val typeID = lucre.expr.String.typeID

    def readIdentified[S <: evt.Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): Attr.String[S] with evt.Node[S] = {
      val peer = lucre.expr.String.read(in, access)
      new StringImpl(targets, peer)
    }

    def apply[S <: evt.Sys[S]](peer: _Expr[S, _String])(implicit tx: S#Tx): Attr.String[S] =
      new StringImpl(evt.Targets[S], peer)
  }

  final class StringImpl[S <: evt.Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _String])
    extends Expr[S, _String] with Attr.String[S] {

    def typeID = String.typeID
    def prefix = "String"

    def mkCopy()(implicit tx: S#Tx): Attr.String[S] = {
      val newPeer = peer match {
        case _Expr.Var(vr) => lucre.expr.String.newVar(vr())
        case _ => peer
      }
      String(newPeer)
    }
  }

  // ---- FadeSpec ----

  object FadeSpec extends Companion[Attr.FadeSpec] {
    final val typeID = _FadeSpec.Elem.typeID

    def readIdentified[S <: evt.Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): Attr.FadeSpec[S] with evt.Node[S] = {
      val peer = _FadeSpec.Elem.read(in, access)
      new FadeSpecImpl(targets, peer)
    }

    def apply[S <: evt.Sys[S]](peer: _Expr[S, _FadeSpec.Value])(implicit tx: S#Tx): Attr.FadeSpec[S] =
      new FadeSpecImpl(evt.Targets[S], peer)
  }

  final class FadeSpecImpl[S <: evt.Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _FadeSpec.Value])
    extends Expr[S, _FadeSpec.Value] with Attr.FadeSpec[S] {

    def typeID = FadeSpec.typeID
    def prefix = "FadeSpec"

    def mkCopy()(implicit tx: S#Tx): Attr.FadeSpec[S] = {
      val newPeer = peer match {
        case _Expr.Var(vr) => _FadeSpec.Elem.newVar(vr())
        case _ => peer
      }
      FadeSpec(newPeer)
    }
  }

  // ---- DoubleVec ----

  object DoubleVec extends Companion[Attr.DoubleVec] {
    val typeID = _DoubleVec.typeID

    def readIdentified[S <: evt.Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): Attr.DoubleVec[S] with evt.Node[S] = {
      val peer = _DoubleVec.read(in, access)
      new DoubleVecImpl(targets, peer)
    }

    def apply[S <: evt.Sys[S]](peer: _Expr[S, Vec[_Double]])(implicit tx: S#Tx): Attr.DoubleVec[S] =
      new DoubleVecImpl(evt.Targets[S], peer)
  }

  final class DoubleVecImpl[S <: evt.Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, Vec[_Double]])
    extends Expr[S, Vec[_Double]] with Attr.DoubleVec[S] {

    def typeID = DoubleVec.typeID
    def prefix = "DoubleVec"

    def mkCopy()(implicit tx: S#Tx): Attr.DoubleVec[S] = {
      val newPeer = peer match {
        case _Expr.Var(vr) => _DoubleVec.newVar(vr())
        case _ => peer
      }
      DoubleVec(newPeer)
    }
  }

  // ---- DoubleVec ----

  object AudioGrapheme extends Companion[Attr.AudioGrapheme] {
    val typeID = Grapheme.Elem.Audio.typeID

    def readIdentified[S <: evt.Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): Attr.AudioGrapheme[S] with evt.Node[S] = {
      // val peer = Grapheme.Elem.Audio.readExpr(in, access)
      val peer = Grapheme.Elem.Audio.read(in, access) match {
        case a: Grapheme.Elem.Audio[S] => a
        case other => sys.error(s"Expected a Grapheme.Elem.Audio, but found $other")  // XXX TODO
      }
      new AudioGraphemeImpl(targets, peer)
    }

    def apply[S <: evt.Sys[S]](peer: Grapheme.Elem.Audio[S])(implicit tx: S#Tx): Attr.AudioGrapheme[S] =
      new AudioGraphemeImpl(evt.Targets[S], peer)
  }

  final class AudioGraphemeImpl[S <: evt.Sys[S]](val targets: evt.Targets[S],
                                                 val peer: Grapheme.Elem.Audio[S])
    extends Expr[S, Grapheme.Value.Audio] with Attr.AudioGrapheme[S] {

    def typeID = AudioGrapheme.typeID
    def prefix = "AudioGrapheme"

    def mkCopy()(implicit tx: S#Tx): Attr.AudioGrapheme[S] = {
      val newPeer = peer
      //      match {
      //        case _Expr.Var(vr) => _DoubleVec.newVar(vr())
      //        case _ => peer
      //      }
      AudioGrapheme(newPeer)
    }
  }

  // ---------- Impl ----------

  trait Companion[E[S <: evt.Sys[S]] <: Writable ] {
    protected[proc] def readIdentified[S <: evt.Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                       (implicit tx: S#Tx): E[S] with evt.Node[S]

    protected def typeID: Int

    implicit final def serializer[S <: evt.Sys[S]]: serial.Serializer[S#Tx, S#Acc, E[S]] = anySer.asInstanceOf[Serializer[S]]

    private val anySer = new Serializer[InMemory]
    private final class Serializer[S <: evt.Sys[S]] extends serial.Serializer[S#Tx, S#Acc, E[S]] {
      def write(v: E[S], out: DataOutput): Unit = v.write(out)

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): E[S] with evt.Node[S] = {
        val targets = evt.Targets.read[S](in, access)
        val cookie  = in.readInt()
        require(cookie == typeID, s"Cookie $cookie does not match expected value $typeID")
        readIdentified(in, access, targets)
      }
    }
  }

  trait Basic[S <: evt.Sys[S]]
    extends Attr[S] with evt.Node[S] {
    self =>

    type Peer <: Writable with Disposable[S#Tx]

    protected def typeID: Int

    final protected def writeData(out: DataOutput): Unit = {
      out.writeInt(typeID)
      peer.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = peer.dispose()

    protected def prefix: String

    override def toString() = s"Attr.${prefix}$id"

    // ---- events ----

    final protected def reader: evt.Reader[S, Attr[S]] = serializer

    //    final protected def foldUpdate(sum: Option[Update[S]], inc: Update[S]): Option[Update[S]] = sum match {
    //      case Some(prev) => Some(prev.copy(changes = prev.changes ++ inc.changes))
    //      case _          => Some(inc)
    //    }

    //    trait EventImpl
    //      extends evt.impl.EventImpl[S, Any, Attr[S]] with evt.InvariantEvent[S, Any, Attr[S]] {
    //
    //      final protected def reader: evt.Reader[S, Attr[S]] = self.reader
    //      final def node: Attr[S] with evt.Node[S] = self
    //    }
  }

  trait Active[S <: evt.Sys[S]]
    extends Basic[S] {
    self =>

    protected def peerEvent: evt.EventLike[S, Any]

    def select(slot: Int): Event[S, Any, Any] = changed

    object changed
      extends evt.impl.EventImpl[S, Update[S], Attr[S]]
      with evt.InvariantEvent   [S, Update[S], Attr[S]] {

      final protected def reader: evt.Reader[S, Attr[S]] = self.reader
      final def node: Attr[S] with evt.Node[S] = self

      final val slot = 0

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Update[S]] = {
        pull(peerEvent).map(ch => Update(self, ch))
      }

      def connect   ()(implicit tx: S#Tx): Unit = peerEvent ---> this
      def disconnect()(implicit tx: S#Tx): Unit = peerEvent -/-> this
    }
  }

  trait Expr[S <: evt.Sys[S], A] extends Active[S] {
    self =>
    type Peer <: _Expr[S, A]
    final protected def peerEvent = peer.changed
  }

  // ----------------- Serializer -----------------

  implicit def serializer[S <: evt.Sys[S]]: evt.Serializer[S, Attr[S]] = anySer.asInstanceOf[Ser[S]]

  private final val anySer = new Ser[InMemory]

  private final class Ser[S <: evt.Sys[S]] extends evt.EventLikeSerializer[S, Attr[S]] {
    def readConstant(in: DataInput)(implicit tx: S#Tx): Attr[S] =
      sys.error("No passive elements known")

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Attr[S] with evt.Node[S] = {
      val typeID = in.readInt()
      typeID /* : @switch */ match {
        case Int             .typeID => Int             .readIdentified(in, access, targets)
        case Long            .typeID => Long            .readIdentified(in, access, targets)
        case Double          .typeID => Double          .readIdentified(in, access, targets)
        case Boolean         .typeID => Boolean         .readIdentified(in, access, targets)
        case String          .typeID => String          .readIdentified(in, access, targets)
        case FadeSpec        .typeID => FadeSpec        .readIdentified(in, access, targets)
        case DoubleVec       .typeID => DoubleVec       .readIdentified(in, access, targets)
        case AudioGrapheme   .typeID => AudioGrapheme   .readIdentified(in, access, targets)
        // case Folder          .typeID => Folder          .readIdentified(in, access, targets)
        // case ProcGroup       .typeID => ProcGroup       .readIdentified(in, access, targets)
        // case AudioGrapheme   .typeID => AudioGrapheme   .readIdentified(in, access, targets)
        // case ArtifactLocation.typeID => ArtifactLocation.readIdentified(in, access, targets)
        case _                       => sys.error(s"Unexpected element type cookie $typeID")
      }
    }
  }
}