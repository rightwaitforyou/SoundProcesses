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
import de.sciss.lucre.event.{EventLike, Sys, Event}
import serial.{DataInput, DataOutput, Writable}
import stm.Disposable
import expr.{Expr => _Expr}
import de.sciss.lucre.synth.InMemory
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.higherKinds
import de.sciss.synth.proc.Attr

object AttrImpl {
  import Attr.Update
  import scala.{Int => _Int, Double => _Double, Boolean => _Boolean, Long => _Long}
  import java.lang.{String => _String}
  import proc.{FadeSpec => _FadeSpec}
  import lucre.synth.expr.{DoubleVec => _DoubleVec}

  // ---- Int ----

  object Int extends Companion[Attr.Int] {
    final val typeID = lucre.expr.Int.typeID

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): Attr.Int[S] with evt.Node[S] = {
      val peer = lucre.expr.Int.read(in, access)
      new IntActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Attr.Int[S] = {
      val peer = lucre.expr.Int.readConst[S](in)
      new IntConstImpl[S](peer)
    }

    def apply[S <: Sys[S]](peer: _Expr[S, _Int])(implicit tx: S#Tx): Attr.Int[S] = peer match {
      case c: _Expr.Const[S, _Int]  => new IntConstImpl(c)
      case _                        => new IntActiveImpl(evt.Targets[S], peer)
    }
  }

  trait IntImpl[S <: Sys[S]] extends Attr.Int[S] {
    final def prefix = "Int"
    final def typeID = Int.typeID
  }

  final class IntConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, _Int])
    extends Passive[S] with IntImpl[S]

  final class IntActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _Int])
    extends Expr[S, _Int] with IntImpl[S] {

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

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): Attr.Long[S] with evt.Node[S] = {
      val peer = lucre.expr.Long.read(in, access)
      new LongActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Attr.Long[S] = {
      val peer = lucre.expr.Long.readConst[S](in)
      new LongConstImpl[S](peer)
    }

    def apply[S <: Sys[S]](peer: _Expr[S, _Long])(implicit tx: S#Tx): Attr.Long[S] = peer match {
      case c: _Expr.Const[S, _Long] => new LongConstImpl(c)
      case _                        => new LongActiveImpl(evt.Targets[S], peer)
    }
  }

  trait LongImpl[S <: Sys[S]] extends Attr.Long[S] {
    final def prefix = "Long"
    final def typeID = Long.typeID
  }

  final class LongConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, _Long])
    extends Passive[S] with LongImpl[S]

  final class LongActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _Long])
    extends Expr[S, _Long] with LongImpl[S] {

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

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): Attr.Double[S] with evt.Node[S] = {
      val peer = lucre.expr.Double.read(in, access)
      new DoubleActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Attr.Double[S] = {
      val peer = lucre.expr.Double.readConst[S](in)
      new DoubleConstImpl[S](peer)
    }

    def apply[S <: Sys[S]](peer: _Expr[S, _Double])(implicit tx: S#Tx): Attr.Double[S] = peer match {
      case c: _Expr.Const[S, _Double] => new DoubleConstImpl(c)
      case _                          => new DoubleActiveImpl(evt.Targets[S], peer)
    }
  }

  trait DoubleImpl[S <: Sys[S]] extends Attr.Double[S] {
    final def prefix = "Double"
    final def typeID = Double.typeID
  }

  final class DoubleConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, _Double])
    extends Passive[S] with DoubleImpl[S]

  final class DoubleActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _Double])
    extends Expr[S, _Double] with DoubleImpl[S] {

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

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): Attr.Boolean[S] with evt.Node[S] = {
      val peer = lucre.expr.Boolean.read(in, access)
      new BooleanActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Attr.Boolean[S] = {
      val peer = lucre.expr.Boolean.readConst[S](in)
      new BooleanConstImpl[S](peer)
    }

    def apply[S <: Sys[S]](peer: _Expr[S, _Boolean])(implicit tx: S#Tx): Attr.Boolean[S] = peer match {
      case c: _Expr.Const[S, _Boolean]  => new BooleanConstImpl(c)
      case _                            => new BooleanActiveImpl(evt.Targets[S], peer)
    }
  }

  trait BooleanImpl[S <: Sys[S]] extends Attr.Boolean[S] {
    final def prefix = "Boolean"
    final def typeID = Boolean.typeID
  }

  final class BooleanConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, _Boolean])
    extends Passive[S] with BooleanImpl[S]

  final class BooleanActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _Boolean])
    extends Expr[S, _Boolean] with BooleanImpl[S] {

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

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): Attr.String[S] with evt.Node[S] = {
      val peer = lucre.expr.String.read(in, access)
      new StringActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Attr.String[S] = {
      val peer = lucre.expr.String.readConst[S](in)
      new StringConstImpl[S](peer)
    }

    def apply[S <: Sys[S]](peer: _Expr[S, _String])(implicit tx: S#Tx): Attr.String[S] = peer match {
      case c: _Expr.Const[S, _String] => new StringConstImpl(c)
      case _                          => new StringActiveImpl(evt.Targets[S], peer)
    }
  }

  trait StringImpl[S <: Sys[S]] extends Attr.String[S] {
    final def prefix = "String"
    final def typeID = String.typeID
  }

  final class StringConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, _String])
    extends Passive[S] with StringImpl[S]

  final class StringActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _String])
    extends Expr[S, _String] with StringImpl[S] {

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

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): Attr.FadeSpec[S] with evt.Node[S] = {
      val peer = _FadeSpec.Elem.read(in, access)
      new FadeSpecActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Attr.FadeSpec[S] = {
      val peer = _FadeSpec.Elem.readConst[S](in)
      new FadeSpecConstImpl[S](peer)
    }

    def apply[S <: Sys[S]](peer: _Expr[S, _FadeSpec.Value])(implicit tx: S#Tx): Attr.FadeSpec[S] = peer match {
      case c: _Expr.Const[S, _FadeSpec.Value] => new FadeSpecConstImpl(c)
      case _                                  => new FadeSpecActiveImpl(evt.Targets[S], peer)
    }
  }

  trait FadeSpecImpl[S <: Sys[S]] extends Attr.FadeSpec[S] {
    final def typeID = FadeSpec.typeID
    final def prefix = "FadeSpec"
  }

  final class FadeSpecConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, _FadeSpec.Value])
    extends Passive[S] with FadeSpecImpl[S]

  final class FadeSpecActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _FadeSpec.Value])
    extends Expr[S, _FadeSpec.Value] with FadeSpecImpl[S] {

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

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): Attr.DoubleVec[S] with evt.Node[S] = {
      val peer = _DoubleVec.read(in, access)
      new DoubleVecActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Attr.DoubleVec[S] = {
      val peer = _DoubleVec.readConst[S](in)
      new DoubleVecConstImpl[S](peer)
    }

    def apply[S <: Sys[S]](peer: _Expr[S, Vec[_Double]])(implicit tx: S#Tx): Attr.DoubleVec[S] =
      if (_Expr.isConst(peer))
        new DoubleVecConstImpl(peer.asInstanceOf[_Expr.Const[S, Vec[_Double]]])
      else
        new DoubleVecActiveImpl(evt.Targets[S], peer)
  }

  trait DoubleVecImpl[S <: Sys[S]] extends Attr.DoubleVec[S] {
    final def typeID = DoubleVec.typeID
    final def prefix = "DoubleVec"
  }

  final class DoubleVecConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, Vec[_Double]])
    extends Passive[S] with DoubleVecImpl[S]

  final class DoubleVecActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, Vec[_Double]])
    extends Expr[S, Vec[_Double]] with DoubleVecImpl[S] {

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

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): Attr.AudioGrapheme[S] with evt.Node[S] = {
      // val peer = Grapheme.Elem.Audio.readExpr(in, access)
      val peer = Grapheme.Elem.Audio.read(in, access) match {
        case a: Grapheme.Elem.Audio[S] => a
        case other => sys.error(s"Expected a Grapheme.Elem.Audio, but found $other")  // XXX TODO
      }
      new AudioGraphemeActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Attr.AudioGrapheme[S] = {
      // val peer = Grapheme.Elem.Audio.readConst[S](in)
      // new AudioGraphemeConstImpl[S](peer)
      sys.error("Constant Grapheme.Elem.Value not supported")
    }

    def apply[S <: Sys[S]](peer: Grapheme.Elem.Audio[S])(implicit tx: S#Tx): Attr.AudioGrapheme[S] = {
      // peer match {
      //  case c: _Expr.Const[S, Grapheme.Value.Audio] =>
      //    new AudioGraphemeConstImpl(c)
      //  case _ =>
      new AudioGraphemeActiveImpl(evt.Targets[S], peer)
      // }
    }
  }

  trait AudioGraphemeImpl[S <: Sys[S]] extends Attr.AudioGrapheme[S] {
    final def typeID = AudioGrapheme.typeID
    final def prefix = "AudioGrapheme"
  }

  //  final class AudioGraphemeConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, Grapheme.Value.Audio])
  //    extends Passive[S] with AudioGraphemeImpl[S]

  final class AudioGraphemeActiveImpl[S <: Sys[S]](val targets: evt.Targets[S],
                                                   val peer: Grapheme.Elem.Audio[S])
    extends Expr[S, Grapheme.Value.Audio] with AudioGraphemeImpl[S] {

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

  trait Companion[E[S <: Sys[S]] <: Attr[S]] extends Attr.Extension {
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

  trait Basic[S <: Sys[S]]
    extends Attr[S] {
    self =>

    type Peer <: Writable with Disposable[S#Tx]

    protected def typeID: Int

    final protected def writeData(out: DataOutput): Unit = {
      out.writeInt(typeID)
      peer.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = peer.dispose()

    protected def prefix: String

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

  trait Passive[S <: Sys[S]]
    extends Basic[S] with evt.impl.Constant {

    final def mkCopy()(implicit tx: S#Tx): this.type = this

    final def changed: EventLike[S, Update[S]] = evt.Dummy[S, Update[S]]

    override def toString() = s"Attr.${prefix}($peer)"

    final def dispose()(implicit tx: S#Tx): Unit = disposeData()
  }

  trait Active[S <: Sys[S]]
    extends Basic[S] with evt.Node[S] {
    self =>

    protected def peerEvent: evt.EventLike[S, Any]

    override def toString() = s"Attr.${prefix}$id"

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

  trait Expr[S <: Sys[S], A] extends Active[S] {
    type Peer <: _Expr[S, A]
    final protected def peerEvent = peer.changed
  }

  // ----------------- Serializer -----------------

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Attr[S]] = anySer.asInstanceOf[Ser[S]]

  private final val anySer = new Ser[InMemory]

  private final val sync = new AnyRef

  @volatile private var extensions = Map[Int, Attr.Extension](
    Int             .typeID -> Int          ,
    Long            .typeID -> Long         ,
    Double          .typeID -> Double       ,
    Boolean         .typeID -> Boolean      ,
    String          .typeID -> String       ,
    FadeSpec        .typeID -> FadeSpec     ,
    DoubleVec       .typeID -> DoubleVec    ,
    AudioGrapheme   .typeID -> AudioGrapheme
  )

  def registerExtension(ext: Attr.Extension): Unit = sync.synchronized {
    val typeID = ext.typeID
    if (extensions.contains(typeID))
      throw new IllegalArgumentException(s"An Attr extension of type $typeID was already registered")

    extensions += typeID -> ext
  }

  private final class Ser[S <: Sys[S]] extends evt.EventLikeSerializer[S, Attr[S]] {
    private def getExtension(in: DataInput): Attr.Extension = {
      val typeID  = in.readInt()
      val tpe     = extensions.getOrElse(typeID, sys.error(s"Unexpected element type cookie $typeID"))
      tpe
    }

    def readConstant(in: DataInput)(implicit tx: S#Tx): Attr[S] =
      getExtension(in).readIdentifiedConstant(in)

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Attr[S] with evt.Node[S] =
      getExtension(in).readIdentified(in, access, targets)
  }
}