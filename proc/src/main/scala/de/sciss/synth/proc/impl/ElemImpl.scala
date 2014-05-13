/*
 *  ElemImpl.scala
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
import de.sciss.synth.proc.Elem

object ElemImpl {
  import Elem.Update
  import scala.{Int => _Int, Double => _Double, Boolean => _Boolean, Long => _Long}
  import java.lang.{String => _String}
  import proc.{FadeSpec => _FadeSpec, ArtifactLocation => _ArtifactLocation, ProcGroup => _ProcGroup, Proc => _Proc}
  import lucre.synth.expr.{DoubleVec => _DoubleVec}

  // ---- Int ----

  object Int extends Companion[IntElem] {
    final val typeID = lucre.expr.Int.typeID

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): IntElem[S] with evt.Node[S] = {
      val peer = lucre.expr.Int.read(in, access)
      new IntActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): IntElem[S] = {
      val peer = lucre.expr.Int.readConst[S](in)
      new IntConstImpl[S](peer)
    }

    def apply[S <: Sys[S]](peer: _Expr[S, _Int])(implicit tx: S#Tx): IntElem[S] = peer match {
      case c: _Expr.Const[S, _Int]  => new IntConstImpl(c)
      case _                        => new IntActiveImpl(evt.Targets[S], peer)
    }
  }

  trait IntImpl[S <: Sys[S]] extends IntElem[S] {
    final def prefix = "Int"
    final def typeID = Int.typeID
  }

  final class IntConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, _Int])
    extends Passive[S] with IntImpl[S]

  final class IntActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _Int])
    extends Active[S] with IntImpl[S] {

    def mkCopy()(implicit tx: S#Tx): IntElem[S] = {
      val newPeer = peer match {
        case _Expr.Var(vr) => lucre.expr.Int.newVar(vr())
        case _ => peer
      }
      Int(newPeer)
    }
  }

  // ---- Long ----

  object Long extends Companion[LongElem] {
    final val typeID = lucre.expr.Long.typeID

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): LongElem[S] with evt.Node[S] = {
      val peer = lucre.expr.Long.read(in, access)
      new LongActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): LongElem[S] = {
      val peer = lucre.expr.Long.readConst[S](in)
      new LongConstImpl[S](peer)
    }

    def apply[S <: Sys[S]](peer: _Expr[S, _Long])(implicit tx: S#Tx): LongElem[S] = peer match {
      case c: _Expr.Const[S, _Long] => new LongConstImpl(c)
      case _                        => new LongActiveImpl(evt.Targets[S], peer)
    }
  }

  trait LongImpl[S <: Sys[S]] extends LongElem[S] {
    final def prefix = "Long"
    final def typeID = Long.typeID
  }

  final class LongConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, _Long])
    extends Passive[S] with LongImpl[S]

  final class LongActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _Long])
    extends Active[S] with LongImpl[S] {

    def mkCopy()(implicit tx: S#Tx): LongElem[S] = {
      val newPeer = peer match {
        case _Expr.Var(vr) => lucre.expr.Long.newVar(vr())
        case _ => peer
      }
      Long(newPeer)
    }
  }

  // ---- Double ----

  object Double extends Companion[DoubleElem] {
    final val typeID = lucre.expr.Double.typeID

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): DoubleElem[S] with evt.Node[S] = {
      val peer = lucre.expr.Double.read(in, access)
      new DoubleActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): DoubleElem[S] = {
      val peer = lucre.expr.Double.readConst[S](in)
      new DoubleConstImpl[S](peer)
    }

    def apply[S <: Sys[S]](peer: _Expr[S, _Double])(implicit tx: S#Tx): DoubleElem[S] = peer match {
      case c: _Expr.Const[S, _Double] => new DoubleConstImpl(c)
      case _                          => new DoubleActiveImpl(evt.Targets[S], peer)
    }
  }

  trait DoubleImpl[S <: Sys[S]] extends DoubleElem[S] {
    final def prefix = "Double"
    final def typeID = Double.typeID
  }

  final class DoubleConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, _Double])
    extends Passive[S] with DoubleImpl[S]

  final class DoubleActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _Double])
    extends Active[S] with DoubleImpl[S] {

    def mkCopy()(implicit tx: S#Tx): DoubleElem[S] = {
      val newPeer = peer match {
        case _Expr.Var(vr) => lucre.expr.Double.newVar(vr())
        case _ => peer
      }
      Double(newPeer)
    }
  }

  // ---- Boolean ----

  object Boolean extends Companion[BooleanElem] {
    final val typeID = lucre.expr.Boolean.typeID

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): BooleanElem[S] with evt.Node[S] = {
      val peer = lucre.expr.Boolean.read(in, access)
      new BooleanActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): BooleanElem[S] = {
      val peer = lucre.expr.Boolean.readConst[S](in)
      new BooleanConstImpl[S](peer)
    }

    def apply[S <: Sys[S]](peer: _Expr[S, _Boolean])(implicit tx: S#Tx): BooleanElem[S] = peer match {
      case c: _Expr.Const[S, _Boolean]  => new BooleanConstImpl(c)
      case _                            => new BooleanActiveImpl(evt.Targets[S], peer)
    }
  }

  trait BooleanImpl[S <: Sys[S]] extends BooleanElem[S] {
    final def prefix = "Boolean"
    final def typeID = Boolean.typeID
  }

  final class BooleanConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, _Boolean])
    extends Passive[S] with BooleanImpl[S]

  final class BooleanActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _Boolean])
    extends Active[S] with BooleanImpl[S] {

    def mkCopy()(implicit tx: S#Tx): BooleanElem[S] = {
      val newPeer = peer match {
        case _Expr.Var(vr) => lucre.expr.Boolean.newVar(vr())
        case _ => peer
      }
      Boolean(newPeer)
    }
  }

  // ---- String ----

  object String extends Companion[StringElem] {
    val typeID = lucre.expr.String.typeID

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): StringElem[S] with evt.Node[S] = {
      val peer = lucre.expr.String.read(in, access)
      new StringActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): StringElem[S] = {
      val peer = lucre.expr.String.readConst[S](in)
      new StringConstImpl[S](peer)
    }

    def apply[S <: Sys[S]](peer: _Expr[S, _String])(implicit tx: S#Tx): StringElem[S] = peer match {
      case c: _Expr.Const[S, _String] => new StringConstImpl(c)
      case _                          => new StringActiveImpl(evt.Targets[S], peer)
    }
  }

  trait StringImpl[S <: Sys[S]] extends StringElem[S] {
    final def prefix = "String"
    final def typeID = String.typeID
  }

  final class StringConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, _String])
    extends Passive[S] with StringImpl[S]

  final class StringActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _String])
    extends Active[S] with StringImpl[S] {

    def mkCopy()(implicit tx: S#Tx): StringElem[S] = {
      val newPeer = peer match {
        case _Expr.Var(vr) => lucre.expr.String.newVar(vr())
        case _ => peer
      }
      String(newPeer)
    }
  }

  // ---- FadeSpec ----

  object FadeSpec extends Companion[_FadeSpec.Elem] {
    final val typeID = _FadeSpec.Expr.typeID

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): _FadeSpec.Elem[S] with evt.Node[S] = {
      val peer = _FadeSpec.Expr.read(in, access)
      new FadeSpecActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): _FadeSpec.Elem[S] = {
      val peer = _FadeSpec.Expr.readConst[S](in)
      new FadeSpecConstImpl[S](peer)
    }

    def apply[S <: Sys[S]](peer: _Expr[S, _FadeSpec])(implicit tx: S#Tx): _FadeSpec.Elem[S] = {
      peer match {
        // note: using _FadeSpec produces a bug in Scala 2.10; import Value instead
        case c: _Expr.Const[S, _FadeSpec] => new FadeSpecConstImpl(c)
        case _                            => new FadeSpecActiveImpl(evt.Targets[S], peer)
      }
    }
  }

  trait FadeSpecImpl[S <: Sys[S]] extends _FadeSpec.Elem[S] {
    final def typeID = FadeSpec.typeID
    final def prefix = "FadeSpec"
  }

  final class FadeSpecConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, _FadeSpec])
    extends Passive[S] with FadeSpecImpl[S]

  final class FadeSpecActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _FadeSpec])
    extends Active[S] with FadeSpecImpl[S] {

    def mkCopy()(implicit tx: S#Tx): _FadeSpec.Elem[S] = {
      val newPeer = peer match {
        case _Expr.Var(vr) => _FadeSpec.Expr.newVar(vr())
        case _ => peer
      }
      FadeSpec(newPeer)
    }
  }

  // ---- DoubleVec ----

  object DoubleVec extends Companion[DoubleVecElem] {
    val typeID = _DoubleVec.typeID

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): DoubleVecElem[S] with evt.Node[S] = {
      val peer = _DoubleVec.read(in, access)
      new DoubleVecActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): DoubleVecElem[S] = {
      val peer = _DoubleVec.readConst[S](in)
      new DoubleVecConstImpl[S](peer)
    }

    def apply[S <: Sys[S]](peer: _Expr[S, Vec[_Double]])(implicit tx: S#Tx): DoubleVecElem[S] =
      if (_Expr.isConst(peer))
        new DoubleVecConstImpl(peer.asInstanceOf[_Expr.Const[S, Vec[_Double]]])
      else
        new DoubleVecActiveImpl(evt.Targets[S], peer)
  }

  trait DoubleVecImpl[S <: Sys[S]] extends DoubleVecElem[S] {
    final def typeID = DoubleVec.typeID
    final def prefix = "DoubleVec"
  }

  final class DoubleVecConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, Vec[_Double]])
    extends Passive[S] with DoubleVecImpl[S]

  final class DoubleVecActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, Vec[_Double]])
    extends Active[S] with DoubleVecImpl[S] {

    def mkCopy()(implicit tx: S#Tx): DoubleVecElem[S] = {
      val newPeer = peer match {
        case _Expr.Var(vr) => _DoubleVec.newVar(vr())
        case _ => peer
      }
      DoubleVec(newPeer)
    }
  }

  // ---- AudioGrapheme ----

  object AudioGrapheme extends Companion[AudioGraphemeElem] {
    val typeID = Grapheme.Expr.Audio.typeID

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                       (implicit tx: S#Tx): AudioGraphemeElem[S] with evt.Node[S] = {
      // val peer = Grapheme.Elem.Audio.readExpr(in, access)
      val peer = Grapheme.Expr.Audio.read(in, access) match {
        case a: Grapheme.Expr.Audio[S] => a
        case other => sys.error(s"Expected a Grapheme.Elem.Audio, but found $other")  // XXX TODO
      }
      new AudioGraphemeActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): AudioGraphemeElem[S] = {
      // val peer = Grapheme.Elem.Audio.readConst[S](in)
      // new AudioGraphemeConstImpl[S](peer)
      sys.error("Constant Grapheme.Elem.Value not supported")
    }

    def apply[S <: Sys[S]](peer: Grapheme.Expr.Audio[S])(implicit tx: S#Tx): AudioGraphemeElem[S] = {
      // peer match {
      //  case c: _Expr.Const[S, Grapheme.Value.Audio] =>
      //    new AudioGraphemeConstImpl(c)
      //  case _ =>
      new AudioGraphemeActiveImpl(evt.Targets[S], peer)
      // }
    }
  }

  trait AudioGraphemeImpl[S <: Sys[S]] extends AudioGraphemeElem[S] {
    final def typeID = AudioGrapheme.typeID
    final def prefix = "AudioGrapheme"
  }

  //  final class AudioGraphemeConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, Grapheme.Value.Audio])
  //    extends Passive[S] with AudioGraphemeImpl[S]

  final class AudioGraphemeActiveImpl[S <: Sys[S]](val targets: evt.Targets[S],
                                                   val peer: Grapheme.Expr.Audio[S])
    extends Active[S] with AudioGraphemeImpl[S] {

    def mkCopy()(implicit tx: S#Tx): AudioGraphemeElem[S] = {
      val newPeer = peer
      //      match {
      //        case _Expr.Var(vr) => _DoubleVec.newVar(vr())
      //        case _ => peer
      //      }
      AudioGrapheme(newPeer)
    }
  }

  // ---- ArtifactLocation ----

  object ArtifactLocation extends Companion[_ArtifactLocation.Elem] {
    val typeID = 0x10003 // _ArtifactLocation.typeID

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                   (implicit tx: S#Tx): _ArtifactLocation.Elem[S] with evt.Node[S] = {
      val peer = _ArtifactLocation.read(in, access)
      new ArtifactLocationActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): _ArtifactLocation.Elem[S] =
      sys.error("Constant Artifact.Location not supported")

    def apply[S <: Sys[S]](peer: _ArtifactLocation[S])(implicit tx: S#Tx): _ArtifactLocation.Elem[S] =
      new ArtifactLocationActiveImpl(evt.Targets[S], peer)
  }

  trait ArtifactLocationImpl[S <: Sys[S]] extends _ArtifactLocation.Elem[S] {
    final def typeID = ArtifactLocation.typeID
    final def prefix = "ArtifactLocation"
  }

  //  final class AudioGraphemeConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, Grapheme.Value.Audio])
  //    extends Passive[S] with AudioGraphemeImpl[S]

  final class ArtifactLocationActiveImpl[S <: Sys[S]](val targets: evt.Targets[S],
                                                      val peer: _ArtifactLocation[S])
    extends Active[S] with ArtifactLocationImpl[S] {

    protected def peerEvent = peer.changed

    def mkCopy()(implicit tx: S#Tx): _ArtifactLocation.Elem[S] = ArtifactLocation(peer)
  }

  // ---- ProcGroup ----

  object ProcGroup extends Companion[ProcGroupElem] {
    val typeID = 0x10001 // _ProcGroup.typeID

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                   (implicit tx: S#Tx): ProcGroupElem[S] with evt.Node[S] = {
      val peer = _ProcGroup.read(in, access)
      new ProcGroupActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): ProcGroupElem[S] =
      sys.error("Constant ProcGroup not supported")

    def apply[S <: Sys[S]](peer: _ProcGroup[S])(implicit tx: S#Tx): ProcGroupElem[S] =
      new ProcGroupActiveImpl(evt.Targets[S], peer)
  }

  trait ProcGroupImpl[S <: Sys[S]] extends ProcGroupElem[S] {
    final def typeID = ProcGroup.typeID
    final def prefix = "ProcGroup"
  }

  final class ProcGroupActiveImpl[S <: Sys[S]](val targets: evt.Targets[S],
                                               val peer: _ProcGroup[S])
    extends Active[S] with ProcGroupImpl[S] {

    protected def peerEvent = peer.changed

    def mkCopy()(implicit tx: S#Tx): ProcGroupElem[S] = ProcGroup(peer)
  }

  // ---- Proc ----

  object Proc extends Companion[_Proc.Elem] {
    val typeID = _Proc.typeID

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                   (implicit tx: S#Tx): _Proc.Elem[S] with evt.Node[S] = {
      val peer = _Proc.read(in, access)
      new ProcActiveImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): _Proc.Elem[S] =
      sys.error("Constant ProcGroup not supported")

    def apply[S <: Sys[S]](peer: _Proc[S])(implicit tx: S#Tx): _Proc.Elem[S] =
      new ProcActiveImpl(evt.Targets[S], peer)
  }

  trait ProcImpl[S <: Sys[S]] extends _Proc.Elem[S] {
    final def typeID = Proc.typeID
    final def prefix = "Proc"
  }

  final class ProcActiveImpl[S <: Sys[S]](val targets: evt.Targets[S],
                                               val peer: _Proc[S])
    extends Active[S] with ProcImpl[S] {

    protected def peerEvent = peer.changed

    def mkCopy()(implicit tx: S#Tx): _Proc.Elem[S] = Proc(peer) // XXX TODO
  }

  // ---------- Impl ----------

  trait Companion[E[S <: Sys[S]] <: Elem[S]] extends Elem.Extension {
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

    final protected def reader: evt.Reader[S, Elem[S]] = serializer
  }

  trait Passive[S <: Sys[S]]
    extends Basic[S] with evt.impl.Constant {

    final def mkCopy()(implicit tx: S#Tx): this.type = this

    final def changed: EventLike[S, Update[S, PeerUpdate]] = evt.Dummy[S, Update[S, PeerUpdate]]

    override def toString = s"Elem.$prefix($peer)"

    final def dispose()(implicit tx: S#Tx): Unit = disposeData()
  }

  trait Active[S <: Sys[S]]
    extends Basic[S] with evt.Node[S] {
    self =>

    type Peer <: evt.Publisher[S, PeerUpdate] with Writable with Disposable[S#Tx]
    // private def peerEvent = peer.changed

    // protected def peerEvent: evt.EventLike[S, Any]

    override def toString() = s"Elem.${prefix}$id"

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

  // ----------------- Serializer -----------------

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Elem[S]] = anySer.asInstanceOf[Ser[S]]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S] = serializer[S].read(in, access)

  private final val anySer = new Ser[InMemory]

  private final val sync = new AnyRef

  @volatile private var extensions = Map[Int, Elem.Extension](
    Int             .typeID -> Int             ,
    Long            .typeID -> Long            ,
    Double          .typeID -> Double          ,
    Boolean         .typeID -> Boolean         ,
    String          .typeID -> String          ,
    FadeSpec        .typeID -> FadeSpec        ,
    DoubleVec       .typeID -> DoubleVec       ,
    AudioGrapheme   .typeID -> AudioGrapheme   ,
    ArtifactLocation.typeID -> ArtifactLocation,
    ProcGroup       .typeID -> ProcGroup       ,
    FolderElemImpl  .typeID -> FolderElemImpl
  )

  def registerExtension(ext: Elem.Extension): Unit = sync.synchronized {
    val typeID = ext.typeID
    if (extensions.contains(typeID))
      throw new IllegalArgumentException(s"An Elem extension of type $typeID was already registered")

    extensions += typeID -> ext
  }

  private final class Ser[S <: Sys[S]] extends evt.EventLikeSerializer[S, Elem[S]] {
    private def getExtension(in: DataInput): Elem.Extension = {
      val typeID  = in.readInt()
      val tpe     = extensions.getOrElse(typeID, sys.error(s"Unexpected element type cookie $typeID"))
      tpe
    }

    def readConstant(in: DataInput)(implicit tx: S#Tx): Elem[S] =
      getExtension(in).readIdentifiedConstant(in)

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Elem[S] with evt.Node[S] =
      getExtension(in).readIdentified(in, access, targets)
  }
}