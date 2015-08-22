///*
// *  ElemImpl.scala
// *  (SoundProcesses)
// *
// *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
// *
// *	This software is published under the GNU General Public License v2+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss
//package synth
//package proc
//package impl
//
//import de.sciss.lucre.event.Sys
//import de.sciss.lucre.expr.{Expr => _Expr}
//import de.sciss.lucre.synth.InMemory
//import de.sciss.lucre.{event => evt}
//import de.sciss.serial.DataInput
//
//import scala.collection.immutable.{IndexedSeq => Vec}
//import scala.language.higherKinds
//
//object ElemImpl {
//  import java.lang.{String => _String}
//
//  import de.sciss.lucre.artifact.{Artifact => _Artifact, ArtifactLocation => _ArtifactLocation}
//  import de.sciss.lucre.synth.expr.{DoubleVec => _DoubleVec}
//  import proc.{FadeSpec => _FadeSpec, Proc => _Proc, Scan => _Scan, Timeline => _Timeline}
//
//  import scala.{Boolean => _Boolean, Double => _Double, Int => _Int, Long => _Long}
//
//  // ---- Int ----
//
//  object Int extends ExprElemCompanionImpl[proc.IntElem, _Int] {
//    protected val tpe = lucre.expr.Int
//
//    protected def newActive[S <: Sys[S]](targets: evt.Targets[S], peer: _Expr[S, _Int])
//                                        (implicit tx: S#Tx): IntElem[S] with evt.Node[S] =
//      new IntActiveImpl(targets, peer)
//
//    protected def newConst[S <: Sys[S]](peer: _Expr.Const[S, _Int])(implicit tx: S#Tx): IntElem[S] =
//      new IntConstImpl[S](peer)
//  }
//
//  private trait IntImpl[S <: Sys[S]] extends IntElem[S] {
//    final def prefix = "Int"
//    final def typeID = Int.typeID
//  }
//
//  private final class IntConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, _Int])
//    extends IntImpl[S] with PassiveElemImpl[S, IntElem[S]]
//
//  private final class IntActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _Int])
//    extends ActiveElemImpl[S] with IntImpl[S] {
//
//    def mkCopy()(implicit tx: S#Tx): IntElem[S] = Int(Int.copyExpr(peer))
//  }
//
//  // ---- Long ----
//
//  object Long extends ExprElemCompanionImpl[LongElem, _Long] {
//    protected val tpe = lucre.expr.Long
//
//    protected def newActive[S <: Sys[S]](targets: evt.Targets[S], peer: _Expr[S, _Long])
//                                        (implicit tx: S#Tx): LongElem[S] with evt.Node[S] =
//      new LongActiveImpl(targets, peer)
//
//    protected def newConst[S <: Sys[S]](peer: _Expr.Const[S, _Long])(implicit tx: S#Tx): LongElem[S] =
//      new LongConstImpl[S](peer)
//  }
//
//  private trait LongImpl[S <: Sys[S]] extends LongElem[S] {
//    final def prefix = "Long"
//    final def typeID = Long.typeID
//  }
//
//  private final class LongConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, _Long])
//    extends LongImpl[S] with PassiveElemImpl[S, LongElem[S]]
//
//  private final class LongActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _Long])
//    extends ActiveElemImpl[S] with LongImpl[S] {
//
//    def mkCopy()(implicit tx: S#Tx): LongElem[S] = Long(Long.copyExpr(peer))
//  }
//
//  // ---- Double ----
//
//  object Double extends ExprElemCompanionImpl[DoubleElem, _Double] {
//    protected val tpe = lucre.expr.Double
//
//    protected def newActive[S <: Sys[S]](targets: evt.Targets[S], peer: _Expr[S, _Double])
//                                        (implicit tx: S#Tx): DoubleElem[S] with evt.Node[S] =
//      new DoubleActiveImpl(targets, peer)
//
//    protected def newConst[S <: Sys[S]](peer: _Expr.Const[S, _Double])(implicit tx: S#Tx): DoubleElem[S] =
//      new DoubleConstImpl[S](peer)
//  }
//
//  private trait DoubleImpl[S <: Sys[S]] extends DoubleElem[S] {
//    final def prefix = "Double"
//    final def typeID = Double.typeID
//  }
//
//  private final class DoubleConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, _Double])
//    extends DoubleImpl[S] with PassiveElemImpl[S, DoubleElem[S]]
//
//  private final class DoubleActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _Double])
//    extends ActiveElemImpl[S] with DoubleImpl[S] {
//
//    def mkCopy()(implicit tx: S#Tx): DoubleElem[S] = Double(Double.copyExpr(peer))
//  }
//
//  // ---- Boolean ----
//
//  object Boolean extends ExprElemCompanionImpl[BooleanElem, _Boolean] {
//    protected val tpe = lucre.expr.Boolean
//
//    protected def newActive[S <: Sys[S]](targets: evt.Targets[S], peer: _Expr[S, _Boolean])
//                                        (implicit tx: S#Tx): BooleanElem[S] with evt.Node[S] =
//      new BooleanActiveImpl(targets, peer)
//
//    protected def newConst[S <: Sys[S]](peer: _Expr.Const[S, _Boolean])(implicit tx: S#Tx): BooleanElem[S] =
//      new BooleanConstImpl[S](peer)
//  }
//
//  private trait BooleanImpl[S <: Sys[S]] extends BooleanElem[S] {
//    final def prefix = "Boolean"
//    final def typeID = Boolean.typeID
//  }
//
//  private final class BooleanConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, _Boolean])
//    extends BooleanImpl[S] with PassiveElemImpl[S, BooleanElem[S]]
//
//  private final class BooleanActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _Boolean])
//    extends ActiveElemImpl[S] with BooleanImpl[S] {
//
//    def mkCopy()(implicit tx: S#Tx): BooleanElem[S] = Boolean(Boolean.copyExpr(peer))
//  }
//
//  // ---- String ----
//
//  object String extends ExprElemCompanionImpl[StringElem, _String] {
//    protected val tpe = lucre.expr.String
//
//    protected def newActive[S <: Sys[S]](targets: evt.Targets[S], peer: _Expr[S, _String])
//                                        (implicit tx: S#Tx): StringElem[S] with evt.Node[S] =
//      new StringActiveImpl(targets, peer)
//
//    protected def newConst[S <: Sys[S]](peer: _Expr.Const[S, _String])(implicit tx: S#Tx): StringElem[S] =
//      new StringConstImpl[S](peer)
//  }
//
//  private trait StringImpl[S <: Sys[S]] extends StringElem[S] {
//    final def prefix = "String"
//    final def typeID = String.typeID
//  }
//
//  private final class StringConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, _String])
//    extends StringImpl[S] with PassiveElemImpl[S, StringElem[S]]
//
//  private final class StringActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _String])
//    extends ActiveElemImpl[S] with StringImpl[S] {
//
//    def mkCopy()(implicit tx: S#Tx): StringElem[S] = String(String.copyExpr(peer))
//  }
//
//  // ---- FadeSpec ----
//
//  object FadeSpec extends ElemCompanionImpl[_FadeSpec.Elem] {
//    final val typeID = _FadeSpec.Expr.typeID
//
//    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
//                                       (implicit tx: S#Tx): _FadeSpec.Elem[S] with evt.Node[S] = {
//      val peer = _FadeSpec.Expr.read(in, access)
//      new FadeSpecActiveImpl(targets, peer)
//    }
//
//    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): _FadeSpec.Elem[S] = {
//      val peer = _FadeSpec.Expr.readConst[S](in)
//      new FadeSpecConstImpl[S](peer)
//    }
//
//    def apply[S <: Sys[S]](peer: _Expr[S, _FadeSpec])(implicit tx: S#Tx): _FadeSpec.Elem[S] = {
//      peer match {
//        // note: using _FadeSpec produces a bug in Scala 2.10; import Value instead
//        case c: _Expr.Const[S, _FadeSpec] => new FadeSpecConstImpl(c)
//        case _                            => new FadeSpecActiveImpl(evt.Targets[S], peer)
//      }
//    }
//  }
//
//  private trait FadeSpecImpl[S <: Sys[S]] extends _FadeSpec.Elem[S] {
//    final def typeID = FadeSpec.typeID
//    final def prefix = "FadeSpec"
//  }
//
//  private final class FadeSpecConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, _FadeSpec])
//    extends FadeSpecImpl[S] with PassiveElemImpl[S, _FadeSpec.Elem[S]]
//
//  private final class FadeSpecActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, _FadeSpec])
//    extends ActiveElemImpl[S] with FadeSpecImpl[S] {
//
//    def mkCopy()(implicit tx: S#Tx): _FadeSpec.Elem[S] = {
//      val newPeer = peer match {
//        case _Expr.Var(vr) => _FadeSpec.Expr.newVar(vr())
//        //        case _FadeSpec.Expr(numFrames, shape, floor) =>
//        //          val curveCopy = shape match {
//        //            case _Expr.Var(vr) => Curves.newVar(vr())
//        //            case _ => shape
//        //          }
//        //          _FadeSpec.Expr(Long.copyExpr(numFrames), curveCopy, Double.copyExpr(floor))
//        case _ => peer
//      }
//      FadeSpec[S](newPeer)
//    }
//  }
//
//  // ---- DoubleVec ----
//
//  object DoubleVec extends ElemCompanionImpl[DoubleVecElem] {
//    val typeID = _DoubleVec.typeID
//
//    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
//                                       (implicit tx: S#Tx): DoubleVecElem[S] with evt.Node[S] = {
//      val peer = _DoubleVec.read(in, access)
//      new DoubleVecActiveImpl(targets, peer)
//    }
//
//    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): DoubleVecElem[S] = {
//      val peer = _DoubleVec.readConst[S](in)
//      new DoubleVecConstImpl[S](peer)
//    }
//
//    def apply[S <: Sys[S]](peer: _Expr[S, Vec[_Double]])(implicit tx: S#Tx): DoubleVecElem[S] =
//      if (_Expr.isConst(peer))
//        new DoubleVecConstImpl(peer.asInstanceOf[_Expr.Const[S, Vec[_Double]]])
//      else
//        new DoubleVecActiveImpl(evt.Targets[S], peer)
//  }
//
//  private trait DoubleVecImpl[S <: Sys[S]] extends DoubleVecElem[S] {
//    final def typeID = DoubleVec.typeID
//    final def prefix = "DoubleVec"
//  }
//
//  private final class DoubleVecConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, Vec[_Double]])
//    extends DoubleVecImpl[S] with PassiveElemImpl[S, DoubleVecElem[S]]
//
//  private final class DoubleVecActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: _Expr[S, Vec[_Double]])
//    extends ActiveElemImpl[S] with DoubleVecImpl[S] {
//
//    def mkCopy()(implicit tx: S#Tx): DoubleVecElem[S] = {
//      val newPeer = peer match {
//        case _Expr.Var(vr) => _DoubleVec.newVar(vr())
//        case _ => peer
//      }
//      DoubleVec(newPeer)
//    }
//  }
//
//  // ---- AudioGrapheme ----
//
//  object AudioGrapheme extends ElemCompanionImpl[AudioGraphemeElem] {
//    val typeID = Grapheme.Expr.Audio.typeID
//
//    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
//                                       (implicit tx: S#Tx): AudioGraphemeElem[S] with evt.Node[S] = {
//      // val peer = Grapheme.Elem.Audio.readExpr(in, access)
//      val peer = Grapheme.Expr.Audio.read(in, access) match {
//        case a: Grapheme.Expr.Audio[S] => a
//        case other => sys.error(s"Expected a Grapheme.Elem.Audio, but found $other")  // XXX TODO
//      }
//      new AudioGraphemeActiveImpl(targets, peer)
//    }
//
//    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): AudioGraphemeElem[S] = {
//      // val peer = Grapheme.Elem.Audio.readConst[S](in)
//      // new AudioGraphemeConstImpl[S](peer)
//      sys.error("Constant Grapheme.Elem.Value not supported")
//    }
//
//    def apply[S <: Sys[S]](peer: Grapheme.Expr.Audio[S])(implicit tx: S#Tx): AudioGraphemeElem[S] = {
//      // peer match {
//      //  case c: _Expr.Const[S, Grapheme.Value.Audio] =>
//      //    new AudioGraphemeConstImpl(c)
//      //  case _ =>
//      new AudioGraphemeActiveImpl(evt.Targets[S], peer)
//      // }
//    }
//  }
//
//  private trait AudioGraphemeImpl[S <: Sys[S]] extends AudioGraphemeElem[S] {
//    final def typeID = AudioGrapheme.typeID
//    final def prefix = "AudioGrapheme"
//  }
//
//  //  final class AudioGraphemeConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, Grapheme.Value.Audio])
//  //    extends Passive[S] with AudioGraphemeImpl[S]
//
//  private final class AudioGraphemeActiveImpl[S <: Sys[S]](val targets: evt.Targets[S],
//                                                   val peer: Grapheme.Expr.Audio[S])
//    extends ActiveElemImpl[S] with AudioGraphemeImpl[S] {
//
//    def mkCopy()(implicit tx: S#Tx): AudioGraphemeElem[S] = {
//      peer.artifact
//      val artifactCopy  = peer.artifact // XXX TODO copy
//      val spec          = peer.spec
//      val offsetCopy    = Long  .copyExpr[S](peer.offset)
//      val gainCopy      = Double.copyExpr[S](peer.gain  )
//      val newPeer       = Grapheme.Expr.Audio(artifactCopy, spec, offsetCopy, gainCopy)
//      AudioGrapheme(newPeer)
//    }
//  }
//
//  // ---- ArtifactLocation ----
//
//  object ArtifactLocation extends ElemCompanionImpl[ArtifactLocationElem] {
//    val typeID = _ArtifactLocation.typeID
//
//    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
//                                   (implicit tx: S#Tx): ArtifactLocationElem[S] with evt.Node[S] = {
//      val peer = _ArtifactLocation.read(in, access)
//      new ArtifactLocationActiveImpl(targets, peer)
//    }
//
//    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): ArtifactLocationElem[S] =
//      sys.error("Constant Artifact.Location not supported")
//
//    def apply[S <: Sys[S]](peer: _ArtifactLocation[S])(implicit tx: S#Tx): ArtifactLocationElem[S] =
//      new ArtifactLocationActiveImpl(evt.Targets[S], peer)
//  }
//
//  private trait ArtifactLocationImpl[S <: Sys[S]] extends ArtifactLocationElem[S] {
//    final def typeID = ArtifactLocation.typeID
//    final def prefix = "ArtifactLocation"
//  }
//
//  //  final class AudioGraphemeConstImpl[S <: Sys[S]](val peer: _Expr.Const[S, Grapheme.Value.Audio])
//  //    extends Passive[S] with AudioGraphemeImpl[S]
//
//  private final class ArtifactLocationActiveImpl[S <: Sys[S]](val targets: evt.Targets[S],
//                                                      val peer: _ArtifactLocation[S])
//    extends ActiveElemImpl[S] with ArtifactLocationImpl[S] {
//
//    protected def peerEvent = peer.changed
//
//    def mkCopy()(implicit tx: S#Tx): ArtifactLocationElem[S] = ArtifactLocation(peer)
//  }
//
//  // ---- Artifact ----
//
//  object Artifact extends ElemCompanionImpl[ArtifactElem] {
//    val typeID = _Artifact.typeID
//
//    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
//                                   (implicit tx: S#Tx): ArtifactElem[S] with evt.Node[S] = {
//      val peer = _Artifact.read(in, access)
//      new ArtifactActiveImpl(targets, peer)
//    }
//
//    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): ArtifactElem[S] =
//      sys.error("Constant Artifact not supported")
//
//    def apply[S <: Sys[S]](peer: _Artifact[S])(implicit tx: S#Tx): ArtifactElem[S] =
//      new ArtifactActiveImpl(evt.Targets[S], peer)
//  }
//
//  private trait ArtifactImpl[S <: Sys[S]] extends ArtifactElem[S] {
//    final def typeID = Artifact.typeID
//    final def prefix = "Artifact"
//  }
//
//  private final class ArtifactActiveImpl[S <: Sys[S]](val targets: evt.Targets[S],
//                                                      val peer: _Artifact[S])
//    extends ActiveElemImpl[S] with ArtifactImpl[S] {
//
//    protected def peerEvent = peer.changed
//
//    def mkCopy()(implicit tx: S#Tx): ArtifactElem[S] = {
//      val peerCpy = _Artifact.Modifiable.copy(peer)
//      Artifact(peerCpy)
//    }
//  }
//
//  // ---- Timeline ----
//
//  object Timeline extends ElemCompanionImpl[_Timeline.Elem] {
//    val typeID = _Timeline.typeID
//
//    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
//                                   (implicit tx: S#Tx): _Timeline.Elem[S] with evt.Node[S] = {
//      val peer = _Timeline.read(in, access)
//      new TimelineActiveImpl(targets, peer)
//    }
//
//    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): _Timeline.Elem[S] =
//      sys.error("Constant Timeline not supported")
//
//    def apply[S <: Sys[S]](peer: _Timeline[S])(implicit tx: S#Tx): _Timeline.Elem[S] =
//      new TimelineActiveImpl(evt.Targets[S], peer)
//  }
//
//  private trait TimelineImpl[S <: Sys[S]] extends _Timeline.Elem[S] {
//    final def typeID = Timeline.typeID
//    final def prefix = "Timeline"
//  }
//
//  private final class TimelineActiveImpl[S <: Sys[S]](val targets: evt.Targets[S],
//                                               val peer: _Timeline[S])
//    extends ActiveElemImpl[S] with TimelineImpl[S] {
//
//    protected def peerEvent = peer.changed
//
//    def mkCopy()(implicit tx: S#Tx): _Timeline.Elem[S] = Timeline(peer) // XXX TODO
//  }
//
//  // ---- Proc ----
//
//  object Proc extends ElemCompanionImpl[_Proc.Elem] {
//    val typeID = _Proc.typeID
//
//    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
//                                   (implicit tx: S#Tx): _Proc.Elem[S] with evt.Node[S] = {
//      val peer = _Proc.read(in, access)
//      new ProcActiveImpl(targets, peer)
//    }
//
//    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): _Proc.Elem[S] =
//      sys.error("Constant ProcGroup not supported")
//
//    def apply[S <: Sys[S]](peer: _Proc[S])(implicit tx: S#Tx): _Proc.Elem[S] =
//      new ProcActiveImpl(evt.Targets[S], peer)
//  }
//
//  private trait ProcImpl[S <: Sys[S]] extends _Proc.Elem[S] {
//    final def typeID = Proc.typeID
//    final def prefix = "Proc"
//  }
//
//  private final class ProcActiveImpl[S <: Sys[S]](val targets: evt.Targets[S],
//                                          val peer: _Proc[S])
//    extends ActiveElemImpl[S] with ProcImpl[S] {
//
//    protected def peerEvent = peer.changed
//
//    def mkCopy()(implicit tx: S#Tx): _Proc.Elem[S] = {
//      val newPeer     = _Proc[S]
//      newPeer.graph() = peer.graph()
//      // peer.scans.keys.foreach(newPeer.scans.add)
//      peer.inputs.iterator.foreach { case (key, scan) =>
//        val scanNew = newPeer.inputs.add(key)
//        scan.iterator.foreach { link =>
//          scanNew.add(link)
//        }
//      }
//      peer.outputs.iterator.foreach { case (key, scan) =>
//        val scanNew = newPeer.outputs.add(key)
//        scan.iterator.foreach { link =>
//          scanNew.add(link)
//        }
//      }
//      Proc(newPeer)
//    }
//  }
//
//  // ---- Scan ----
//
//  object Scan extends ElemCompanionImpl[_Scan.Elem] {
//    val typeID = _Scan.typeID
//
//    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
//                                   (implicit tx: S#Tx): _Scan.Elem[S] with evt.Node[S] = {
//      val peer = _Scan.read(in, access)
//      new ScanActiveImpl(targets, peer)
//    }
//
//    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): _Scan.Elem[S] =
//      sys.error("Constant Scan not supported")
//
//    def apply[S <: Sys[S]](peer: _Scan[S])(implicit tx: S#Tx): _Scan.Elem[S] =
//      new ScanActiveImpl(evt.Targets[S], peer)
//  }
//
//  private trait ScanImpl[S <: Sys[S]] extends _Scan.Elem[S] {
//    final def typeID = Scan.typeID
//    final def prefix = "Scan"
//  }
//
//  private final class ScanActiveImpl[S <: Sys[S]](val targets: evt.Targets[S],
//                                                  val peer: _Scan[S])
//    extends ActiveElemImpl[S] with ScanImpl[S] {
//
//    protected def peerEvent = peer.changed
//
//    def mkCopy()(implicit tx: S#Tx): _Scan.Elem[S] = {
//      val newPeer = _Scan[S]
//      peer.iterator.foreach { link =>
//        newPeer.add(link)
//      }
//      _Scan.Elem(newPeer)
//    }
//  }
//
//  // ----------------- Serializer -----------------
//
//  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Elem[S]] = anySer.asInstanceOf[Ser[S]]
//
//  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S] = serializer[S].read(in, access)
//
//  private final val anySer = new Ser[InMemory]
//
//  private final val sync = new AnyRef
//
//  def debug() = extensions.toString()
//
//  @volatile private var extensions = Map[Int, Elem.Extension](
//    Int             .typeID -> Int             ,
//    Long            .typeID -> Long            ,
//    Double          .typeID -> Double          ,
//    Boolean         .typeID -> Boolean         ,
//    String          .typeID -> String          ,
//    FadeSpec        .typeID -> FadeSpec        ,
//    DoubleVec       .typeID -> DoubleVec       ,
//    AudioGrapheme   .typeID -> AudioGrapheme   ,
//    ArtifactLocation.typeID -> ArtifactLocation,
//    Artifact        .typeID -> Artifact        ,
//    Proc            .typeID -> Proc            ,
//    Scan            .typeID -> Scan            ,
//    Timeline        .typeID -> Timeline        ,
//    FolderElemImpl  .typeID -> FolderElemImpl  ,
//    Ensemble        .typeID -> EnsembleImpl.ElemImpl,
//    Action          .typeID -> ActionImpl  .ElemImpl,
//    Code            .typeID -> CodeImpl    .ElemImpl
//  )
//
//  def registerExtension(ext: Elem.Extension): Unit = sync.synchronized {
//    val typeID = ext.typeID
//    if (extensions.contains(typeID))
//      throw new IllegalArgumentException(s"An Elem extension of type $typeID was already registered")
//
//    extensions += typeID -> ext
//  }
//
//  private final class Ser[S <: Sys[S]] extends evt.EventLikeSerializer[S, Elem[S]] {
//    private def getExtension(in: DataInput): Elem.Extension = {
//      val typeID  = in.readInt()
//      val tpe     = extensions.getOrElse(typeID, sys.error(s"Unexpected element type cookie $typeID"))
//      tpe
//    }
//
//    def readConstant(in: DataInput)(implicit tx: S#Tx): Elem[S] =
//      getExtension(in).readIdentifiedConstant(in)
//
//    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Elem[S] with evt.Node[S] =
//      getExtension(in).readIdentified(in, access, targets)
//  }
//}