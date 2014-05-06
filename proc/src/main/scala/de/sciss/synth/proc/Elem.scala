/*
 *  Elem.scala
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

import de.sciss.lucre.{event => evt}
import evt.{EventLike, Sys}
import de.sciss.lucre.stm.Disposable
import proc.impl.{ElemImpl => Impl}
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.{higherKinds, implicitConversions}
import de.sciss.serial.{Serializer, DataInput, Writable}
import de.sciss.lucre.expr.Expr

object Elem {
  final case class Update[S <: Sys[S], +Upd](element: Elem[S], change: Upd)

  //  type Expr[S <: Sys[S], A] = Elem[S] {
  //    type Peer[~ <: Sys[~]] = de.sciss.lucre.expr.Expr[~, A]
  //  }

  // ----------------- Serializer -----------------

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Elem[S]] = Impl.serializer[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S] = Impl.read(in, access)

  trait Extension {
    /** Unique type identifier */
    def typeID: scala.Int

    /** Read identified active element */
    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                      (implicit tx: S#Tx): Elem[S] with evt.Node[S]

    /** Read identified constant element */
    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Elem[S]
  }

  def registerExtension(ext: Extension): Unit = Impl.registerExtension(ext)
}
trait Elem[S <: Sys[S]]
  extends Writable with Disposable[S#Tx] /* Mutable[S#ID, S#Tx] */ with evt.Publisher[S, Elem.Update[S, Any]] { me =>

  def typeID: Int

  type Peer
  type PeerUpdate

  /** The actual object wrapped by the element. */
  val peer: Peer

  override def changed: EventLike[S, Elem.Update[S, PeerUpdate]]

  import me.{Peer => Peer0, PeerUpdate => PeerUpdate0}

  def mkCopy()(implicit tx: S#Tx): Elem[S] {
    type Peer       = Peer0
    type PeerUpdate = PeerUpdate0
  }
}

// ---- elements ----

object IntElem {
  def apply[S <: Sys[S]](peer: Expr[S, Int])(implicit tx: S#Tx): IntElem[S] =
    proc.impl.ElemImpl.Int(peer)

  object Obj {
    def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, IntElem]] =
      if (obj.elem.isInstanceOf[IntElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, IntElem]])
      else None
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, IntElem[S]] = Impl.Int.serializer[S]
}
trait IntElem[S <: Sys[S]] extends Elem[S] {
  type Peer       = Expr[S, Int]
  type PeerUpdate = model.Change[Int]
}

object DoubleElem {
  def apply[S <: Sys[S]](peer: Expr[S, Double])(implicit tx: S#Tx): DoubleElem[S] =
    proc.impl.ElemImpl.Double(peer)

  object Obj {
    def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, DoubleElem]] =
      if (obj.elem.isInstanceOf[DoubleElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, DoubleElem]])
      else None
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, DoubleElem[S]] = Impl.Double.serializer[S]
}
trait DoubleElem[S <: Sys[S]] extends Elem[S] {
  type Peer       = Expr[S, Double]
  type PeerUpdate = model.Change[Double]
}

object LongElem {
  def apply[S <: Sys[S]](peer: Expr[S, Long])(implicit tx: S#Tx): LongElem[S] =
    proc.impl.ElemImpl.Long(peer)

  object Obj {
    def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, LongElem]] =
      if (obj.elem.isInstanceOf[LongElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, LongElem]])
      else None
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, LongElem[S]] = Impl.Long.serializer[S]
}
trait LongElem[S <: Sys[S]] extends Elem[S] {
  type Peer       = Expr[S, Long]
  type PeerUpdate = model.Change[Long]
}

object BooleanElem {
  def apply[S <: Sys[S]](peer: Expr[S, Boolean])(implicit tx: S#Tx): BooleanElem[S] =
    proc.impl.ElemImpl.Boolean(peer)

  object Obj {
    def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, BooleanElem]] =
      if (obj.elem.isInstanceOf[BooleanElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, BooleanElem]])
      else None
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, BooleanElem[S]] = Impl.Boolean.serializer[S]
}
trait BooleanElem[S <: Sys[S]] extends Elem[S] {
  type Peer       = Expr[S, Boolean]
  type PeerUpdate = model.Change[Boolean]
}

object StringElem {
  def apply[S <: Sys[S]](peer: Expr[S, String])(implicit tx: S#Tx): StringElem[S] =
    proc.impl.ElemImpl.String(peer)

  object Obj {
    def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, StringElem]] =
      if (obj.elem.isInstanceOf[StringElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, StringElem]])
      else None
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, StringElem[S]] = Impl.String.serializer[S]
}
trait StringElem[S <: Sys[S]] extends Elem[S] {
  type Peer       = Expr[S, String]
  type PeerUpdate = model.Change[String]
}

object FadeSpecElem {
  def apply[S <: Sys[S]](peer: Expr[S, FadeSpec.Value])(implicit tx: S#Tx): FadeSpecElem[S] =
    proc.impl.ElemImpl.FadeSpec(peer)

  object Obj {
    def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, FadeSpecElem]] =
      if (obj.elem.isInstanceOf[FadeSpecElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, FadeSpecElem]])
      else None
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, FadeSpecElem[S]] = Impl.FadeSpec.serializer[S]
}
trait FadeSpecElem[S <: Sys[S]] extends Elem[S] {
  type Peer       = Expr[S, FadeSpec.Value]
  type PeerUpdate = model.Change[FadeSpec.Value]
} // FadeSpec.Elem[S]

object DoubleVecElem {
  def apply[S <: Sys[S]](peer: Expr[S, Vec[Double]])(implicit tx: S#Tx): DoubleVecElem[S] =
    proc.impl.ElemImpl.DoubleVec(peer)

  object Obj {
    def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, DoubleVecElem]] =
      if (obj.elem.isInstanceOf[DoubleVecElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, DoubleVecElem]])
      else None
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, DoubleVecElem[S]] = Impl.DoubleVec.serializer[S]
}
trait DoubleVecElem[S <: Sys[S]] extends Elem[S] {
  type Peer       = Expr[S, Vec[Double]]
  type PeerUpdate = model.Change[Vec[Double]]
}

object AudioGraphemeElem {
  def apply[S <: Sys[S]](peer: Grapheme.Elem.Audio[S])(implicit tx: S#Tx): AudioGraphemeElem[S] =
    proc.impl.ElemImpl.AudioGrapheme(peer)

  object Obj {
    def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, AudioGraphemeElem]] =
      if (obj.elem.isInstanceOf[AudioGraphemeElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, AudioGraphemeElem]])
      else None
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, AudioGraphemeElem[S]] = Impl.AudioGrapheme.serializer[S]
}
trait AudioGraphemeElem[S <: Sys[S]] extends Elem[S] {
  type Peer       = Grapheme.Elem.Audio[S]
  type PeerUpdate = model.Change[Grapheme.Value.Audio]
}

object ArtifactLocationElem {
  def apply[S <: Sys[S]](peer: Artifact.Location[S])(implicit tx: S#Tx): ArtifactLocationElem[S] =
    proc.impl.ElemImpl.ArtifactLocation(peer)

  object Obj {
    def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, ArtifactLocationElem]] =
      if (obj.elem.isInstanceOf[ArtifactLocationElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, ArtifactLocationElem]])
      else None
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ArtifactLocationElem[S]] = Impl.ArtifactLocation.serializer[S]
}
trait ArtifactLocationElem[S <: Sys[S]] extends Elem[S] {
  type Peer       = Artifact.Location[S]
  type PeerUpdate = Artifact.Location.Update[S]
}

object ProcGroupElem {
  def apply[S <: Sys[S]](peer: ProcGroup[S])(implicit tx: S#Tx): ProcGroupElem[S] =
    proc.impl.ElemImpl.ProcGroup(peer)

  object Obj {
    def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, ProcGroupElem]] =
      if (obj.elem.isInstanceOf[ProcGroupElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, ProcGroupElem]])
      else None
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ProcGroupElem[S]] = Impl.ProcGroup.serializer[S]
}
trait ProcGroupElem[S <: Sys[S]] extends Elem[S] {
  type Peer       = ProcGroup[S]
  type PeerUpdate = ProcGroup.Update[S]
}

object ProcElem {
  def apply[S <: Sys[S]](peer: Proc[S])(implicit tx: S#Tx): ProcElem[S] =
    proc.impl.ElemImpl.Proc(peer)

  object Obj {
    def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, ProcElem]] =
      if (obj.elem.isInstanceOf[ProcElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, ProcElem]])
      else None
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ProcElem[S]] = Impl.Proc.serializer[S]
}
trait ProcElem[S <: Sys[S]] extends Elem[S] {
  type Peer       = Proc[S]
  type PeerUpdate = Proc.Update[S]
}