/*
 *  Attr.scala
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

import lucre.{event => evt, stm}
import lucre.expr.Expr
import evt.Sys
import de.sciss.lucre.stm.{Disposable, Mutable}
import de.sciss.lucre.event.Publisher
import proc.impl.{AttrImpl => Impl}
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.{higherKinds, implicitConversions}
import de.sciss.serial.{DataInput, Writable}

object Attr {
  import scala.{Int => _Int, Double => _Double, Boolean => _Boolean, Long => _Long}
  import java.lang.{String => _String}
  import proc.{FadeSpec => _FadeSpec, Artifact => _Artifact}

  final case class Update[S <: Sys[S]](element: Attr[S], change: Any)

  // ----------------- Int -----------------

  object Int {
    def apply[S <: Sys[S]](peer: Expr[S, _Int])(implicit tx: S#Tx): Int[S] = Impl.Int(peer)

    implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Int[S]] = Impl.Int.serializer[S]
  }
  trait Int[S <: Sys[S]] extends Attr[S] {
    type Peer = Expr[S, _Int]
    def mkCopy()(implicit tx: S#Tx): Int[S]
  }

  // ----------------- Long -----------------

  object Long {
    def apply[S <: Sys[S]](peer: Expr[S, _Long])(implicit tx: S#Tx): Long[S] = Impl.Long(peer)

    implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Long[S]] = Impl.Long.serializer[S]
  }
  trait Long[S <: Sys[S]] extends Attr[S] {
    type Peer = Expr[S, _Long]
    def mkCopy()(implicit tx: S#Tx): Long[S]
  }

  // ----------------- Double -----------------

  object Double {
    def apply[S <: Sys[S]](peer: Expr[S, _Double])(implicit tx: S#Tx): Double[S] = Impl.Double(peer)

    implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Double[S]] = Impl.Double.serializer[S]
  }
  trait Double[S <: Sys[S]] extends Attr[S] {
    type Peer = Expr[S, _Double]
    def mkCopy()(implicit tx: S#Tx): Double[S]
  }

  // ----------------- Boolean -----------------

  object Boolean {
    def apply[S <: Sys[S]](peer: Expr[S, _Boolean])(implicit tx: S#Tx): Boolean[S] = Impl.Boolean(peer)

    implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Boolean[S]] = Impl.Boolean.serializer[S]
  }
  trait Boolean[S <: Sys[S]] extends Attr[S] {
    type Peer = Expr[S, _Boolean]
    def mkCopy()(implicit tx: S#Tx): Boolean[S]
  }

  // ----------------- String -----------------

  object String {
    def apply[S <: Sys[S]](peer: Expr[S, _String])(implicit tx: S#Tx): String[S] = Impl.String(peer)

    implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, String[S]] = Impl.String.serializer[S]
  }
  trait String[S <: Sys[S]] extends Attr[S] {
    type Peer = Expr[S, _String]
    def mkCopy()(implicit tx: S#Tx): String[S]
  }

  // ----------------- FadeSpec -----------------

  object FadeSpec {
    def apply[S <: Sys[S]](peer: Expr[S, _FadeSpec.Value])(implicit tx: S#Tx): FadeSpec[S] = Impl.FadeSpec(peer)

    implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, FadeSpec[S]] = Impl.FadeSpec.serializer[S]
  }
  trait FadeSpec[S <: Sys[S]] extends Attr[S] {
    type Peer = Expr[S, _FadeSpec.Value]
    def mkCopy()(implicit tx: S#Tx): FadeSpec[S]
  }

  // ----------------- DoubleVec -----------------

  object DoubleVec {
    def apply[S <: Sys[S]](peer: Expr[S, Vec[_Double]])(implicit tx: S#Tx): DoubleVec[S] = Impl.DoubleVec(peer)

    implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, DoubleVec[S]] =
      Impl.DoubleVec.serializer[S]
  }
  trait DoubleVec[S <: Sys[S]] extends Attr[S] {
    type Peer = Expr[S, Vec[_Double]]
    def mkCopy()(implicit tx: S#Tx): DoubleVec[S]
  }

  // ----------------- AudioGrapheme -----------------

  object AudioGrapheme {
    def apply[S <: Sys[S]](peer: Grapheme.Elem.Audio[S])(implicit tx: S#Tx): AudioGrapheme[S] =
      Impl.AudioGrapheme(peer)

    implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, AudioGrapheme[S]] =
      Impl.AudioGrapheme.serializer[S]
  }
  trait AudioGrapheme[S <: Sys[S]] extends Attr[S] {
    type Peer = Grapheme.Elem.Audio[S]
    def mkCopy()(implicit tx: S#Tx): AudioGrapheme[S]
  }

  // ----------------- ArtifactLocation -----------------

  object ArtifactLocation {
    def apply[S <: Sys[S]](peer: _Artifact.Location[S])(implicit tx: S#Tx): ArtifactLocation[S] =
      Impl.ArtifactLocation(peer)

    implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, ArtifactLocation[S]] =
      Impl.ArtifactLocation.serializer[S]
  }
  trait ArtifactLocation[S <: Sys[S]] extends Attr[S] {
    type Peer = _Artifact.Location[S]
    def mkCopy()(implicit tx: S#Tx): ArtifactLocation[S]
  }

  // ----------------- Serializer -----------------

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Attr[S]] = Impl.serializer[S]

  trait Extension {
    /** Unique type identifier */
    def typeID: scala.Int

    /** Read identified active attribute */
    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                      (implicit tx: S#Tx): Attr[S] with evt.Node[S]

    /** Read identified constant attribute */
    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Attr[S]
  }

  def registerExtension(ext: Extension): Unit = Impl.registerExtension(ext)
}
trait Attr[S <: Sys[S]]
  extends Writable with Disposable[S#Tx] /* Mutable[S#ID, S#Tx] */ with Publisher[S, Attr.Update[S]] {

  type Peer

  /** The actual object wrapped by the element. */
  def peer: Peer

  def mkCopy()(implicit tx: S#Tx): Attr[S]
}