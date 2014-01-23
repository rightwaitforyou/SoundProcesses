package de.sciss
package synth
package proc

import lucre.{event => evt, stm}
import lucre.expr.Expr
import stm.Mutable
import de.sciss.lucre.event.Publisher
import language.{higherKinds, implicitConversions}
import proc.impl.{AttributeImpl => Impl}

object Attribute {
  import scala.{Int => _Int, Double => _Double, Boolean => _Boolean}
  import java.lang.{String => _String}
  import proc.{FadeSpec => _FadeSpec}

  final case class Update[S <: evt.Sys[S]](element: Attribute[S], change: Any)

  // ----------------- Int -----------------

  object Int {
    def apply[S <: evt.Sys[S]](peer: Expr[S, _Int])(implicit tx: S#Tx): Int[S] = Impl.Int(peer)

    implicit def serializer[S <: evt.Sys[S]]: serial.Serializer[S#Tx, S#Acc, Int[S]] = Impl.Int.serializer[S]
  }
  trait Int[S <: evt.Sys[S]] extends Attribute[S] {
    type Peer = Expr[S, _Int]
    def mkCopy()(implicit tx: S#Tx): Int[S]
  }

  // ----------------- Double -----------------

  object Double {
    def apply[S <: evt.Sys[S]](peer: Expr[S, _Double])(implicit tx: S#Tx): Double[S] = Impl.Double(peer)

    implicit def serializer[S <: evt.Sys[S]]: serial.Serializer[S#Tx, S#Acc, Double[S]] = Impl.Double.serializer[S]
  }
  trait Double[S <: evt.Sys[S]] extends Attribute[S] {
    type Peer = Expr[S, _Double]
    def mkCopy()(implicit tx: S#Tx): Double[S]
  }

  // ----------------- Boolean -----------------

  object Boolean {
    def apply[S <: evt.Sys[S]](peer: Expr[S, _Boolean])(implicit tx: S#Tx): Boolean[S] = Impl.Boolean(peer)

    implicit def serializer[S <: evt.Sys[S]]: serial.Serializer[S#Tx, S#Acc, Boolean[S]] = Impl.Boolean.serializer[S]
  }
  trait Boolean[S <: evt.Sys[S]] extends Attribute[S] {
    type Peer = Expr[S, _Boolean]
    def mkCopy()(implicit tx: S#Tx): Boolean[S]
  }

  // ----------------- String -----------------

  object String {
    def apply[S <: evt.Sys[S]](peer: Expr[S, _String])(implicit tx: S#Tx): String[S] = Impl.String(peer)

    implicit def serializer[S <: evt.Sys[S]]: serial.Serializer[S#Tx, S#Acc, String[S]] = Impl.String.serializer[S]
  }
  trait String[S <: evt.Sys[S]] extends Attribute[S] {
    type Peer = Expr[S, _String]
    def mkCopy()(implicit tx: S#Tx): String[S]
  }

  // ----------------- FadeSpec -----------------

  object FadeSpec {
    def apply[S <: evt.Sys[S]](peer: Expr[S, _FadeSpec.Value])(implicit tx: S#Tx): FadeSpec[S] = Impl.FadeSpec(peer)

    implicit def serializer[S <: evt.Sys[S]]: serial.Serializer[S#Tx, S#Acc, FadeSpec[S]] = Impl.FadeSpec.serializer[S]
  }
  trait FadeSpec[S <: evt.Sys[S]] extends Attribute[S] {
    type Peer = Expr[S, _FadeSpec.Value]
    def mkCopy()(implicit tx: S#Tx): FadeSpec[S]
  }

  // ----------------- Serializer -----------------

  implicit def serializer[S <: evt.Sys[S]]: evt.Serializer[S, Attribute[S]] = Impl.serializer[S]
}
trait Attribute[S <: evt.Sys[S]] extends Mutable[S#ID, S#Tx] with Publisher[S, Attribute.Update[S]] {
  type Peer

  /** The actual object wrapped by the element. */
  def peer: Peer

  def mkCopy()(implicit tx: S#Tx): Attribute[S]
}