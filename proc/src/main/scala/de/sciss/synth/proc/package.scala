/*
 *  package.scala
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

package de.sciss.synth

import de.sciss.lucre.expr
import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.synth.expr.DoubleVec
import de.sciss.{lucre, model}
import de.sciss.serial.Serializer
import de.sciss.synth.proc.impl.ElemImpl

import annotation.elidable
import annotation.elidable.CONFIG
import de.sciss.lucre.event.Sys
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.higherKinds
import scala.language.existentials

package object proc {
  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'proc' - ", Locale.US)
  var showLog           = false
  var showAuralLog      = false
  var showTransportLog  = false

  @elidable(CONFIG) private[proc] def logAural(what: => String): Unit =
    if (showAuralLog) Console.out.println(logHeader.format(new Date()) + "aural " + what)

  @elidable(CONFIG) private[proc] def logTransport(what: => String): Unit =
    if (showTransportLog) Console.out.println(logHeader.format(new Date()) + "transport " + what)

  @elidable(CONFIG) private[proc] def log(what: => String): Unit =
    if (showLog) Console.out.println(logHeader.format(new Date()) + what)

  // ---- types ----

  type Folder[S <: Sys[S]] = expr.List.Modifiable[S, Obj[S], Obj.Update[S]]

  // ---- elements ----

  implicit object IntElem extends Elem.Companion[IntElem] {
    def typeID = lucre.expr.Int.typeID

    def apply[S <: Sys[S]](peer: Expr[S, Int])(implicit tx: S#Tx): IntElem[S] =
      proc.impl.ElemImpl.Int(peer)

    object Obj {
      def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, IntElem]] =
        if (obj.elem.isInstanceOf[IntElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, IntElem]])
        else None
    }

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, IntElem[S]] = ElemImpl.Int.serializer[S]
  }
  trait IntElem[S <: Sys[S]] extends Elem[S] {
    type Peer       = Expr[S, Int]
    type PeerUpdate = model.Change[Int]
  }

  implicit object DoubleElem extends Elem.Companion[DoubleElem] {
    def typeID = lucre.expr.Double.typeID

    def apply[S <: Sys[S]](peer: Expr[S, Double])(implicit tx: S#Tx): DoubleElem[S] =
      proc.impl.ElemImpl.Double(peer)

    object Obj {
      def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, DoubleElem]] =
        if (obj.elem.isInstanceOf[DoubleElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, DoubleElem]])
        else None
    }

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, DoubleElem[S]] = ElemImpl.Double.serializer[S]
  }
  trait DoubleElem[S <: Sys[S]] extends Elem[S] {
    type Peer       = Expr[S, Double]
    type PeerUpdate = model.Change[Double]
  }

  implicit object LongElem extends Elem.Companion[LongElem] {
    def typeID = lucre.expr.Long.typeID

    def apply[S <: Sys[S]](peer: Expr[S, Long])(implicit tx: S#Tx): LongElem[S] =
      proc.impl.ElemImpl.Long(peer)

    object Obj {
      def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, LongElem]] =
        if (obj.elem.isInstanceOf[LongElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, LongElem]])
        else None
    }

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, LongElem[S]] = ElemImpl.Long.serializer[S]
  }
  trait LongElem[S <: Sys[S]] extends Elem[S] {
    type Peer       = Expr[S, Long]
    type PeerUpdate = model.Change[Long]
  }

  implicit object BooleanElem extends Elem.Companion[BooleanElem] {
    def typeID = lucre.expr.Boolean.typeID

    def apply[S <: Sys[S]](peer: Expr[S, Boolean])(implicit tx: S#Tx): BooleanElem[S] =
      proc.impl.ElemImpl.Boolean(peer)

    object Obj {
      def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, BooleanElem]] =
        if (obj.elem.isInstanceOf[BooleanElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, BooleanElem]])
        else None
    }

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, BooleanElem[S]] = ElemImpl.Boolean.serializer[S]
  }
  trait BooleanElem[S <: Sys[S]] extends Elem[S] {
    type Peer       = Expr[S, Boolean]
    type PeerUpdate = model.Change[Boolean]
  }

  implicit object StringElem extends Elem.Companion[StringElem] {
    def typeID = lucre.expr.String.typeID

    def apply[S <: Sys[S]](peer: Expr[S, String])(implicit tx: S#Tx): StringElem[S] =
      proc.impl.ElemImpl.String(peer)

    object Obj {
      def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, StringElem]] =
        if (obj.elem.isInstanceOf[StringElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, StringElem]])
        else None
    }

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, StringElem[S]] = ElemImpl.String.serializer[S]
  }
  trait StringElem[S <: Sys[S]] extends Elem[S] {
    type Peer       = Expr[S, String]
    type PeerUpdate = model.Change[String]
  }

  implicit object DoubleVecElem extends Elem.Companion[DoubleVecElem] {
    def typeID = DoubleVec.typeID

    def apply[S <: Sys[S]](peer: Expr[S, Vec[Double]])(implicit tx: S#Tx): DoubleVecElem[S] =
      proc.impl.ElemImpl.DoubleVec(peer)

    object Obj {
      def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, DoubleVecElem]] =
        if (obj.elem.isInstanceOf[DoubleVecElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, DoubleVecElem]])
        else None
    }

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, DoubleVecElem[S]] = ElemImpl.DoubleVec.serializer[S]
  }
  trait DoubleVecElem[S <: Sys[S]] extends Elem[S] {
    type Peer       = Expr[S, Vec[Double]]
    type PeerUpdate = model.Change[Vec[Double]]
  }

  implicit object AudioGraphemeElem extends Elem.Companion[AudioGraphemeElem] {
    def typeID = Grapheme.Expr.Audio.typeID

    def apply[S <: Sys[S]](peer: Grapheme.Expr.Audio[S])(implicit tx: S#Tx): AudioGraphemeElem[S] =
      proc.impl.ElemImpl.AudioGrapheme(peer)

    object Obj {
      def unapply[S <: Sys[S]](obj: Obj[S]): Option[proc.Obj.T[S, AudioGraphemeElem]] =
        if (obj.elem.isInstanceOf[AudioGraphemeElem[S]]) Some(obj.asInstanceOf[proc.Obj.T[S, AudioGraphemeElem]])
        else None
    }

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, AudioGraphemeElem[S]] = ElemImpl.AudioGrapheme.serializer[S]
  }
  trait AudioGraphemeElem[S <: Sys[S]] extends Elem[S] {
    type Peer       = Grapheme.Expr.Audio[S]
    type PeerUpdate = model.Change[Grapheme.Value.Audio]
  }
}