/*
 *  BiExpr.scala
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

package de.sciss.lucre
package bitemp

import de.sciss.lucre.expr.{ExprType1, Expr}
import de.sciss.lucre.{event => evt}
import evt.Targets
import de.sciss.serial.{DataOutput, DataInput}
import language.implicitConversions
import de.sciss.model.Change

object BiExpr {
  def apply[S <: evt.Sys[S], A](time: Expr[S, Long], mag: Expr[S, A])
                               (implicit tx: S#Tx, magType: ExprType1[A]): BiExpr[S, A] = (time, mag) match {
    case (Expr.Const(timeVal), Expr.Const(magVal)) =>
      ConstImpl(timeVal, magVal)
    case _ =>
      val targets = evt.Targets.partial[S] // XXX TODO partial?
      new EventImpl[S, A](targets, time, mag)
  }

  def unapply[S <: evt.Sys[S], A](expr: BiExpr[S, A]): Option[(Expr[S, Long], Expr[S, A])] =
    Some((expr.time, expr.mag))

  //   implicit def fromTuple[ S <: evt.Sys[ S ], A, A1, T ]( tuple: (T, A1) )
  //                                                        ( implicit tx: S#Tx, magType: ExprType[ A ],
  //                                                          timeView: T => Expr[ S, Long ],
  //                                                          magView: A1 => Expr[ S, A ]) : BiExpr[ S, A ] =
  //      apply[ S, A ]( timeView( tuple._1 ), magView( tuple._2 ))

  //  implicit def fromTuple[S <: evt.Sys[S], T, A](tuple: (T, A))(implicit tx: S#Tx, timeView: T => Expr[S, Long],
  //    magView: A => Expr[S, A], magType: ExprType[A]): BiExpr[S, A] = apply(tuple._1, tuple._2)

  implicit def serializer[S <: evt.Sys[S], A](implicit magType: ExprType1[A]): evt.Serializer[S, BiExpr[S, A]] =
    new Ser

  private final class Ser[S <: evt.Sys[S], A](implicit magType: ExprType1[A])
    extends evt.EventLikeSerializer[S, BiExpr[S, A]] {

    def readConstant(in: DataInput)(implicit tx: S#Tx): BiExpr[S, A] = {
      val timeVal = in.readLong()
      val magVal = magType.readValue(in)
      ConstImpl(timeVal, magVal)
    }

    def read(in: DataInput, access: S#Acc, targets: Targets[S])
            (implicit tx: S#Tx): BiExpr[S, A] with evt.Node[S] = {
      val time  = expr.Long.read(in, access)
      val mag   = magType  .read(in, access)
      new EventImpl[S, A](targets, time, mag)
    }
  }

  private final class EventImpl[S <: evt.Sys[S], A](protected val targets: evt.Targets[S],
                                                    val time: Expr[S, Long], val mag: Expr[S, A])
                                                   (implicit magType: ExprType1[A])
    extends BiExpr[S, A] with expr.impl.NodeImpl[S, (Long, A)] {
    override def toString() = "(" + time + " -> " + mag + ")"

    def value    (implicit tx: S#Tx): (Long, A) = timeValue -> magValue
    def timeValue(implicit tx: S#Tx)            = time.value
    def magValue (implicit tx: S#Tx)            = mag.value

    protected def writeData(out: DataOutput): Unit = {
      time.write(out)
      mag.write(out)
    }

    def connect()(implicit tx: S#Tx): Unit = {
      time.changed ---> this
      mag .changed ---> this
    }

    def disconnect()(implicit tx: S#Tx): Unit = {
      time.changed -/-> this
      mag .changed -/-> this
    }

    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Change[(Long, A)]] = {
      val timeEvt = time.changed
      val timeCh  = if (pull.contains(timeEvt)) pull(timeEvt) else None
      val magEvt  = mag.changed
      val magCh   = if (pull.contains(magEvt )) pull(magEvt ) else None

      (timeCh, magCh) match {
        case (Some(tch), Some(mch)) =>
          Some(tch zip mch)
        case (Some(tch), None) =>
          val mv = magValue
          Some(Change(tch.before -> mv, tch.now -> mv))
        case (None, Some(mch)) =>
          val tv = timeValue
          Some(Change(tv -> mch.before, tv -> mch.now))
        case (None, None) =>
          None
      }
    }

    protected def reader: evt.Reader[S, Expr[S, (Long, A)]] = serializer
  }

  private final case class ConstImpl[S <: evt.Sys[S], A](timeVal: Long, magVal: A)(implicit magType: ExprType1[A])
    extends BiExpr[S, A] with expr.impl.ConstImpl[S, (Long, A)] {

    protected def constValue: (Long, A) = timeVal -> magVal

    def timeValue(implicit tx: S#Tx) = timeVal
    def magValue (implicit tx: S#Tx) = magVal

    protected def writeData(out: DataOutput): Unit = {
      out    .writeLong (timeVal)
      magType.writeValue(magVal, out)
    }

    def time: Expr[S, Long] = expr.Long.newConst(timeVal)
    def mag : Expr[S, A   ] = magType  .newConst(magVal )
  }
}

trait BiExpr[S <: evt.Sys[S], A] extends Expr[S, (Long, A)] {
  def time: Expr[S, Long]
  def mag : Expr[S, A   ]

  def timeValue(implicit tx: S#Tx): Long
  def magValue (implicit tx: S#Tx): A
}