/*
 *  FadeSpec.scala
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

import lucre.{event => evt}
import lucre.expr.Expr
import serial.{DataOutput, DataInput, ImmutableSerializer}
import de.sciss.lucre.event.{Pull, Targets}
import de.sciss.lucre.synth.expr.{Curves, Doubles, BiTypeImpl, Longs}
import de.sciss.synth.Curve.linear
import de.sciss.{model => m}

object FadeSpec {
  private final val COOKIE = 0x4664 // 'Fd'

  object Value {
    implicit object serializer extends ImmutableSerializer[Value] {
      def write(v: Value, out: DataOutput): Unit = {
        import v._
        out.writeShort(COOKIE)
        out.writeLong (numFrames)
        Curve.serializer.write(curve, out)
        out.writeFloat(floor)
      }

      def read(in: DataInput): Value = {
        val cookie = in.readShort()
        require(cookie == COOKIE, s"Unexpected cookie $cookie, expected $COOKIE")
        val numFrames = in.readLong()
        val curve     = Curve.serializer.read(in)
        val floor     = in.readFloat()
        Value(numFrames = numFrames, curve = curve, floor = floor)
      }
    }
  }
  final case class Value(numFrames: Long, curve: Curve = linear, floor: Float = 0f)

  object Elem extends BiTypeImpl[Value] {
    final val typeID = 14

    // 0 reserved for variables
    private final val elemCookie = 1

    //    def longType    : BiType[Long    ] = Longs
    //    def spanLikeType: BiType[SpanLike] = SpanLikes

    def readValue(in: DataInput): Value = Value.serializer.read(in)

    def writeValue(value: Value, out: DataOutput): Unit =
      Value.serializer.write(value, out)

    def apply[S <: evt.Sys[S]](numFrames: Expr[S, Long], shape: Expr[S, Curve], floor: Expr[S, Double])
                              (implicit tx: S#Tx): Elem[S] = {
      val targets = Targets.partial[S] // XXX TODO partial?
      new Impl(targets, numFrames, shape, floor)
    }

    def unapply[S <: evt.Sys[S]](expr: Elem[S]): Option[(Expr[S, Long], Expr[S, Curve], Expr[S, Double])] =
      expr match {
        case impl: Impl[S] => Some((impl.numFrames, impl.shape, impl.floor))
        case _ => None
      }

    protected def readTuple[S <: evt.Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                              (implicit tx: S#Tx): ReprNode[S] = {
      require(cookie == elemCookie, s"Unexpected cookie $cookie (requires $elemCookie)")
      val numFrames = Longs  .readExpr(in, access)
      val shape     = Curves .readExpr(in, access)
      val floor     = Doubles.readExpr(in, access)
      new Impl(targets, numFrames, shape, floor)
    }

    private final class Impl[S <: evt.Sys[S]](protected val targets: Targets[S],
                                              val numFrames: Expr[S, Long],
                                              val shape: Expr[S, Curve],
                                              val floor: Expr[S, Double])
      extends lucre.expr.impl.NodeImpl[S, Value] with Elem[S] {

      def value(implicit tx: S#Tx) = Value(numFrames.value, shape.value, floor.value.toFloat)

      protected def reader = Elem.serializer[S]

      def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[m.Change[Value]] = {
        val framesEvt = numFrames.changed
        val framesChO = if (pull.contains(framesEvt)) pull(framesEvt) else None
        val shapeEvt  = shape.changed
        val shapeChO  = if (pull.contains(shapeEvt )) pull(shapeEvt ) else None
        val floorEvt  = floor.changed
        val floorChO  = if (pull.contains(floorEvt )) pull(floorEvt ) else None

        if (framesChO.isEmpty && shapeChO.isEmpty && floorChO.isEmpty) return None

        val framesCh = framesChO.getOrElse {
          val framesV = numFrames.value
          m.Change(framesV, framesV)
        }

        val shapeCh = shapeChO.getOrElse {
          val shapeV = shape.value
          m.Change(shapeV, shapeV)
        }

        val floorCh = floorChO.getOrElse {
          val floorV = floor.value
          m.Change(floorV, floorV)
        }

        val before  = Value(framesCh.before, shapeCh.before, floorCh.before.toFloat)
        val now     = Value(framesCh.now,    shapeCh.now,    floorCh.now   .toFloat)

        Some(m.Change(before, now))
      }

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(elemCookie)
        numFrames.write(out)
        shape    .write(out)
        floor    .write(out)
      }

      def connect()(implicit tx: S#Tx): Unit = {
        numFrames.changed ---> this
        shape    .changed ---> this
        floor    .changed ---> this
      }

      def disconnect()(implicit tx: S#Tx): Unit = {
        numFrames.changed -/-> this
        shape    .changed -/-> this
        floor    .changed -/-> this
      }
    }
  }
  sealed trait Elem[S <: evt.Sys[S]] extends Expr[S, FadeSpec.Value]
}
