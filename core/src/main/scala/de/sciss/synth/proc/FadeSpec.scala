/*
 *  FadeSpec.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.lucre.event.{Pull, Targets}
import de.sciss.lucre.expr.{Expr => _Expr}
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.{event => evt, expr}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.synth.Curve.linear
import de.sciss.{model => m}

object FadeSpec extends Obj.Type {
  final val typeID = 14

  override def init(): Unit = {
    super.init()
    Expr .init()
  }

  private final val COOKIE = 0x4664 // 'Fd'

  implicit object serializer extends ImmutableSerializer[FadeSpec] {
    def write(v: FadeSpec, out: DataOutput): Unit = {
      import v._
      out.writeShort(COOKIE)
      out.writeLong (numFrames)
      Curve.serializer.write(curve, out)
      out.writeFloat(floor)
    }

    def read(in: DataInput): FadeSpec = {
      val cookie = in.readShort()
      require(cookie == COOKIE, s"Unexpected cookie $cookie, expected $COOKIE")
      val numFrames = in.readLong()
      val curve     = Curve.serializer.read(in)
      val floor     = in.readFloat()
      FadeSpec(numFrames = numFrames, curve = curve, floor = floor)
    }
  }

  object Expr extends expr.impl.ExprTypeImpl[FadeSpec] {
    def typeID = FadeSpec.typeID

    // 0 reserved for variables
    private final val elemCookie = 1

    //    def longType    : BiType[Long    ] = Longs
    //    def spanLikeType: BiType[SpanLike] = SpanLikes

    def valueSerializer: ImmutableSerializer[FadeSpec] = FadeSpec.serializer

//    def readValue (                 in : DataInput ): FadeSpec  = FadeSpec.serializer.read (       in )
//    def writeValue(value: FadeSpec, out: DataOutput): Unit      = FadeSpec.serializer.write(value, out)

    def apply[S <: Sys[S]](numFrames: _Expr[S, Long], shape: _Expr[S, Curve], floor: _Expr[S, Double])
                              (implicit tx: S#Tx): Expr[S] = {
      val targets = Targets[S]
      new Impl(targets, numFrames, shape, floor).connect()
    }

    def unapply[S <: Sys[S]](expr: Expr[S]): Option[(_Expr[S, Long], _Expr[S, Curve], _Expr[S, Double])] =
      expr match {
        case impl: Impl[S] => Some((impl.numFrames, impl.shape, impl.floor))
        case _ => None
      }

//    // XXX TODO: not cool. Should use `1` for `elemCookie`
//    override protected def readNode[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
//                                              (implicit tx: S#Tx): Ex[S] with evt.Node[S] = {
//      require(cookie == elemCookie, s"Unexpected cookie $cookie (requires $elemCookie)")
//      val numFrames = lucre      .expr.Long  .read(in, access)
//      val shape     = .... : _Expr[S, Curve] // RRR lucre.synth.expr.Curve .read(in, access)
//      val floor     = lucre      .expr.Double.read(in, access)
//      new Impl(targets, numFrames, shape, floor)
//    }

    private final class Impl[S <: Sys[S]](protected val targets: Targets[S],
                                              val numFrames: _Expr[S, Long],
                                              val shape: _Expr[S, Curve],
                                              val floor: _Expr[S, Double])
      extends lucre.expr.impl.NodeImpl[S, FadeSpec] with Expr[S] {

      def tpe: Obj.Type = FadeSpec

      def value(implicit tx: S#Tx): FadeSpec = FadeSpec(numFrames.value, shape.value, floor.value.toFloat)

      object changed extends Changed {
        def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[m.Change[FadeSpec]] = {
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

          val before  = FadeSpec(framesCh.before, shapeCh.before, floorCh.before.toFloat)
          val now     = FadeSpec(framesCh.now,    shapeCh.now,    floorCh.now   .toFloat)

          Some(m.Change(before, now))
        }
      }

      protected def disposeData()(implicit tx: S#Tx): Unit = disconnect()

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(elemCookie)
        numFrames.write(out)
        shape    .write(out)
        floor    .write(out)
      }

      def connect()(implicit tx: S#Tx): this.type = {
        numFrames.changed ---> changed
        shape    .changed ---> changed
        floor    .changed ---> changed
        this
      }

      private[this] def disconnect()(implicit tx: S#Tx): Unit = {
        numFrames.changed -/-> changed
        shape    .changed -/-> changed
        floor    .changed -/-> changed
      }
    }
  }
  sealed trait Expr[S <: Sys[S]] extends _Expr[S, FadeSpec]

  override def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Expr.readIdentifiedObj(in, access)
}
final case class FadeSpec(numFrames: Long, curve: Curve = linear, floor: Float = 0f)
