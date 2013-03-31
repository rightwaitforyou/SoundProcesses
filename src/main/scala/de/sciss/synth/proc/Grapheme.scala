/*
 *  Grapheme.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package proc

import de.sciss.lucre.{event => evt, io, bitemp, expr}
import impl.CommonSerializers
import expr.Expr
import bitemp.{BiType, BiExpr}
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.synth.expr.{Doubles, SpanLikes, Longs}
import annotation.switch

import impl.{GraphemeImpl => Impl}
import de.sciss.synth.io.AudioFileSpec
import de.sciss.lucre.event.{Targets, Event}
import io.{Writable, DataInput, DataOutput, ImmutableSerializer}
import de.sciss.span.{SpanLike, Span}

object Grapheme {
  // If necessary for some views, we could eventually add the Elems, too,
  // like `changes: IIdxSeq[ (Elem[ S ], Value) ]`. Then the question would be
  // if Elem should have an id method? I.e. we'll have `add( elem: Elem[ S ]) : StoredElem[ S ]`
  // where `trait StoredElem[ S <: evt.Sys[ S ]] { def elem: Elem[ S ]; def id: S#ID }`?
  final case class Update[S <: evt.Sys[S]](grapheme: Grapheme[S], changes: IIdxSeq[Segment])

  implicit def serializer[S <: evt.Sys[S]]: io.Serializer[S#Tx, S#Acc, Grapheme[S]] = Impl.serializer[S]

  // 0 reserved for variables
  private final val curveCookie = 1
  private final val audioCookie = 2

  object Value {
    implicit val biType: BiType[Value] = Elem

    implicit object Serializer extends ImmutableSerializer[Value] {
      def write(v: Value, out: DataOutput) {
        v.write(out)
      }

      def read(in: DataInput): Value = {
        (in.readByte().toInt: @switch) match {
          case `curveCookie`  => Curve.readIdentified(in)
          case `audioCookie`  => Audio.readIdentified(in)
          case cookie         => sys.error("Unexpected cookie " + cookie)
        }
      }
    }

    object Curve {
      implicit object Serializer extends ImmutableSerializer[Curve] {
        def write(v: Curve, out: DataOutput) { v.write(out) }
        def read(in: DataInput): Curve = {
          val cookie = in.readByte()
          require(cookie == `curveCookie`, s"Unexpected cookie $cookie")
          readIdentified(in)
        }
      }
      private[Value] def readIdentified(in: DataInput): Curve = {
        val sz      = in.readInt()
        val values  = IIdxSeq.fill(sz) {
          val mag = in.readDouble()
          val env = CommonSerializers.EnvConstShape.read(in)
          (mag, env)
        }
        Curve(values: _*)
      }
    }
    /**
     * A mono- or polyphonic constant value
     */
    final case class Curve(values: (Double, Env.ConstShape)*) extends Value {
      def numChannels = values.size

      def write(out: DataOutput) {
        out.writeByte(curveCookie)
        val sz = values.size
        out.writeInt(sz)
        values.foreach { case (mag, shape) =>
          out.writeDouble(mag)
          CommonSerializers.EnvConstShape.write(shape, out)
        }
      }
    }

    //      /**
    //       * A mono- or polyphonic envelope segment
    //       *
    //       * @param span    the span value covered by this segment
    //       * @param values  a sequence of tuples, each consisting of the value at start of the segment,
    //       *                the target value of the segment, and the shape of the segment
    //       */
    //      final case class Segment( span: Span.HasStart, values: (Double, Double, Env.ConstShape)* ) extends Value {
    //         def numChannels = values.size
    //
    //         def from( start: Long ) : Segment = {
    //            val newSpan = span.intersect( Span.from( start )).nonEmptyOption.getOrElse {
    //               throw new IllegalArgumentException(
    //                  "Segment.from - start position " + start + " lies outside of span " + span )
    //            }
    //            val newValues  = span match {
    //               case Span( oldStart, oldStop) =>
    //                  val pos = (start - oldStart).toDouble / (oldStop - oldStart)
    //                  values.map { case (oldStartVal, stopVal, shape) =>
    //                     val newStartVal = shape.levelAt( pos.toFloat, oldStartVal.toFloat, stopVal.toFloat).toDouble
    //                     (newStartVal, stopVal, shape)
    //                  }
    //
    //               case _ => values // either of start or stop is infinite, therefore interpolation does not make sense
    //            }
    //            Segment( newSpan, newValues: _* )
    //         }
    //      }

    object Audio {
      implicit object Serializer extends ImmutableSerializer[Audio] {
        def write(v: Audio, out: DataOutput) { v.write(out) }
        def read(in: DataInput): Audio = {
          val cookie = in.readByte()
          require(cookie == `audioCookie`, s"Unexpected cookie $cookie")
          readIdentified(in)
        }
      }
      private[Value] def readIdentified(in: DataInput): Audio = {
        val artifact  = Artifact.read(in)
        val spec      = CommonSerializers.AudioFileSpec.read(in)
        val offset    = in.readLong()
        val gain      = in.readDouble()
        Audio(artifact, spec, offset, gain)
      }
    }
    final case class Audio(artifact: Artifact, spec: AudioFileSpec, offset: Long, gain: Double)
      extends Value {
      def numChannels = spec.numChannels

      def write(out: DataOutput) {
        out.writeByte(audioCookie)
        artifact.write(out)
        CommonSerializers.AudioFileSpec.write(spec, out)
        out.writeLong(offset)
        out.writeDouble(gain)
      }
    }
  }

  /**
   * An evaluated and flattened scan element. This is either an immutable value such as a constant or
   * envelope segment, or a real-time signal, coming either from the same process (`Source`) or being
   * fed by another embedded process (`Sink`).
   */
  sealed trait Value extends Writable {
    def numChannels: Int

    // def span: Span.HasStart
  }

  object Segment {

    sealed trait Defined extends Segment {
      def numChannels: Int
      final def isDefined = true
    }
    final case class Const(span: Span.HasStart, values: IIdxSeq[Double]) extends Defined {
      def numChannels = values.size
    }
    final case class Curve(span: Span, values: IIdxSeq[(Double, Double, Env.ConstShape)]) extends Defined {
      def numChannels = values.size
    }
    final case class Audio(span: Span.HasStart, value: Value.Audio) extends Defined {
      def numChannels = value.numChannels
    }
    final case class Undefined(span: Span.HasStart) extends Segment {
      def isDefined = false
    }
  }

  sealed trait Segment {
    def span: Span.HasStart
    def isDefined: Boolean
  }

  object Elem extends BiType[Value] {
    final val typeID = 11

    object Curve extends expr.Type[Value.Curve] {
      def apply[S <: evt.Sys[S]](values: (Expr[S, Double], Env.ConstShape)*)(implicit tx: S#Tx): Elem[S] = {
        val targets = evt.Targets.partial[S] // XXX TODO partial?
        new CurveImpl(targets, values.toIndexedSeq)
      }

      def unapplySeq[S <: evt.Sys[S]](expr: Elem[S]): Option[Seq[(Expr[S, Double], Env.ConstShape)]] = {
        if (expr.isInstanceOf[CurveImpl[_]]) {
          val c = expr.asInstanceOf[CurveImpl[S]]
          Some(c.values)
        } else {
          None
        }
      }

      def readValue(in: DataInput): Value.Curve       = Value.Curve.Serializer.read(in)
      def writeValue(v: Value.Curve, out: DataOutput) { Value.Curve.Serializer.write(v, out) }

      protected def readTuple[S <: evt.Sys[S]](cookie: Int, in: DataInput, access: S#Acc,
                                               targets: Targets[S])(implicit tx: S#Tx): Curve[S] with evt.Node[S] = {
        require(cookie == curveCookie, s"Unexpected cookie $cookie")
        readIdentifiedTuple(in, access, targets)
      }

      private[Grapheme] def readIdentifiedTuple[S <: evt.Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                                (implicit tx: S#Tx): Curve[S] with evt.Node[S] = {
        val sz      = in.readInt()
        val values  = IIdxSeq.fill(sz) {
          val mag   = Doubles.readExpr(in, access)
          val shape = CommonSerializers.EnvConstShape.read(in)
          (mag, shape)
        }
        new CurveImpl(targets, values)
      }
    }
    sealed trait Curve[S <: evt.Sys[S]] extends Elem[S] with Expr[S, Value.Curve]

    object Audio extends expr.Type[Value.Audio]{
      def apply[S <: evt.Sys[S]](artifact: Artifact, spec: AudioFileSpec, offset: Expr[S, Long], gain: Expr[S, Double])
                                (implicit tx: S#Tx): Elem[S] = {
        val targets = evt.Targets.partial[S] // XXX TODO partial?
        new AudioImpl(targets, artifact, spec, offset, gain)
      }

      def unapply[S <: evt.Sys[S]](expr: Elem[S]): Option[(Artifact, AudioFileSpec, Expr[S, Long], Expr[S, Double])] = {
        if (expr.isInstanceOf[AudioImpl[_]]) {
          val a = expr.asInstanceOf[AudioImpl[S]]
          Some((a.artifact, a.spec, a.offset, a.gain))
        } else {
          None
        }
      }

      def readValue(in: DataInput): Value.Audio       = Value.Audio.Serializer.read(in)
      def writeValue(v: Value.Audio, out: DataOutput) { Value.Audio.Serializer.write(v, out) }

      protected def readTuple[S <: evt.Sys[S]](cookie: Int, in: DataInput, access: S#Acc,
                                               targets: Targets[S])(implicit tx: S#Tx): Audio[S] with evt.Node[S] = {
        require(cookie == audioCookie, s"Unexpected cookie $cookie")
        readIdentifiedTuple(in, access, targets)
      }

      private[Grapheme] def readIdentifiedTuple[S <: evt.Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                                (implicit tx: S#Tx): Audio[S] with evt.Node[S] = {
        val artifact  = Artifact.read(in)
        val spec      = CommonSerializers.AudioFileSpec.read(in)
        val offset    = Longs.readExpr(in, access)
        val gain      = Doubles.readExpr(in, access)
        new AudioImpl(targets, artifact, spec, offset, gain)
      }
    }
    sealed trait Audio[S <: evt.Sys[S]] extends Elem[S] with Expr[S, Value.Audio]

    private final class CurveImpl[S <: evt.Sys[S]](protected val targets: evt.Targets[S],
                                                   val values: IIdxSeq[(Expr[S, Double], Env.ConstShape)])
      extends expr.impl.NodeImpl[S, Value.Curve] with Curve[S] {
      def value(implicit tx: S#Tx): Value.Curve = {
        val v = values.map {
          case (mag, shape) => mag.value -> shape
        }
        Value.Curve(v: _*)
      }

      def connect()(implicit tx: S#Tx) {
        values.foreach { tup =>
          tup._1.changed ---> this
        }
      }

      def disconnect()(implicit tx: S#Tx) {
        values.foreach { tup =>
          tup._1.changed -/-> this
        }
      }

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[evt.Change[Value.Curve]] = {
        val beforeVals  = Vector.newBuilder[(Double, Env.ConstShape)]
        val nowVals     = Vector.newBuilder[(Double, Env.ConstShape)]
        values.foreach {
          case (mag, shape) =>
            val magEvt = mag.changed
            if (magEvt.isSource(pull)) {
              magEvt.pullUpdate(pull) match {
                case Some(evt.Change(magBefore, magNow)) =>
                  beforeVals += magBefore -> shape
                  nowVals    += magNow    -> shape
                case _ =>
                  val mv      = mag.value
                  beforeVals += mv -> shape
                  nowVals    += mv -> shape
              }
            } else {
              val mv      = mag.value
              beforeVals += mv -> shape
              nowVals    += mv -> shape
            }
        }
        Curve.change(Value.Curve(beforeVals.result(): _*), Value.Curve(nowVals.result(): _*))
      }

      protected def reader = Curve.serializer[S]

      protected def writeData( out: DataOutput ) {
        out.writeByte(curveCookie)
        val sz = values.size
        out.writeInt(sz)
        values.foreach { tup =>
          tup._1.write(out)
          CommonSerializers.EnvConstShape.write(tup._2, out)
        }
      }

      override def toString() = "Elem.Curve" + id
    }

    private final class AudioImpl[S <: evt.Sys[S]](protected val targets: evt.Targets[S], val artifact: Artifact,
                                                   val spec: AudioFileSpec, val offset: Expr[S, Long],
                                                   val gain: Expr[S, Double])
      extends expr.impl.NodeImpl[S, Value.Audio] with Audio[S] {
      def value(implicit tx: S#Tx): Value.Audio = {
        val offsetVal = offset.value
        val gainVal   = gain.value
        Value.Audio(artifact, spec, offsetVal, gainVal)
      }

      def connect()(implicit tx: S#Tx) {
        offset.changed ---> this
        gain  .changed ---> this
      }

      def disconnect()(implicit tx: S#Tx) {
        offset.changed -/-> this
        gain  .changed -/-> this
      }

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[evt.Change[Value.Audio]] = {
        val offsetEvt = offset.changed
        val offsetOpt = if (offsetEvt.isSource(pull)) offsetEvt.pullUpdate(pull) else None
        val (offsetBefore, offsetNow) = offsetOpt.map(_.toTuple).getOrElse {
          val ov = offset.value
          (ov, ov)
        }

        val gainEvt = gain.changed
        val gainOpt = if (gainEvt.isSource(pull)) gainEvt.pullUpdate(pull) else None
        val (gainBefore, gainNow) = gainOpt.map(_.toTuple).getOrElse {
          val gv = gain.value
          (gv, gv)
        }

        Audio.change(Value.Audio(artifact, spec, offsetBefore, gainBefore),
                     Value.Audio(artifact, spec, offsetNow,    gainNow   ))
      }

      protected def reader = Audio.serializer[S]

      protected def writeData(out: DataOutput) {
        out.writeByte(audioCookie)
        artifact.write(out)
        CommonSerializers.AudioFileSpec.write(spec, out)
        offset.write(out)
        gain.write(out)
      }

      override def toString() = "Elem.Audio" + id
    }

    // ---- bitype ----

    def longType: BiType[Long] = Longs
    def spanLikeType: BiType[SpanLike] = SpanLikes

    def readValue(in: DataInput): Value = Value.Serializer.read(in)
    def writeValue(value: Value, out: DataOutput) {
      value.write(out)
    }

    protected def readTuple[S <: evt.Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                            (implicit tx: S#Tx): Elem[S] with evt.Node[S] = {
      (cookie: @switch) match {
        case `curveCookie` => Curve.readIdentifiedTuple(in, access, targets)
        case `audioCookie` => Audio.readIdentifiedTuple(in, access, targets)
        case _ => sys.error("Unexpected cookie " + cookie)
      }
    }
  }

  //  type Elem[S <: evt.Sys[S]] = Expr[S, Value]
  sealed trait Elem[S <: evt.Sys[S]] extends Expr[S, Value]

  type TimedElem[S <: evt.Sys[S]] = BiExpr[S, Value]

  trait Modifiable[S <: evt.Sys[S]] extends Grapheme[S] {
    def add   (elem: BiExpr[S, Value])(implicit tx: S#Tx): Unit
    def remove(elem: BiExpr[S, Value])(implicit tx: S#Tx): Boolean
    def clear()(implicit tx: S#Tx): Unit
  }

  object Modifiable {
    def apply[S <: evt.Sys[S]](implicit tx: S#Tx): Modifiable[S] = Impl.modifiable[S]

    def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Modifiable[S] =
      Impl.readModifiable(in, access)

    implicit def serializer[S <: evt.Sys[S]]: io.Serializer[S#Tx, S#Acc, Modifiable[S]] =
      Impl.modifiableSerializer[S]

    /**
     * Extractor to check if a `Grapheme` is actually a `Grapheme.Modifiable`
     */
    def unapply[S <: evt.Sys[S]](g: Grapheme[S]): Option[Modifiable[S]] = {
      if (g.isInstanceOf[Modifiable[_]]) Some(g.asInstanceOf[Modifiable[S]]) else None
    }
  }

  def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Grapheme[S] = Impl.read(in, access)
}
trait Grapheme[S <: evt.Sys[S]] extends evt.Node[S] {
  import Grapheme.{Value, Segment, Modifiable, TimedElem, Update}

  /**
   * The idea of all traits which distinguish between read-only and modifiable sub-type is that
   * eventually the super-type acts somewhat like an expression. It might be possible to map
   * a grapheme with operators, and it might be preferable to avoid having to have the modifiable
   * operations in the mapped object (cf. `Expr` versus `Expr.Var`).
   */
  def modifiableOption: Option[Modifiable[S]]

  def at     (time: Long)(implicit tx: S#Tx): Option[TimedElem[S]]
  def valueAt(time: Long)(implicit tx: S#Tx): Option[Value]
  def segment(time: Long)(implicit tx: S#Tx): Option[Segment.Defined]

  def nearestEventAfter(time: Long)(implicit tx: S#Tx): Option[Long]

  def changed: Event[S, Update[S], Grapheme[S]]

  def debugList()(implicit tx: S#Tx): List[Segment.Defined]
}