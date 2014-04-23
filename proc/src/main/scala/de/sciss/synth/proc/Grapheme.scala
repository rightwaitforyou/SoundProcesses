/*
 *  Grapheme.scala
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

import lucre.{event => evt, bitemp, expr}
import de.sciss.lucre.expr.{ExprType1, Expr}
import bitemp.BiExpr
import collection.immutable.{IndexedSeq => Vec}
import annotation.switch
import impl.{GraphemeImpl => Impl}
import synth.io.AudioFileSpec
import de.sciss.lucre.event.Publisher
import span.Span
import serial.{Writable, DataInput, DataOutput, ImmutableSerializer, Serializer}
import java.io.File
import language.implicitConversions
import de.sciss.{model => m}
import evt.Sys

object Grapheme {
  // If necessary for some views, we could eventually add the Elems, too,
  // like `changes: Vec[ (Elem[ S ], Value) ]`. Then the question would be
  // if Elem should have an id method? I.e. we'll have `add( elem: Elem[ S ]) : StoredElem[ S ]`
  // where `trait StoredElem[ S <: Sys[ S ]] { def elem: Elem[ S ]; def id: S#ID }`?
  final case class Update[S <: evt.Sys[S]](grapheme: Grapheme[S], changes: Vec[Segment])

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Grapheme[S]] = Impl.serializer[S]

  // 0 reserved for variables
  private final val curveCookie = 1
  private final val audioCookie = 2

  object Value {
    implicit val biType: ExprType1[Value] = Elem

    implicit object serializer extends ImmutableSerializer[Value] {
      def write(v: Value, out: DataOutput): Unit = v.write(out)

      def read(in: DataInput): Value =
        (in.readByte().toInt: @switch) match {
          case `curveCookie`  => Curve.readIdentified(in)
          case `audioCookie`  => Audio.readIdentified(in)
          case cookie         => sys.error("Unexpected cookie " + cookie)
        }
    }

    // implicit def curveFromMonoTuple(tup: (Double, synth.Curve)): Curve = Curve.fromMonoTuple(tup)

    object Curve {
      // implicit def fromMonoTuple(tup: (Double, synth.Curve)): Curve = Curve(tup)

      implicit object serializer extends ImmutableSerializer[Curve] {
        def write(v: Curve, out: DataOutput): Unit = v.write(out)
        def read(in: DataInput): Curve = {
          val cookie = in.readByte()
          require(cookie == `curveCookie`, s"Unexpected cookie $cookie")
          readIdentified(in)
        }
      }
      private[Value] def readIdentified(in: DataInput): Curve = {
        val sz      = in.readInt()
        val values  = Vec.fill(sz) {
          val mag   = in.readDouble()
          val env   = synth.Curve.serializer.read(in)
          (mag, env)
        }
        Curve(values: _*)
      }
    }

    /** A mono- or polyphonic constant value.
      *
      * @param values  pairs interpreted as target values and target shapes.
      */
    final case class Curve(values: (Double, synth.Curve)*) extends Value {
      def numChannels = values.size

      def write(out: DataOutput): Unit = {
        out.writeByte(curveCookie)
        val sz = values.size
        out.writeInt(sz)
        values.foreach { case (mag, shape) =>
          out.writeDouble(mag)
          synth.Curve.serializer.write(shape, out)
        }
      }
    }

    object Audio {
      implicit object serializer extends ImmutableSerializer[Audio] {
        def write(v: Audio, out: DataOutput): Unit = v.write(out)
        def read(in: DataInput): Audio = {
          val cookie = in.readByte()
          require(cookie == `audioCookie`, s"Unexpected cookie $cookie")
          readIdentified(in)
        }
      }
      private[Value] def readIdentified(in: DataInput): Audio = {
        val artifact  = new File(in.readUTF()) // Artifact.read(in, access)
        val spec      = AudioFileSpec.Serializer.read(in)
        val offset    = in.readLong()
        val gain      = in.readDouble()
        Audio(artifact, spec, offset, gain)
      }
    }

    /** An audio region segment.
      *
      * @param  artifact  the audio file
      * @param  spec      the audio file specification, e.g. retrieved via `AudioFile.readSpec`
      * @param  offset    the file offset in sample frames
      * @param  gain      the gain factor (linear, where 1.0 is original volume)
      */
    final case class Audio(artifact: Artifact.Value, spec: AudioFileSpec, offset: Long, gain: Double)
      extends Value {

      def numChannels = spec.numChannels

      def write(out: DataOutput): Unit = {
        out.writeByte(audioCookie)
        out.writeUTF(artifact.getPath) // artifact.write(out)
        AudioFileSpec.Serializer.write(spec, out)
        out.writeLong(offset)
        out.writeDouble(gain)
      }
    }
  }

  /** An evaluated and flattened scan element. This is either an immutable value such as a constant or
    * envelope segment, or a real-time signal, coming either from the same process (`Source`) or being
    * fed by another embedded process (`Sink`).
    */
  sealed trait Value extends Writable {
    def numChannels: Int
  }

  object Segment {
    sealed trait Defined extends Segment {
      def numChannels: Int
      final def isDefined = true
    }
    final case class Const(span: Span.HasStart, values: Vec[Double]) extends Defined {
      def numChannels = values.size
    }
    final case class Curve(span: Span, values: Vec[(Double, Double, synth.Curve)]) extends Defined {
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

  object Elem extends expr.impl.ExprTypeImplA[Value] {
    final val typeID = 11

    object Curve extends expr.impl.ExprTypeImplA[Value.Curve] {
      final val typeID = 12

      def apply[S <: Sys[S]](values: (Expr[S, Double], synth.Curve)*)(implicit tx: S#Tx): Curve[S] = {
        val targets = evt.Targets.partial[S] // XXX TODO partial?
        new CurveImpl(targets, values.toIndexedSeq)
      }

      def unapplySeq[S <: Sys[S]](expr: Elem[S]): Option[Seq[(Expr[S, Double], synth.Curve)]] = {
        if (expr.isInstanceOf[CurveImpl[_]]) {
          val c = expr.asInstanceOf[CurveImpl[S]]
          Some(c.values)
        } else {
          None
        }
      }

      def readValue(in: DataInput): Value.Curve = Value.Curve.serializer.read(in)

      def writeValue(v: Value.Curve, out: DataOutput): Unit = v.write(out)

      protected def readTuple[S <: evt.Sys[S]](cookie: Int, in: DataInput, access: S#Acc,
                                               targets: evt.Targets[S])(implicit tx: S#Tx): Curve[S] with evt.Node[S] = {
        require(cookie == curveCookie, s"Unexpected cookie $cookie")
        readIdentifiedTuple(in, access, targets)
      }

      private[Grapheme] def readIdentifiedTuple[S <: evt.Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                                (implicit tx: S#Tx): Curve[S] with evt.Node[S] = {
        val sz      = in.readInt()
        val values  = Vec.fill(sz) {
          val mag   = lucre.expr.Double.read(in, access)
          val shape = synth.Curve.serializer.read(in)
          (mag, shape)
        }
        new CurveImpl(targets, values)
      }
    }
    sealed trait Curve[S <: evt.Sys[S]] extends Elem[S] with Expr[S, Value.Curve]

    object Audio extends expr.impl.ExprTypeImplA[Value.Audio] {
      final val typeID = 13

      def apply[S <: Sys[S]](artifact: Artifact[S], spec: AudioFileSpec, offset: Expr[S, Long], gain: Expr[S, Double])
                                (implicit tx: S#Tx): Audio[S] = {
        val targets = evt.Targets.partial[S] // XXX TODO partial?
        new AudioImpl(targets, artifact, spec, offset, gain)
      }

      def unapply[S <: Sys[S]](expr: Elem[S]): Option[(Artifact[S], AudioFileSpec, Expr[S, Long], Expr[S, Double])] = {
        if (expr.isInstanceOf[AudioImpl[_]]) {
          val a = expr.asInstanceOf[AudioImpl[S]]
          Some((a.artifact, a.spec, a.offset, a.gain))
        } else {
          None
        }
      }

      def readValue(in: DataInput): Value.Audio = Value.Audio.serializer.read(in)

      def writeValue(v: Value.Audio, out: DataOutput): Unit = v.write(out)

      protected def readTuple[S <: evt.Sys[S]](cookie: Int, in: DataInput, access: S#Acc,
                                           targets: evt.Targets[S])(implicit tx: S#Tx): Audio[S] with evt.Node[S] = {
        require(cookie == audioCookie, s"Unexpected cookie $cookie")
        readIdentifiedTuple(in, access, targets)
      }

      private[Grapheme] def readIdentifiedTuple[S <: evt.Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                             (implicit tx: S#Tx): Audio[S] with evt.Node[S] = {
        val artifact  = Artifact.read(in, access)
        val spec      = AudioFileSpec.Serializer.read(in)
        val offset    = lucre.expr.Long  .read(in, access)
        val gain      = lucre.expr.Double.read(in, access)
        new AudioImpl(targets, artifact, spec, offset, gain)
      }
    }
    sealed trait Audio[S <: evt.Sys[S]] extends Elem[S] with Expr[S, Value.Audio] {
      def artifact: Artifact[S]
      def offset  : Expr[S, Long  ]
      def gain    : Expr[S, Double]
      def spec    : AudioFileSpec
    }

    private final class CurveImpl[S <: evt.Sys[S]](protected val targets: evt.Targets[S],
                                                   val values: Vec[(Expr[S, Double], synth.Curve)])
      extends expr.impl.NodeImpl[S, Value.Curve] with Curve[S] {
      def value(implicit tx: S#Tx): Value.Curve = {
        val v = values.map {
          case (mag, shape) => mag.value -> shape
        }
        Value.Curve(v: _*)
      }

      def connect()(implicit tx: S#Tx): Unit =
        values.foreach { tup =>
          tup._1.changed ---> this
        }

      def disconnect()(implicit tx: S#Tx): Unit =
        values.foreach { tup =>
          tup._1.changed -/-> this
        }

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[m.Change[Value.Curve]] = {
        val beforeVals  = Vec.newBuilder[(Double, synth.Curve)]
        val nowVals     = Vec.newBuilder[(Double, synth.Curve)]
        values.foreach {
          case (mag, shape) =>
            val magEvt = mag.changed
            if (pull.contains(magEvt)) {
              pull(magEvt) match {
                case Some(m.Change(magBefore, magNow)) =>
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
        val before  = Value.Curve(beforeVals.result(): _*)
        val now     = Value.Curve(nowVals.result(): _*)
        if (before == now) None else Some(model.Change(before, now))
      }

      protected def reader = Curve.serializer[S]

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(curveCookie)
        val sz = values.size
        out.writeInt(sz)
        values.foreach { tup =>
          tup._1.write(out)
          synth.Curve.serializer.write(tup._2, out)
        }
      }

      override def toString() = "Elem.Curve" + id
    }

    private final class AudioImpl[S <: evt.Sys[S]](protected val targets: evt.Targets[S], val artifact: Artifact[S],
                                                   val spec: AudioFileSpec, val offset: Expr[S, Long],
                                                   val gain: Expr[S, Double])
      extends expr.impl.NodeImpl[S, Value.Audio] with Audio[S] {
      def value(implicit tx: S#Tx): Value.Audio = {
        val artVal    = artifact.value
        val offsetVal = offset  .value
        val gainVal   = gain    .value
        Value.Audio(artVal, spec, offsetVal, gainVal)
      }

      def connect()(implicit tx: S#Tx): Unit = {
        artifact.changed ---> this
        offset  .changed ---> this
        gain    .changed ---> this
      }

      def disconnect()(implicit tx: S#Tx): Unit = {
        artifact.changed -/-> this
        offset  .changed -/-> this
        gain    .changed -/-> this
      }

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[m.Change[Value.Audio]] = {
        val artEvt = artifact.changed
        val artOpt = if (pull.contains(artEvt)) pull(artEvt) else None
        val (artBefore, artNow) = artOpt.map(_.toTuple).getOrElse {
          val art = artifact.value
          (art, art)
        }

        val offsetEvt = offset.changed
        val offsetOpt = if (pull.contains(offsetEvt)) pull(offsetEvt) else None
        val (offsetBefore, offsetNow) = offsetOpt.map(_.toTuple).getOrElse {
          val ov = offset.value
          (ov, ov)
        }

        val gainEvt = gain.changed
        val gainOpt = if (pull.contains(gainEvt)) pull(gainEvt) else None
        val (gainBefore, gainNow) = gainOpt.map(_.toTuple).getOrElse {
          val gv = gain.value
          (gv, gv)
        }

        val before  = Value.Audio(artBefore, spec, offsetBefore, gainBefore)
        val now     = Value.Audio(artNow   , spec, offsetNow   , gainNow   )
        if (before == now) None else Some(model.Change(before, now))
      }

      protected def reader = Audio.serializer[S]

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(audioCookie)
        artifact.write(out)
        AudioFileSpec.Serializer.write(spec, out)
        offset.write(out)
        gain.write(out)
      }

      override def toString() = "Elem.Audio" + id
    }

    // ---- bi-type ----

    //    def longType    : BiType[Long    ] = Longs
    //    def spanLikeType: BiType[SpanLike] = SpanLikes

    def readValue(in: DataInput): Value = Value.serializer.read(in)
    def writeValue(value: Value, out: DataOutput): Unit =value.write(out)

    protected def readTuple[S <: evt.Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                            (implicit tx: S#Tx): Elem[S] with evt.Node[S] = {
      (cookie: @switch) match {
        case `curveCookie` => Curve.readIdentifiedTuple(in, access, targets)
        case `audioCookie` => Audio.readIdentifiedTuple(in, access, targets)
        case _ => sys.error("Unexpected cookie " + cookie)
      }
    }
  }

  //  type Elem[S <: Sys[S]] = Expr[S, Value]
  sealed trait Elem[S <: evt.Sys[S]] extends Expr[S, Value]

  type TimedElem[S <: evt.Sys[S]] = BiExpr[S, Value]

  trait Modifiable[S <: evt.Sys[S]] extends Grapheme[S] {
    def add   (elem: BiExpr[S, Value])(implicit tx: S#Tx): Unit
    def remove(elem: BiExpr[S, Value])(implicit tx: S#Tx): Boolean
    def clear()(implicit tx: S#Tx): Unit
  }

  object Modifiable {
    def apply[S <: Sys[S]](implicit tx: S#Tx): Modifiable[S] = Impl.modifiable[S]

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Modifiable[S] =
      Impl.readModifiable(in, access)

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Modifiable[S]] =
      Impl.modifiableSerializer[S]

    /** Extractor to check if a `Grapheme` is actually a `Grapheme.Modifiable`. */
    def unapply[S <: Sys[S]](g: Grapheme[S]): Option[Modifiable[S]] = {
      if (g.isInstanceOf[Modifiable[_]]) Some(g.asInstanceOf[Modifiable[S]]) else None
    }
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Grapheme[S] = Impl.read(in, access)
}
trait Grapheme[S <: evt.Sys[S]] extends evt.Node[S] with Publisher[S, Grapheme.Update[S]] {
  import Grapheme.{Value, Segment, Modifiable, TimedElem}

  /** The idea of all traits which distinguish between read-only and modifiable sub-type is that
    * eventually the super-type acts somewhat like an expression. It might be possible to map
    * a grapheme with operators, and it might be preferable to avoid having to have the modifiable
    * operations in the mapped object (cf. `Expr` versus `Expr.Var`).
    */
  def modifiableOption: Option[Modifiable[S]]

  def at     (time: Long)(implicit tx: S#Tx): Option[TimedElem[S]]
  def valueAt(time: Long)(implicit tx: S#Tx): Option[Value]
  def segment(time: Long)(implicit tx: S#Tx): Option[Segment.Defined]

  def nearestEventAfter(time: Long)(implicit tx: S#Tx): Option[Long]

  def debugList()(implicit tx: S#Tx): List[Segment.Defined]
}