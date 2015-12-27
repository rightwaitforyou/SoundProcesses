/*
 *  Grapheme.scala
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

import de.sciss.lucre.bitemp.BiPin
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer, Serializer, Writable}
import de.sciss.synth.proc.impl.{GraphemeImpl => Impl}

import scala.annotation.switch
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

object Grapheme extends Obj.Type {
  final val typeID = 0x10002

//  // If necessary for some views, we could eventually add the Elems, too,
//  // like `changes: Vec[ (Elem[ S ], Value) ]`. Then the question would be
//  // if Elem should have an id method? I.e. we'll have `add( elem: Elem[ S ]) : StoredElem[ S ]`
//  // where `trait StoredElem[ S <: Sys[ S ]] { def elem: Elem[ S ]; def id: S#ID }`?
//  final case class Update[S <: Sys[S]](grapheme: Grapheme[S], changes: Vec[Segment])

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Grapheme[S]] = Impl.serializer[S]

  // 0 reserved for variables
  private final val curveCookie = 1
//  private final val audioCookie = 2

//  override def init(): Unit = {
//    super.init()
////    Expr .init()
//  }

  object Value {
    // implicit val biType: ExprType1[Value] = Expr

    implicit object serializer extends ImmutableSerializer[Value] {
      def write(v: Value, out: DataOutput): Unit = v.write(out)

      def read(in: DataInput): Value =
        (in.readByte().toInt: @switch) match {
          case `curveCookie`  => Curve.readIdentified(in)
//          case `audioCookie`  => Audio.readIdentified(in)
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
          if (cookie != curveCookie) sys.error(s"Unexpected cookie $cookie")
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
  }

  /** An evaluated and flattened scan element. This is either an immutable value such as a constant or
    * envelope segment, or a real-time signal, coming either from the same process (`Source`) or being
    * fed by another embedded process (`Sink`).
    */
  sealed trait Value extends Writable {
    def numChannels: Int
  }

//  object Segment {
//    sealed trait Defined extends Segment {
//      def numChannels: Int
//      final def isDefined = true
//    }
//    final case class Const(span: Span.HasStart, values: Vec[Double]) extends Defined {
//      def numChannels = values.size
//    }
//    final case class Curve(span: Span, values: Vec[(Double, Double, synth.Curve)]) extends Defined {
//      def numChannels = values.size
//    }
////    final case class Audio(span: Span.HasStart, value: Value.Audio) extends Defined {
////      def numChannels = value.numChannels
////    }
//    final case class Undefined(span: Span.HasStart) extends Segment {
//      def isDefined = false
//    }
//  }
//
//  sealed trait Segment {
//    def span: Span.HasStart
//    def isDefined: Boolean
//  }

//  object Expr extends expr.impl.ExprTypeImpl[Value, Expr] {
//    final val typeID = 11
//
//    override def init(): Unit = {
//      super.init()
//      Curve.init()
////      Audio.init()
//    }
//
//    protected def mkConst[S <: Sys[S]](id: S#ID, value: A)(implicit tx: S#Tx): Const[S] =
//      new _Const[S](id, value)
//
//    protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[Ex[S]], connect: Boolean)
//                                    (implicit tx: S#Tx): Var[S] = {
//      val res = new _Var[S](targets, vr)
//      if (connect) res.connect()
//      res
//    }
//
//    private[this] final class _Const[S <: Sys[S]](val id: S#ID, val constValue: A)
//      extends ConstImpl[S] with Expr[S]
//
//    private[this] final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[Ex[S]])
//      extends VarImpl[S] with Expr[S]
//
//    object Curve extends expr.impl.ExprTypeImpl[Value.Curve, Expr.Curve] {
//      // final val typeID = 11
//      final val typeID = 12
//
//      import Expr.{Curve => Repr}
//
//      // override def init(): Unit = ()  // prevent double registration
//
//      private[this] lazy val _init: Unit = registerExtension(ApplyCurve)
//
//      override def init(): Unit = {
//        super.init()
//        _init
//      }
//
//      protected def mkConst[S <: Sys[S]](id: S#ID, value: A)(implicit tx: S#Tx): Const[S] =
//        new _Const[S](id, value)
//
//      protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[Ex[S]], connect: Boolean)
//                                      (implicit tx: S#Tx): Var[S] = {
//        val res = new _Var[S](targets, vr)
//        if (connect) res.connect()
//        res
//      }
//
//      private[this] final class _Const[S <: Sys[S]](val id: S#ID, val constValue: A)
//        extends ConstImpl[S] with Repr[S]
//
//      private[this] final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[Ex[S]])
//        extends VarImpl[S] with Repr[S]
//
//      def apply[S <: Sys[S]](values: (DoubleObj[S], synth.Curve)*)(implicit tx: S#Tx): Curve[S] = {
//        val targets = evt.Targets[S]
//        new ApplyCurve(targets, values.toIndexedSeq).connect()
//      }
//
//      def unapplySeq[S <: Sys[S]](expr: Expr[S]): Option[Seq[(DoubleObj[S], synth.Curve)]] = {
//        if (expr.isInstanceOf[ApplyCurve[_]]) {
//          val c = expr.asInstanceOf[ApplyCurve[S]]
//          Some(c.values)
//        } else {
//          None
//        }
//      }
//
//      def valueSerializer: ImmutableSerializer[Value.Curve] = Value.Curve.serializer
//    }
//    sealed trait Curve[S <: Sys[S]] extends Expr[S] with _Expr[S, Value.Curve]
//
//    private object ApplyCurve extends Type.Extension1[Curve] {
//      def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
//                                    (implicit tx: S#Tx): Curve[S] = {
//        val sz      = in.readInt()
//        val values  = Vector.fill(sz) {
//          val mag   = DoubleObj.read(in, access)
//          val shape = synth.Curve.serializer.read(in)
//          (mag, shape)
//        }
//        new ApplyCurve(targets, values)
//      }
//
//      def name: String = "ApplyCurve"
//
//      val opHi = curveCookie
//      val opLo = curveCookie
//    }
//    private final class ApplyCurve[S <: Sys[S]](protected val targets: evt.Targets[S],
//                                                val values: Vec[(DoubleObj[S], synth.Curve)])
//      extends expr.impl.NodeImpl[S, Value.Curve] with Curve[S] {
//
//      def tpe: Obj.Type = Curve
//
//      def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
//        new ApplyCurve(Targets[Out], values.map { case (xs, curve) => context(xs) -> curve }).connect()
//
//      def value(implicit tx: S#Tx): Value.Curve = {
//        val v = values.map {
//          case (mag, shape) => mag.value -> shape
//        }
//        Value.Curve(v: _*)
//      }
//
//      def connect()(implicit tx: S#Tx): this.type = {
//        values.foreach { tup =>
//          tup._1.changed ---> changed
//        }
//        this
//      }
//
//      private[this] def disconnect()(implicit tx: S#Tx): Unit =
//        values.foreach { tup =>
//          tup._1.changed -/-> changed
//        }
//
//      object changed extends Changed {
//        def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[m.Change[Value.Curve]] = {
//          val beforeVals  = Vec.newBuilder[(Double, synth.Curve)]
//          val nowVals     = Vec.newBuilder[(Double, synth.Curve)]
//          values.foreach {
//            case (mag, shape) =>
//              val magEvt = mag.changed
//              if (pull.contains(magEvt)) {
//                pull(magEvt) match {
//                  case Some(m.Change(magBefore, magNow)) =>
//                    beforeVals += magBefore -> shape
//                    nowVals    += magNow    -> shape
//                  case _ =>
//                    val mv      = mag.value
//                    beforeVals += mv -> shape
//                    nowVals    += mv -> shape
//                }
//              } else {
//                val mv      = mag.value
//                beforeVals += mv -> shape
//                nowVals    += mv -> shape
//              }
//          }
//          val before  = Value.Curve(beforeVals.result(): _*)
//          val now     = Value.Curve(nowVals.result(): _*)
//          if (before == now) None else Some(model.Change(before, now))
//        }
//      }
//
//      protected def writeData(out: DataOutput): Unit = {
//        out.writeByte(1)  // 'node not var'
//        out.writeInt(curveCookie) // op-id
//        val sz = values.size
//        out.writeInt(sz)
//        values.foreach { tup =>
//          tup._1.write(out)
//          synth.Curve.serializer.write(tup._2, out)
//        }
//      }
//
//      protected def disposeData()(implicit tx: S#Tx): Unit = disconnect()
//
//      override def toString: String = s"Elem.Curve$id"
//    }
//
//    def valueSerializer: ImmutableSerializer[Value] = Value.serializer
//  }

//  sealed trait Expr[S <: Sys[S]] extends _Expr[S, Value]
//
//  type TimedElem[S <: Sys[S]] = BiPin.Entry[S, Expr[S]]

  trait Modifiable[S <: Sys[S]] extends Grapheme[S] with BiPin.Modifiable[S, Obj[S]]

  def apply[S <: Sys[S]](numChannels: Int)(implicit tx: S#Tx): Modifiable[S] = Impl.modifiable[S](numChannels)

  object Modifiable {
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

  override def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)
}
trait Grapheme[S <: Sys[S]] extends BiPin[S, Obj[S]] {
  import Grapheme.Modifiable

  /** The idea of all traits which distinguish between read-only and modifiable sub-type is that
    * eventually the super-type acts somewhat like an expression. It might be possible to map
    * a grapheme with operators, and it might be preferable to avoid having to have the modifiable
    * operations in the mapped object (cf. `Expr` versus `Expr.Var`).
    */
  override def modifiableOption: Option[Modifiable[S]]

//  def at     (time: Long)(implicit tx: S#Tx): Option[TimedElem[S]]
//  def valueAt(time: Long)(implicit tx: S#Tx): Option[Value]
//  def segment(time: Long)(implicit tx: S#Tx): Option[Segment.Defined]

//  def eventAfter(time: Long)(implicit tx: S#Tx): Option[Long]

  def firstEvent(implicit tx: S#Tx): Option[Long]

//  def numChannels: Int

//  def debugList()(implicit tx: S#Tx): List[Segment.Defined]
}