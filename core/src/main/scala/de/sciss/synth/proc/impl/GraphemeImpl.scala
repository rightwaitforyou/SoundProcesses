/*
 *  GraphemeImpl.scala
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

package de.sciss.synth
package proc
package impl

import de.sciss.lucre.bitemp.BiPin
import de.sciss.lucre.event.{impl => evti, Targets}
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Elem, Copy, NoSys, Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.span.Span
import de.sciss.synth.proc.Grapheme.Segment

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}

object GraphemeImpl {
  import Grapheme.{Expr, Modifiable, TimedElem, Value}

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Grapheme[S] =
    serializer[S].read(in, access)

  def readModifiable[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Grapheme.Modifiable[S] = {
    modifiableSerializer[S].read(in, access)
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Grapheme[S]] =
    anySer.asInstanceOf[Ser[S]]

  implicit def modifiableSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Grapheme.Modifiable[S]] =
    anySer.asInstanceOf[Serializer[S#Tx, S#Acc, Grapheme.Modifiable[S]]] // whatever... right now it is modifiable

  private val anySer = new Ser[NoSys]

  //   private val anyModSer   = new ModSer[ evt.InMemory ]

  private final class Ser[S <: Sys[S]] extends ObjSerializer[S, Grapheme[S]] {
    def tpe: Obj.Type = Grapheme
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Grapheme[S] = {
    val targets     = Targets.read(in, access)
    val numChannels = in.readInt()
    val pin         = BiPin.Modifiable.read[S, Expr[S]](in, access)
    new Impl(targets, numChannels, pin)
  }

  //   private final class ModSer[ S <: Sys[ S ]] extends evt.NodeSerializer[ S, Grapheme.Modifiable[ S ]] {
  //      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Grapheme.Modifiable[ S ] = {
  //         implicit val elemType = Elem // .serializer[ S ]
  //         val pin = BiPin.Modifiable.read[ S, Value ]( in, access )
  //         new Impl( targets, pin )
  //      }
  //   }

  def modifiable[S <: Sys[S]](numChannels: Int)(implicit tx: S#Tx): Modifiable[S] = {
    val targets = evt.Targets[S] // XXX TODO: partial?
    implicit val elemType = Expr
    val pin = BiPin.Modifiable[S, Expr[S]]
    new Impl(targets, numChannels, pin).connect()
  }

  // ---- actual implementation ----

  private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S], val numChannels: Int,
                                        pin: BiPin.Modifiable[S, Grapheme.Expr[S]])
    extends Modifiable[S] with evti.SingleNode[S, Grapheme.Update[S]] {
    graph =>

    def tpe: Obj.Type = Grapheme

    override def toString: String = s"Grapheme$id"

    def modifiableOption: Option[Modifiable[S]] = Some(this)

    def copy()(implicit tx: S#Tx, copy: Copy[S]): Elem[S] =
      new Impl(Targets[S], numChannels, copy(pin)).connect()

    object changed extends Changed with evti.Root[S, Grapheme.Update[S]]

    // ---- forwarding to pin ----

    def add(elem: TimedElem[S])(implicit tx: S#Tx): Unit = {
      val elemCh = elem.value.value.numChannels
      if (elemCh != numChannels)
        throw new IllegalArgumentException(
          s"Trying to add element with $elemCh channels to grapheme with $numChannels channels"
        )

      pin.add(elem)
    }
    def remove(elem: TimedElem[S])(implicit tx: S#Tx): Boolean  = pin.remove(elem)

    def clear()(implicit tx: S#Tx): Unit = pin.clear()

    def at(time: Long)(implicit tx: S#Tx): Option[TimedElem[S]] = pin.at(time)

    def eventAfter(time: Long)(implicit tx: S#Tx): Option[Long] = pin.eventAfter(time)

    def firstEvent(implicit tx: S#Tx): Option[Long] = pin.eventAfter(Long.MinValue)

    // ---- extensions ----

    def debugList()(implicit tx: S#Tx): List[Segment.Defined] = {
      @tailrec def loop(time: Long, tail: List[Segment.Defined]): List[Segment.Defined] =
        segment(time) match {
          case Some(s) =>
            loop(s.span.start - 1, s :: tail)
          case _ => tail
        }

      loop(Long.MinValue, Nil)
    }

    def valueAt(time: Long)(implicit tx: S#Tx): Option[Value] =
      pin.floor(time).map(_.value.value)

    def segment(time: Long)(implicit tx: S#Tx): Option[Segment.Defined] =
      pin.floor(time).map { elem =>
        val floorTime = elem.key  .value
        val floorVal  = elem.value.value
        // val (floorTime, floorVal) = elem.value
        segmentFromFloor(floorTime, floorVal)
      }

    private def segmentFromSpan(floorTime: Long, floorCurveVals: Vec[Double],
                                ceilTime: Long, ceilValue: Value): Segment.Defined = {
      val span = Span(floorTime, ceilTime)
      ceilValue match {
        case ceilCurve: Value.Curve if ceilCurve.numChannels == floorCurveVals.size =>
          val curveValues = floorCurveVals.zip(ceilCurve.values).map {
            case (floor, (ceil, shape)) => (floor, ceil, shape)
          }
          Segment.Curve(span, curveValues)
        case _ =>
          Segment.Const(span, floorCurveVals)
      }
    }

    private def segmentFromFloor(floorTime: Long, floorValue: Value)(implicit tx: S#Tx): Segment.Defined = {
      floorValue match {
        case floorCurve: Value.Curve =>
          val floorCurveVals: Vec[Double] = floorCurve.values.map(_._1)(breakOut)
          pin.ceil(floorTime + 1) match {
            case Some(ceilElem) =>
              val ceilTime = ceilElem.key  .value
              val ceilVal  = ceilElem.value.value
              // val (ceilTime, ceilVal) = ceilElem.value
              segmentFromSpan(floorTime, floorCurveVals, ceilTime, ceilVal)
            case None =>
              Segment.Const(Span.from(floorTime), floorCurveVals)
          }

        case av: Value.Audio =>
          val span = pin.eventAfter(floorTime) match {
            case Some(ceilTime) => Span(floorTime, ceilTime)
            case _ => Span.from(floorTime)
          }
          Segment.Audio(span, av)
      }
    }

    private def segmentAfterRemoved(removeTime: Long)(implicit tx: S#Tx): Segment = segment(removeTime).getOrElse {
      val span = pin.eventAfter(removeTime) match {
        case Some(ceilTime) => Span(removeTime, ceilTime)
        case _ => Span.from(removeTime)
      }
      Segment.Undefined(span)
    }

    private def segmentsAfterAdded(addTime: Long, addValue: Value)(implicit tx: S#Tx): Vec[Segment] = {
      val floorSegm = pin.floor(addTime - 1) match {
        case Some(floorElem) =>
          val floorTime  = floorElem.key.value
          val floorValue = floorElem.value.value
          // val (floorTime, floorValue) = floorElem.value
          val s = floorValue match {
            case floorCurve: Value.Curve =>
              val floorCurveVals: Vec[Double] = floorCurve.values.map(_._1)(breakOut)
              segmentFromSpan(floorTime, floorCurveVals, addTime, addValue)
            case av: Value.Audio =>
              Segment.Audio(Span(floorTime, addTime), av)
          }
          Vector(s)
        case _ =>
          Vector.empty
      }

      floorSegm :+ segmentFromFloor(addTime, addValue)
    }

    // ---- node and event ----

    def connect()(implicit tx: S#Tx): this.type = {
      pin.changed ---> changed
      this
    }

    private[this] def disconnect()(implicit tx: S#Tx): Unit = pin.changed -/-> changed

    private def incorporate(in: Vec[Segment], add: Segment): Vec[Segment] = {
      val addSpan = add.span
      //         val (pre, tmp)    = in.span(  s => !s.span.overlaps( addSpan ))
      //         val (mid, post)   = tmp.span( s =>  s.span.overlaps( addSpan ))
      //         assert( mid.forall( _.span == addSpan ))
      //         pre :+ add +: post
      val addStart = addSpan.start
      val i = in.indexWhere(_.span.start >= addStart)
      if (i < 0) {
        in :+ add
      } else {
        val inSpan = in(i).span
        if (inSpan == addSpan) {
          in.patch(i, Vec(add), 1)
        } else {
          assert(!inSpan.overlaps(addSpan))
          in.patch(i, Vec(add), 0)
        }
      }
    }

    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Grapheme.Update[S]] = {
      pull(pin.changed).flatMap { upd =>
        val segm: Vec[Segment] = upd.changes.foldLeft(Vec.empty[Segment]) {
          case (res, ch) =>
            val seq = ch match {
              case BiPin.Added  (addTime, entry)    => segmentsAfterAdded(addTime, entry.value.value)
              case BiPin.Removed(remTime, entry)    => Vec(segmentAfterRemoved(remTime))

              // the BiPin.Collection update assumes the 'pin' character
              // of the elements. that means that for example, an insertion
              //
              // |------I-------|
              // floor          ceil
              //
              // will fire the dirty region
              //
              //        I-------|
              //
              // but the grapheme will change significantly more when curves are involved:
              // - if `I` is `Audio`
              //   - it may have been that ceil was a curve, and thus a segment was
              //     stretching from floorTime to ceilTime, which is now replaced by
              //     a constant from floorTime to I-time. Note that because multiple changes
              //     might be collapsed, we should not assume that the segment was constructed
              //     with the value now found at ceilTime. Instead, pessimistically, we will
              //     always assume that the span floor--I must be fired.
              //   - the span I--ceil is straight forward and does not need additional evaluations
              // - if `I` is `Curve`
              //   - the dirty region floor--I must be fired as above.
              //   - additionally the span I--ceil must be resolved by looking at the holder at
              //     ceilTime: if ceilTime is undefined, or if it is `Audio`, the span I--ceil is a
              //     constant, otherwise if it is `Curve` we need to calculate the curve segment.

              case BiPin.Moved(timeCh, elem) =>
                // val (timeCh, magCh) = elemCh.unzip
                val seqAdd = segmentsAfterAdded(timeCh.now, elem.value.value /* magCh.now */)
                if (timeCh.isSignificant) {
                  segmentAfterRemoved(timeCh.before) +: seqAdd
                } else seqAdd
            }
            seq.foldLeft(res)(incorporate)
        }
        if (segm.nonEmpty) Some(Grapheme.Update(graph, segm)) else None
      }
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      disconnect()
      pin.dispose()
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeInt(numChannels)
      pin.write(out)
    }
  }
}