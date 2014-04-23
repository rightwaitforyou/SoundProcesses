/*
 *  GraphemeImpl.scala
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
package proc
package impl

import de.sciss.lucre.{event => evt, synth, bitemp}
import bitemp.BiPin
import collection.breakOut
import collection.immutable.{IndexedSeq => Vec}
import evt.{Event, impl => evti, Sys}
import proc.Grapheme.Segment
import annotation.tailrec
import de.sciss.span.Span
import de.sciss.serial.{DataOutput, DataInput}
import de.sciss.lucre
import de.sciss.lucre.synth.InMemory

object GraphemeImpl {
  import Grapheme.{Elem, TimedElem, Value, Modifiable}

  private implicit val timeEx = lucre.expr.Long

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Grapheme[S] = {
    serializer[S].read(in, access)
  }

  def readModifiable[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Grapheme.Modifiable[S] = {
    modifiableSerializer[S].read(in, access)
  }

  implicit def serializer[S <: Sys[S]]: evt.NodeSerializer[S, Grapheme[S]] =
    anySer.asInstanceOf[evt.NodeSerializer[S, Grapheme[S]]]

  implicit def modifiableSerializer[S <: Sys[S]]: evt.NodeSerializer[S, Grapheme.Modifiable[S]] =
    anySer.asInstanceOf[evt.NodeSerializer[S, Grapheme.Modifiable[S]]] // whatever...

  private val anySer = new Ser[InMemory]

  //   private val anyModSer   = new ModSer[ evt.InMemory ]

  private final class Ser[S <: Sys[S]] extends evt.NodeSerializer[S, Grapheme[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Grapheme[S] = {
      implicit val elemType = Elem // .serializer[ S ]
      val pin = BiPin.Modifiable.read[S, Value](in, access)
      new Impl(targets, pin)
    }
  }

  //   private final class ModSer[ S <: Sys[ S ]] extends evt.NodeSerializer[ S, Grapheme.Modifiable[ S ]] {
  //      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Grapheme.Modifiable[ S ] = {
  //         implicit val elemType = Elem // .serializer[ S ]
  //         val pin = BiPin.Modifiable.read[ S, Value ]( in, access )
  //         new Impl( targets, pin )
  //      }
  //   }

  def modifiable[S <: Sys[S]](implicit tx: S#Tx): Modifiable[S] = {
    val targets = evt.Targets[S] // XXX TODO: partial?
    implicit val elemType = Elem
    val pin = BiPin.Modifiable[S, Value]
    new Impl(targets, pin)
  }

  // ---- actual implementation ----

  private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                        pin: BiPin.Modifiable[S, Grapheme.Value])
    extends Modifiable[S] with evti.StandaloneLike[S, Grapheme.Update[S], Grapheme[S]] {
    graph =>

    override def toString() = "Grapheme" + id

    def modifiableOption: Option[Modifiable[S]] = Some(this)

    def changed: Event[S, Grapheme.Update[S], Grapheme[S]] = this

    // ---- forwarding to pin ----

    def add   (elem: TimedElem[S])(implicit tx: S#Tx): Unit     = pin.add(elem)
    def remove(elem: TimedElem[S])(implicit tx: S#Tx): Boolean  = pin.remove(elem)

    def clear()(implicit tx: S#Tx): Unit = pin.clear()

    def at(time: Long)(implicit tx: S#Tx): Option[TimedElem[S]] = pin.at(time)

    def nearestEventAfter(time: Long)(implicit tx: S#Tx): Option[Long] = pin.nearestEventAfter(time)

    // ---- extensions ----

    def debugList()(implicit tx: S#Tx): List[Segment.Defined] = {
      @tailrec def loop(time: Long, tail: List[Segment.Defined]): List[Segment.Defined] =
        segment(time) match {
          case Some(s) =>
            loop(s.span.start - 1, s :: tail)
          case _ => tail
        }

      loop(Long.MaxValue - 1, Nil)
    }

    def valueAt(time: Long)(implicit tx: S#Tx): Option[Value] = {
      pin.floor(time).map(_.magValue)
    }

    def segment(time: Long)(implicit tx: S#Tx): Option[Segment.Defined] = {
      pin.floor(time).map { elem =>
        val (floorTime, floorVal) = elem.value
        segmentFromFloor(floorTime, floorVal)
      }
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
      val t1 = floorTime + 1
      floorValue match {
        case floorCurve: Value.Curve =>
          val floorCurveVals: Vec[Double] = floorCurve.values.map(_._1)(breakOut)
          pin.ceil(t1) match {
            case Some(ceilElem) =>
              val (ceilTime, ceilVal) = ceilElem.value
              segmentFromSpan(floorTime, floorCurveVals, ceilTime, ceilVal)
            case None =>
              Segment.Const(Span.from(floorTime), floorCurveVals)
          }

        case av: Value.Audio =>
          val span = pin.nearestEventAfter(t1) match {
            case Some(ceilTime) => Span(floorTime, ceilTime)
            case _ => Span.from(floorTime)
          }
          Segment.Audio(span, av)
      }
    }

    private def segmentAfterRemoved(removeTime: Long)(implicit tx: S#Tx): Segment = segment(removeTime).getOrElse {
      val span = pin.nearestEventAfter(removeTime + 1) match {
        case Some(ceilTime) => Span(removeTime, ceilTime)
        case _ => Span.from(removeTime)
      }
      Segment.Undefined(span)
    }

    private def segmentsAfterAdded(addTime: Long, addValue: Value)(implicit tx: S#Tx): Vec[Segment] = {
      val floorSegm = pin.floor(addTime - 1) match {
        case Some(floorElem) =>
          val (floorTime, floorValue) = floorElem.value
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

    protected def reader: evt.Reader[S, Grapheme[S]] = serializer[S]

    def connect   ()(implicit tx: S#Tx): Unit = pin.changed ---> this
    def disconnect()(implicit tx: S#Tx): Unit = pin.changed -/-> this

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
              case BiPin.Added((addTime, addVal), _)  => segmentsAfterAdded(addTime, addVal)
              case BiPin.Removed((remTime, _), _)     => Vec(segmentAfterRemoved(remTime))

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

              case BiPin.Element(elem, elemCh) =>
                val (timeCh, magCh) = elemCh.unzip
                val seqAdd = segmentsAfterAdded(timeCh.now, magCh.now)
                if (timeCh.isSignificant) {
                  segmentAfterRemoved(timeCh.before) +: seqAdd
                } else seqAdd
            }
            seq.foldLeft(res)(incorporate)
        }
        if (segm.nonEmpty) Some(Grapheme.Update(graph, segm)) else None
      }
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = pin.dispose()
    protected def writeData(out: DataOutput)      : Unit = pin.write(out)
  }
}