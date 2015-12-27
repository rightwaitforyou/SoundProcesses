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

import de.sciss.lucre.bitemp.impl.BiPinImpl
import de.sciss.lucre.bitemp.impl.BiPinImpl.Tree
import de.sciss.lucre.event.Targets
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, Serializer}

object GraphemeImpl {
  import Grapheme.Modifiable

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

  private final class Ser[S <: Sys[S]] extends ObjSerializer[S, Grapheme[S]] {
    def tpe: Obj.Type = Grapheme
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Grapheme[S] = {
    val targets = Targets.read(in, access)
    new Impl(targets) {
      val tree: Tree[S, Obj[S]] = readTree(in, access)
    }
  }

  def modifiable[S <: Sys[S]](numChannels: Int)(implicit tx: S#Tx): Modifiable[S] = {
    val targets = evt.Targets[S]
    new Impl[S](targets) {
      val tree: Tree[S, Obj[S]] = newTree()
    } // .connect()
  }

  // ---- actual implementation ----

  private abstract class Impl[S <: Sys[S]](protected val targets: evt.Targets[S])
    extends BiPinImpl.Impl[S, Obj] with Grapheme.Modifiable[S] {
    in =>

    final def tpe: Obj.Type = Grapheme

    override def toString: String = s"Grapheme$id"

    final def modifiableOption: Option[Modifiable[S]] = Some(this)

    final def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl[Out](evt.Targets[Out]) { out =>
        val tree: Tree[Out, A] = out.newTree()
        context.defer[PinAux](in, out)(BiPinImpl.copyTree(in.tree, out.tree, out))
        // out.connect()
      }

    final def firstEvent(implicit tx: S#Tx): Option[Long] = eventAfter(Long.MinValue)

//    def segment(time: Long)(implicit tx: S#Tx): Option[Segment.Defined] =
//      pin.floor(time).map { elem =>
//        val floorTime = elem.key  .value
//        val floorVal  = elem.value.value
//        // val (floorTime, floorVal) = elem.value
//        segmentFromFloor(floorTime, floorVal)
//      }

//    private def segmentFromSpan(floorTime: Long, floorCurveVals: Vec[Double],
//                                ceilTime: Long, ceilValue: Value): Segment.Defined = {
//      val span = Span(floorTime, ceilTime)
//      ceilValue match {
//        case ceilCurve: Value.Curve if ceilCurve.numChannels == floorCurveVals.size =>
//          val curveValues = floorCurveVals.zip(ceilCurve.values).map {
//            case (floor, (ceil, shape)) => (floor, ceil, shape)
//          }
//          Segment.Curve(span, curveValues)
//        case _ =>
//          Segment.Const(span, floorCurveVals)
//      }
//    }

//    private def segmentFromFloor(floorTime: Long, floorValue: Value)(implicit tx: S#Tx): Segment.Defined = {
//      floorValue match {
//        case floorCurve: Value.Curve =>
//          val floorCurveVals: Vec[Double] = floorCurve.values.map(_._1)(breakOut)
//          pin.ceil(floorTime + 1) match {
//            case Some(ceilElem) =>
//              val ceilTime = ceilElem.key  .value
//              val ceilVal  = ceilElem.value.value
//              // val (ceilTime, ceilVal) = ceilElem.value
//              segmentFromSpan(floorTime, floorCurveVals, ceilTime, ceilVal)
//            case None =>
//              Segment.Const(Span.from(floorTime), floorCurveVals)
//          }
//
////        case av: Value.Audio =>
////          val span = pin.eventAfter(floorTime) match {
////            case Some(ceilTime) => Span(floorTime, ceilTime)
////            case _              => Span.from(floorTime)
////          }
////          Segment.Audio(span, av)
//      }
//    }

//    private def segmentAfterRemoved(removeTime: Long)(implicit tx: S#Tx): Segment = segment(removeTime).getOrElse {
//      val span = pin.eventAfter(removeTime) match {
//        case Some(ceilTime) => Span(removeTime, ceilTime)
//        case _ => Span.from(removeTime)
//      }
//      Segment.Undefined(span)
//    }

//    private def segmentsAfterAdded(addTime: Long, addValue: Value)(implicit tx: S#Tx): Vec[Segment] = {
//      val floorSegm = pin.floor(addTime - 1) match {
//        case Some(floorElem) =>
//          val floorTime  = floorElem.key.value
//          val floorValue = floorElem.value.value
//          // val (floorTime, floorValue) = floorElem.value
//          val s = floorValue match {
//            case floorCurve: Value.Curve =>
//              val floorCurveVals: Vec[Double] = floorCurve.values.map(_._1)(breakOut)
//              segmentFromSpan(floorTime, floorCurveVals, addTime, addValue)
////            case av: Value.Audio =>
////              Segment.Audio(Span(floorTime, addTime), av)
//          }
//          Vector(s)
//        case _ =>
//          Vector.empty
//      }
//
//      floorSegm :+ segmentFromFloor(addTime, addValue)
//    }

//    private def incorporate(in: Vec[Segment], add: Segment): Vec[Segment] = {
//      val addSpan = add.span
//      //         val (pre, tmp)    = in.span(  s => !s.span.overlaps( addSpan ))
//      //         val (mid, post)   = tmp.span( s =>  s.span.overlaps( addSpan ))
//      //         assert( mid.forall( _.span == addSpan ))
//      //         pre :+ add +: post
//      val addStart = addSpan.start
//      val i = in.indexWhere(_.span.start >= addStart)
//      if (i < 0) {
//        in :+ add
//      } else {
//        val inSpan = in(i).span
//        if (inSpan == addSpan) {
//          in.patch(i, Vec(add), 1)
//        } else {
//          assert(!inSpan.overlaps(addSpan))
//          in.patch(i, Vec(add), 0)
//        }
//      }
//    }
  }
}