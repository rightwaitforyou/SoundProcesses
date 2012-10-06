/*
 *  GraphemeImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2012 Hanns Holger Rutz. All rights reserved.
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
package impl

import de.sciss.lucre.{event => evt, Writable, bitemp, expr, DataOutput, DataInput}
import annotation.switch
import expr.Expr
import bitemp.{Span, BiPin}
import de.sciss.synth.expr.{Doubles, Longs}
import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import evt.{Event, EventLikeSerializer, impl => evti, Sys}
import io.AudioFileSpec
import proc.Grapheme.Segment

object GraphemeImpl {
   import Grapheme.{Elem, TimedElem, Value, Modifiable}
   import Elem.{Audio, Curve}

   private implicit val timeEx = Longs

   def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Grapheme[ S ] = {
      serializer[ S ].read( in, access )
   }

   implicit def serializer[ S <: Sys[ S ]] : evt.NodeSerializer[ S, Grapheme[ S ]] =
      anySer.asInstanceOf[ evt.NodeSerializer[ S, Grapheme[ S ]]]

   private val anySer = new Ser[ evt.InMemory ]

   private final class Ser[ S <: Sys[ S ]] extends evt.NodeSerializer[ S, Grapheme[ S ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Grapheme[ S ] = {
         implicit val elemType = Elem // .serializer[ S ]
         val pin = BiPin.Modifiable.read[ S, Value ]( in, access )
         new Impl( targets, pin )
      }
   }

   def modifiable[ S <: Sys[ S ]]( implicit tx: S#Tx ) : Modifiable[ S ] = {
      val targets = evt.Targets[ S ]   // XXX TODO: partial?
      implicit val elemType = Elem
      val pin     = BiPin.Modifiable[ S, Value ]
      new Impl( targets, pin )
   }

   // ---- actual implementation ----

   private final class Impl[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ],
                                             pin: BiPin.Modifiable[ S, Grapheme.Value ])
   extends Modifiable[ S ] with evti.StandaloneLike[ S, Grapheme.Update[ S ], Grapheme[ S ]] {
      graph =>

      override def toString = "Grapheme" + id

      def modifiableOption : Option[ Modifiable[ S ]] = Some( this )

      def changed: Event[ S, Grapheme.Update[ S ], Grapheme[ S ]] = this

      // ---- forwarding to pin ----

      def add( elem: TimedElem[ S ] )( implicit tx: S#Tx ) { pin.add( elem )}

      def remove( elem: TimedElem[ S ] )( implicit tx: S#Tx ) : Boolean = pin.remove( elem )

      def clear()( implicit tx: S#Tx ) { pin.clear() }

      def at( time: Long )( implicit tx: S#Tx ) : Option[ TimedElem[ S ]] = pin.at( time )

      def nearestEventAfter( time: Long )( implicit tx: S#Tx ) : Option[ Long ] = pin.nearestEventAfter( time )

      // ---- extensions ----

      def valueAt( time: Long )( implicit tx: S#Tx ) : Option[ Value ] = {
         pin.floor( time ).map( _.magValue )
      }

      def segment( time: Long )( implicit tx: S#Tx ) : Option[ Segment ] = {
         pin.floor( time ).map { elem =>
            val (floorTime, floorVal) = elem.value
            floorVal match {
               case floorCurve: Value.Curve =>
                  val floorCurveVals: IIdxSeq[ Double ] = floorCurve.values.map( _._1 )( breakOut )
                  pin.ceil( time + 1 ) match {
                     case Some( ceilElem ) =>
                        val (ceilTime, ceilVal) = ceilElem.value
                        val span = Span( floorTime, ceilTime )
                        ceilVal match {
                           case ceilCurve: Value.Curve if ceilCurve.numChannels == floorCurve.numChannels =>
                              val curveValues = floorCurveVals.zip( ceilCurve.values ).map {
                                 case (floor, (ceil, shape)) => (floor, ceil, shape)
                              }
                              Segment.Curve( span, curveValues )
                           case _ =>
                              Segment.Const( span, floorCurveVals )
                        }
                     case None =>
                        Segment.Const( Span.from( floorTime ), floorCurveVals )
                  }

               case av: Value.Audio =>
                  val span = pin.nearestEventAfter( time + 1 ) match {
                     case Some( ceilTime )   => Span( floorTime, ceilTime )
                     case _                  => Span.from( floorTime )
                  }
                  Segment.Audio( span, av )
            }
         }
      }

      private def valueFromFloor( floorTime: Long, floorHolder: Elem[ S ])( implicit tx: S#Tx ) : Value = {
         ???
//         floorHolder.value match {
//            case floorCurve @ Elem.Curve( floorValues @ _* ) =>
//               val floorValuesVal: IIdxSeq[ Double ] = floorValues.map( _._1.value )( breakOut )
//               curveValueFromFloor( floorTime, floorValuesVal )
//
//            case Elem.Audio( artifact, spec, offset, gain ) =>
//               val offsetVal  = offset.value
//               val gainVal    = gain.value
//               val span       = pin.nearestEventAfter( floorTime + 1 ) match {
//                  case Some( ceilTime )   => Span( floorTime, ceilTime )
//                  case _                  => Span.from( floorTime )
//               }
//               Value.Audio( span, artifact, spec, offsetVal, gainVal )
//            }
      }

      private def curveValueFromFloor( floorTime: Long, floorValues: IIdxSeq[ Double ])( implicit tx: S#Tx ) : Value = {
         ???
//         pin.ceil( floorTime + 1 ) match {
//            case Some( (ceilTime, ceilHolder) ) =>
//               curveValue( floorTime, floorValues, ceilTime, ceilHolder )
//            case _ =>
//               Value.Const( Span.from( floorTime ), floorValues: _* )
//         }
      }

//      private def curveValue( floorTime: Long, floorValues: IIdxSeq[ Double ], ceilTime: Long, ceilHolder: Elem[ S ])( implicit tx: S#Tx ) : Value = {
//         val span = Span( floorTime, ceilTime )
//         ceilHolder.value match {
//            case ceilCurve @ Elem.Curve( ceilValues @ _* ) if floorValues.size == ceilValues.size =>
//               val segmValues = (floorValues zip ceilValues).map { case (startVal, (stop, shape)) =>
//                  (startVal, stop.value, shape)
//               }
//               Value.Segment( span, segmValues: _* )
//            case _ =>
//               Value.Const( span, floorValues: _* )
//         }
//      }

      // ---- node and event ----

      protected def reader: evt.Reader[ S, Grapheme[ S ]] = serializer[ S ]

      def connect()( implicit tx: S#Tx ) {
         pin.changed ---> this
      }

      def disconnect()( implicit tx: S#Tx ) {
         pin.changed -/-> this
      }

      def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Grapheme.Update[ S ]] = {
         None // ???
//         pin.changed.pullUpdate( pull ).map {
//            // the BiPin.Collection update assumes the 'pin' character
//            // of the elements. that means that for example, an insertion
//            //
//            // |------I-------|
//            // floor          ceil
//            //
//            // will fire the dirty region
//            //
//            //        I-------|
//            //
//            // but the grapheme will change significantly more when curves are involved:
//            // - if `I` is `Audio`
//            //   - it may have been that ceil was a curve, and thus a segment was
//            //     stretching from floorTime to ceilTime, which is now replaced by
//            //     a constant from floorTime to I-time. Note that because multiple changes
//            //     might be collapsed, we should not assume that the segment was constructed
//            //     with the value now found at ceilTime. Instead, pessimistically, we will
//            //     always assume that the span floor--I must be fired.
//            //   - the span I--ceil is straight forward and does not need additional evaluations
//            // - if `I` is `Curve`
//            //   - the dirty region floor--I must be fired as above.
//            //   - additionally the span I--ceil must be resolved by looking at the holder at
//            //     ceilTime: if ceilTime is undefined, or if it is `Audio`, the span I--ceil is a
//            //     constant, otherwise if it is `Curve` we need to calculate the curve segment.
//            case BiPin.Collection( _, changes ) =>
//               // changes = IIdxSeq[ (Span.HasStart, ElemHolder) ]
//               val values = changes.map { case (changeSpan, changeHolder) =>
//                  val changeTime = changeSpan.start
////                  pin.floor( changeTime - 1 ).map {
////                     case (floorTime, floorHolder) =>
////                        floorHolder.value match {
////                           case floorCurve @ Elem.Curve( floorValues @ _* ) =>
////                              val floorValuesVal: IIdxSeq[ Double ] = floorValues.map( _._1.value )( breakOut )
////                              curveValue( floorTime, floorValuesVal, changeTime, changeHolder )
////
////                           case Elem.Audio( artifact, spec, offset, gain ) =>
////                              val offsetVal  = offset.value
////                              val gainVal    = gain.value
////                              val floorSpan  = Span( floorTime, changeTime )
////                              Value.Audio( floorSpan, artifact, spec, offsetVal, gainVal )
////                           }
////
////                  }
////                  ...
//
//                  valueFromFloor( changeTime, changeHolder )
//               }
//               Grapheme.Update( graph, values )
//
//            case BiPin.Element( _, changes ) =>
//               // changes = IIdxSeq[ (ElemHolder, ElemHolderUpdate)
//               val values = changes.flatMap { case (elem, upd) =>
//                  upd match {
//                     case CurveHolderUpdate( nowTime, /* before, */ nowValues ) =>
//                        // before and now: : IIdxSeq[ (Double, Env.ConstShape) ]
//                        val right   = curveValueFromFloor( nowTime, nowValues.map( _._1 )) :: Nil
//                        pin.floor( nowTime - 1 ) match {
//                           case Some( (floorTime, floorHolder) ) =>
//                              floorHolder.value match {
//                                 case floorCurve @ Elem.Curve( floorValues @ _* ) if floorValues.size == nowValues.size =>
//                                    val segmValues = (floorValues zip nowValues).map { case (start, (stopVal, shape)) =>
//                                       (start._1.value, stopVal, shape)
//                                    }
//                                    Value.Segment( Span( floorTime, nowTime ), segmValues: _* ) :: right
//                                 case _ => right
//                              }
//                           case _ => right
//                        }
//
//                     case AudioHolderUpdate( startTime, artifact, spec, nowOffset, nowGain ) =>
//                        // before/now offset: Long, before/now gain: Double
//                        val span = pin.nearestEventAfter( startTime + 1 ) match {
//                           case Some( stopTime ) => Span( startTime, stopTime )
//                           case _                => Span.From( startTime )
//                        }
//                        Value.Audio( span, artifact, spec, nowOffset, nowGain ) :: Nil
//                  }
//               }
//               Grapheme.Update( graph, values )
//         }
      }

      protected def disposeData()( implicit tx: S#Tx ) {
         pin.dispose()
      }

      protected def writeData( out: DataOutput ) {
         pin.write( out )
      }
   }
}