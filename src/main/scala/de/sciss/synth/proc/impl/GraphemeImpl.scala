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

import de.sciss.lucre.{event => evt, Writable, bitemp, expr, DataOutput, stm, DataInput}
import stm.{Serializer, Sys}
import annotation.switch
import expr.Expr
import bitemp.{Span, BiPin}
import de.sciss.synth.expr.{Doubles, Longs}
import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import evt.EventLikeSerializer

object GraphemeImpl {
   import Grapheme.{Elem, Value, Modifiable}
   import Elem.{Audio, Curve}

   private implicit val time = Longs

   def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Grapheme[ S ] = {
      serializer[ S ].read( in, access )
   }

   implicit def serializer[ S <: Sys[ S ]] : evt.NodeSerializer[ S, Grapheme[ S ]] =
      anySer.asInstanceOf[ evt.NodeSerializer[ S, Grapheme[ S ]]]

   private val anySer = new Ser[ I ]

   private final class Ser[ S <: Sys[ S ]] extends evt.NodeSerializer[ S, Grapheme[ S ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Grapheme[ S ] = {
         val pin = BiPin.Modifiable.read[ S, ElemHolder[ S ], ElemHolderUpdate ]( identity )( in, access )
         new Impl( targets, pin )
      }
   }

   def modifiable[ S <: Sys[ S ]]( implicit tx: S#Tx ) : Modifiable[ S ] = {
      val targets = evt.Targets[ S ]   // XXX TODO: partial?
      val pin     = BiPin.Modifiable[ S, ElemHolder[ S ], ElemHolderUpdate ]( identity )
      new Impl( targets, pin )
   }

   private implicit def elemSerializer[ S <: Sys[ S ]] : EventLikeSerializer[ S, ElemHolder[ S ]] =
      anyElemSer.asInstanceOf[ EventLikeSerializer[ S, ElemHolder[ S ]]]

   private val anyElemSer = new ElemSer[ I ]

   // ---- ElemHolder ----

   private sealed trait ElemHolderUpdate
   private final case class CurveHolderUpdate( before: IIdxSeq[ (Double, Env.ConstShape) ],
                                               now:    IIdxSeq[ (Double, Env.ConstShape) ])
   extends ElemHolderUpdate

   private final case class AudioHolderUpdate( beforeOffset: Long, beforeGain: Double,
                                               nowOffset: Long, nowGain: Double ) extends ElemHolderUpdate

   private sealed trait ElemHolder[ S <: Sys[ S ]]
   extends evt.EventLike[ S, ElemHolderUpdate, ElemHolder[ S ]] with Writable {
      def value: Elem[ S ]
   }

   private final val curveCookie = 0
   private final val audioCookie = 1

   private sealed trait CurveHolder[ S <: Sys[ S ]] extends ElemHolder[ S ] {
      def value: Curve[ S ]

      final protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( curveCookie )
         val idx = value.values.toIndexedSeq
         out.writeInt( idx.size )
         idx.foreach { tup =>
            tup._1.write( out )
            CommonSerializers.EnvConstShape.write( tup._2, out )
         }
      }
   }

   private sealed trait AudioHolder[ S <: Sys[ S ]] extends ElemHolder[ S ] {
      def value: Audio[ S ]

      final protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( audioCookie )
         value.artifact.write( out )
         CommonSerializers.AudioFileSpec.write( value.spec, out )
         value.offset.write( out )
         value.gain.write( out )
      }
   }

   private final case class ConstCurve[ S <: Sys[ S ]]( value: Curve[ S ])
   extends CurveHolder[ S ] with evt.Dummy[ S, ElemHolderUpdate, ElemHolder[ S ]] with evt.Constant[ S ]

   private final class MutableCurve[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ], val value: Curve[ S ])
   extends CurveHolder[ S ] with evt.StandaloneLike[ S, ElemHolderUpdate, ElemHolder[ S ]] {
      protected def reader : evt.Reader[ S, ElemHolder[ S ]] = elemSerializer[ S ]

      def connect()( implicit tx: S#Tx ) {
         value.values.foreach( tup => tup._1.changed ---> this )
      }

      def disconnect()( implicit tx: S#Tx ) {
         value.values.foreach( tup => tup._1.changed -/-> this )
      }

      def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ ElemHolderUpdate ] = {
         // parent is Expr[ S, Double ] coming from one or more of the curve's values
         val valueChanges: IIdxSeq[ ((Double, Env.ConstShape), (Double, Env.ConstShape))] =
            value.values.map({ case (mag, shape) =>
               val magEvt  = mag.changed
               val magCh   = if( magEvt.isSource( pull )) magEvt.pullUpdate( pull ) else None
               magCh match {
                  case Some( evt.Change( oldMag, newMag )) => (oldMag, shape) -> (newMag, shape)
                  case None =>
                     val flat = (mag.value, shape)
                     flat -> flat
               }
            })( breakOut )
         val (before, now) = valueChanges.unzip
         if( before != now ) {
            Some( CurveHolderUpdate( before, now ))
         } else None
      }

      protected def disposeData()( implicit tx: S#Tx ) {}
   }

   private final case class ConstAudio[ S <: Sys[ S ]]( value: Audio[ S ])
   extends AudioHolder[ S ] with evt.Dummy[ S, ElemHolderUpdate, ElemHolder[ S ]] with evt.Constant[ S ]

   private final class MutableAudio[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ], val value: Audio[ S ])
   extends AudioHolder[ S ] with evt.StandaloneLike[ S, ElemHolderUpdate, ElemHolder[ S ]] {
      protected def reader : evt.Reader[ S, ElemHolder[ S ]] = elemSerializer[ S ]

      def connect()( implicit tx: S#Tx ) {
         value.offset.changed ---> this
         value.gain.changed   ---> this
      }

      def disconnect()( implicit tx: S#Tx ) {
         value.offset.changed -/-> this
         value.gain.changed   -/-> this
      }

      def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ ElemHolderUpdate ] = {
         val offset     = value.offset
         val gain       = value.gain
         val offsetEvt  = offset.changed
         val gainEvt    = gain.changed
         val offsetCh   = if( offsetEvt.isSource( pull )) offsetEvt.pullUpdate( pull ) else None
         val gainCh     = if( gainEvt.isSource(   pull )) gainEvt.pullUpdate(   pull ) else None

         val (oldOffset, newOffset) = offsetCh match {
            case Some( evt.Change( _old, _new )) => _old -> _new
            case None =>
               val offsetVal = offset.value
               offsetVal -> offsetVal
         }

         val (oldGain, newGain) = gainCh match {
            case Some( evt.Change( _old, _new )) => _old -> _new
            case None =>
               val gainVal = gain.value
               gainVal -> gainVal
         }

         if( oldOffset != newOffset || oldGain != newGain ) {
            Some( AudioHolderUpdate( oldOffset, oldGain, newOffset, newGain ))
         } else None
      }

      protected def disposeData()( implicit tx: S#Tx ) {}
   }

   private final class ElemSer[ S <: Sys[ S ]] extends EventLikeSerializer[ S, ElemHolder[ S ]] {
      def readConstant( in: DataInput )( implicit tx: S#Tx ) : ElemHolder[ S ] = {
         (in.readUnsignedByte(): @switch) match {
            case `curveCookie` =>
               val sz      = in.readInt()
               val values  = IIdxSeq.fill( sz ) {
                  val value   = Doubles.readConst[ S ]( in )
                  val shape   = CommonSerializers.EnvConstShape.read( in )
                  value -> shape
               }
               val curve = Curve( values: _* )
               ConstCurve( curve )

            case `audioCookie` =>
               val artifact   = Artifact.read( in )
               val spec       = CommonSerializers.AudioFileSpec.read( in )
               val offset     = Longs.readConst[ S ]( in )
               val gain       = Doubles.readConst[ S ]( in )
               val audio   = Audio( artifact, spec, offset, gain )
               ConstAudio( audio )

            case other => sys.error( "Unexpected cookie " + other )
         }
      }

      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : ElemHolder[ S ] with evt.Node[ S ] = {
         (in.readUnsignedByte(): @switch) match {
            case `curveCookie` =>
               val sz      = in.readInt()
               val values  = IIdxSeq.fill( sz ) {
      //                  val value   = Doubles.readConst[ S ]( in )
                  val value   = Doubles.readExpr( in, access )
                  val shape   = CommonSerializers.EnvConstShape.read( in )
                  value -> shape
               }
               val curve = Curve( values: _* )
               new MutableCurve( targets, curve )

            case `audioCookie` =>
               val artifact   = Artifact.read( in )
               val spec       = CommonSerializers.AudioFileSpec.read( in )
      //               val offset     = Longs.readConst[ S ]( in )
               val offset     = Longs.readExpr[ S ]( in, access )
      //               val gain       = Doubles.readConst[ S ]( in )
               val gain       = Doubles.readExpr[ S ]( in, access )
               val audio   = Audio( artifact, spec, offset, gain )
               new MutableAudio( targets, audio )

            case other => sys.error( "Unexpected cookie " + other )
         }
      }
   }

   // ---- actual implementation ----

   private final class Impl[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ],
                                             pin: BiPin.Modifiable[ S, ElemHolder[ S ], ElemHolderUpdate ])
   extends Modifiable[ S ] with evt.StandaloneLike[ S, Grapheme.Update[ S ], Grapheme[ S ]] {
      override def toString = "Grapheme" + pin.id

      def modifiableOption : Option[ Modifiable[ S ]] = Some( this )

      private def wrap( elem: Elem[ S ])( implicit tx: S#Tx ) : ElemHolder[ S ] = elem match {
         case curve @ Curve( _ ) =>
            if( curve.isConstant ) ConstCurve( curve ) else new MutableCurve( evt.Targets[ S ], curve )  // XXX TODO partial?
         case audio @ Audio( _, _, _, _ ) =>
            if( audio.isConstant ) ConstAudio( audio ) else new MutableAudio( evt.Targets[ S ], audio )  // XXX TODO partial?
      }

      // ---- forwarding to pin ----

      def add( time: Expr[ S, Long ], elem: Elem[ S ])( implicit tx: S#Tx ) {
         pin.add( time, wrap( elem ))
      }

      def remove( time: Expr[ S, Long ], elem: Elem[ S ])( implicit tx: S#Tx ) : Boolean = {
         val timeVal = time.value
         pin.intersect( timeVal ).find({ case (time2, hold) => time2 == time && hold.value == elem }) match {
            case Some( (time2, hold) ) => pin.remove( time2, hold )
            case _ => false
         }
//         pin.remove( time, elem )
      }

      def clear()( implicit tx: S#Tx ) {
         pin.clear()
      }

      def at( time: Long )( implicit tx: S#Tx ) : Option[ Elem[ S ]] = pin.at( time ).map( _.value )

      // ---- extensions ----

      def valueAt( time: Long )( implicit tx: S#Tx ) : Option[ Value ] = {
         pin.floor( time ).map { case (floorTime, floorHolder) =>
            floorHolder.value match {
               case floorCurve @ Elem.Curve( floorValues @ _* ) =>
                  val floorValuesVal = floorValues.map( _._1.value )
                  pin.ceil( time + 1 ) match {
                     case Some( (ceilTime, ceilHolder) ) =>
                        val span = Span( floorTime, ceilTime )
                        ceilHolder.value match {
                           case ceilCurve @ Elem.Curve( ceilValues @ _* ) if floorCurve.numChannels == ceilCurve.numChannels =>
                              val segmValues    = (floorValuesVal zip ceilValues).map { case (startVal, (stop, shape)) =>
                                 (startVal, stop.value, shape)
                              }
                              Value.Segment( span, segmValues: _* )
                           case _ =>
                              Value.Const( span, floorValuesVal: _* )
                        }
                     case _ =>
                        Value.Const( Span.from( floorTime ), floorValuesVal: _* )
                  }

               case Elem.Audio( artifact, spec, offset, gain ) =>
                  val offsetVal  = offset.value
                  val gainVal    = gain.value
                  val span       = pin.nearestEventAfter( time + 1 ) match {
                     case Some( ceilTime )   => Span( floorTime, ceilTime )
                     case _                  => Span.from( floorTime )
                  }
                  Value.Audio( span, artifact, spec, offsetVal, gainVal )
            }
         }
      }

      // ---- node and event ----

      protected def reader: evt.Reader[ S, Grapheme[ S ]] = serializer[ S ]

      def connect()( implicit tx: S#Tx ) {
         pin.changed ---> this
      }

      def disconnect()( implicit tx: S#Tx ) {
         pin.changed -/-> this
      }

      def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Grapheme.Update[ S ]] = {
         pin.changed.pullUpdate( pull ).map {
            case BiPin.Collection( _, changes ) => ???
            case BiPin.Element(    _, changes ) => ???
         }
      }

      protected def disposeData()( implicit tx: S#Tx ) {
         pin.dispose()
      }

      protected def writeData( out: DataOutput ) {
         pin.write( out )
      }
   }
}