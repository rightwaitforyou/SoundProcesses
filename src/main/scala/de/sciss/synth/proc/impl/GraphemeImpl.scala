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
import bitemp.BiPin
import de.sciss.synth.expr.{Doubles, Longs}
import collection.immutable.{IndexedSeq => IIdxSeq}

object GraphemeImpl {
   import Grapheme.{Elem, Value, Modifiable}
   import Elem.{Audio, Curve}

   private val anySer = new Ser[ I ]

   private implicit val time = Longs

   def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Grapheme[ S ] = {
      serializer[ S ].read( in, access )
   }

   implicit def serializer[ S <: Sys[ S ]] : Serializer[ S#Tx, S#Acc, Grapheme[ S ]] =
      anySer.asInstanceOf[ Serializer[ S#Tx, S#Acc, Grapheme[ S ]]]

   private implicit def elemSerializer[ S <: Sys[ S ]] : EventLikeSerializer[ S, ElemHolder[ S ]] =
      anyElemSer.asInstanceOf[ EventLikeSerializer[ S, ElemHolder[ S ]]]

   private val anyElemSer = new Ser[ I ]

   private final class Ser[ S <: Sys[ S ]] extends evt.NodeSerializer[ S, Grapheme[ S ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Grapheme[ S ] = {
         val pin = BiPin.Modifiable.read[ S, ElemHolder[ S ], Elem.Update[ S ]]( identity )( in, access )
         new Impl( targets, pin )
      }
   }

   private sealed trait ElemHolder[ S <: Sys[ S ]]
   extends evt.EventLike[ S, Elem.Update[ S ], ElemHolder[ S ]] with Writable {
      def value: Elem[ S ]
      protected def headerCookie: Int
   }

   def modifiable[ S <: Sys[ S ]]( implicit tx: S#Tx ) : Modifiable[ S ] = {
      val targets = evt.Targets[ S ]   // XXX TODO: partial?
      val pin     = BiPin.Modifiable[ S, ElemHolder[ S ], Elem.Update[ S ]]( identity )
      new Impl( targets, pin )
   }

   private final val curveCookie = 0
   private final val audioCookie = 1
   private final val constCookie = 0
   private final val mutableCookie = 1

   private sealed trait CurveHolder[ S <: Sys[ S ]] extends ElemHolder[ S ] {
      def value: Curve[ S ]

      final def write( out: DataOutput ) {
         out.writeUnsignedByte( headerCookie )
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

      final def write( out: DataOutput ) {
         out.writeUnsignedByte( headerCookie )
         out.writeUnsignedByte( audioCookie )
         value.artifact.write( out )
         CommonSerializers.AudioFileSpec.write( value.spec, out )
         value.offset.write( out )
         value.gain.write( out )
      }
   }

   private final case class ConstCurve[ S <: Sys[ S ]]( value: Curve[ S ])
   extends CurveHolder[ S ] with evt.Dummy[ S, Elem.Update[ S ], ElemHolder[ S ]] {
      protected def headerCookie = constCookie
   }

   private final case class MutableCurve[ S <: Sys[ S ]]( node: evt.Node[ S ], value: Curve[ S ])
   extends CurveHolder[ S ]
   with evt.EventImpl[ S, Elem.Update[ S ], Elem.Update[ S ], ElemHolder[ S ]]
   with evt.InvariantEvent[ S, Elem.Update[ S ], ElemHolder[ S ]] with evt.Reader {
      protected def headerCookie = mutableCookie

      def reader: evt.Reader[ S, ElemHolder[ S ]] = elemSerializer[ S ]
      def slot = 0

      def connect()( implicit tx: S#Tx ) {
         value.values.foreach( tup => evt.Intruder.--->( tup._1.changed, this ))
      }

      def disconnect()( implicit tx: S#Tx ) {
         value.values.foreach( tup => evt.Intruder.-/->( tup._1.changed, this ))
      }

      def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Elem.Update[ S ]] = {
         ???
      }
   }

   private final case class ConstAudio[ S <: Sys[ S ]]( value: Audio[ S ])
   extends AudioHolder[ S ] with evt.Dummy[ S, Elem.Update[ S ], ElemHolder[ S ]] { // with evt.Constant[ S ]
      protected def headerCookie = constCookie
   }

//   private final class ElemSer[ S <: Sys[ S ]] extends EventLikeSerializer[ S, ElemHolder[ S ]]
   private final class ElemSer[ S <: Sys[ S ]] extends Serializer[ S#Tx, S#Acc, ElemHolder[ S ]] {
      def write( elem: ElemHolder[ S ], out: DataOutput ) { elem.write( out )}

      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : ElemHolder[ S ] = {
         (in.readUnsignedByte(): @switch) match {
            case `curveCookie` =>
               val sz      = in.readInt()
               val values  = IIdxSeq.fill( sz ) {
//                  val value   = Doubles.readConst[ S ]( in )
                  val value   = Doubles.readExpr( in, access )
                  val shape   = CommonSerializers.EnvConstShape.read( in )
                  value -> shape
               }
               ConstCurve( Curve( values: _* ))

            case `audioCookie` =>
               val artifact   = Artifact.read( in )
               val spec       = CommonSerializers.AudioFileSpec.read( in )
//               val offset     = Longs.readConst[ S ]( in )
               val offset     = Longs.readExpr[ S ]( in, access )
//               val gain       = Doubles.readConst[ S ]( in )
               val gain       = Doubles.readExpr[ S ]( in, access )
               ConstAudio( Audio( artifact, spec, offset, gain ))

            case other => sys.error( "Unexpected cookie " + other )
         }
      }

//      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : ElemHolder[ S ] = {
//         (in.readUnsignedByte(): @switch) match {
//            case `curveCookie` =>
//               val sz      = in.readInt()
//               val values  = IIdxSeq.fill( sz ) {
//                  val value   = Doubles.readExpr( in, access )
//                  val shape   = CommonSerializers.EnvConstShape.read( in )
//                  value -> shape
//               }
//               Curve( values: _* )
//               ??? // new MutableElem( )
//
//            case `audioCookie` =>
//               val artifact   = Artifact.read( in )
//               val spec       = CommonSerializers.AudioFileSpec.read( in )
//               val offset     = Longs.readExpr( in, access )
//               val gain       = Doubles.readExpr( in, access )
//               Audio( artifact, spec, offset, gain )
//               ??? // new ConstElem( )
//
//            case other => sys.error( "Unexpected cookie " + other )
//         }
//      }
   }

   private final class Impl[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ],
                                             pin: BiPin.Modifiable[ S, ElemHolder[ S ], Elem.Update[ S ]])
   extends Modifiable[ S ] {
      override def toString = "Grapheme" + pin.id

      def modifiableOption : Option[ Modifiable[ S ]] = Some( this )

      private def wrap( elem: Elem[ S ])( implicit tx: S#Tx ) : ElemHolder[ S ] = elem match {
         case curve @ Curve( _ ) =>
            if( curve.isConstant ) ConstCurve( curve ) else ???
         case audio @ Audio( _, _, _, _ ) =>
            if( audio.isConstant ) ConstAudio( audio ) else ???
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

      def valueAt( time: Long )( implicit tx: S#Tx ) : Option[ Value[ S ]] = {
         ???
      }

      // ---- evt.Node ----

      protected def disposeData()( implicit tx: S#Tx ) {
         pin.dispose()
      }

      protected def writeData( out: DataOutput ) {
         pin.write( out )
      }

      def select( slot: Int, invariant: Boolean ) : evt.NodeSelector[ S, _ ] = ???
   }
}