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
import evt.EventLikeSerializer
import annotation.switch
import expr.Expr
import io.AudioFileSpec
import bitemp.BiPin
import de.sciss.synth.expr.Longs

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
   }

   def curveElem[ S <: Sys[ S ]]( values: (Expr[ S, Double ], Env.ConstShape)* )( implicit tx: S#Tx ) : Curve[ S ] = {
      val idx     = values.toIndexedSeq
      val const   = idx.collect { case (Expr.Const( c ), shape) => c -> shape }
      if( const.size == idx.size ) {   // all constant

      } else {

      }

      ???
//      if( targetLevel.isInstanceOf[ Expr.Const[ _, _ ]]) {
//         Const( targetLevel, shape )
//      } else {
//         val tgt = evt.Targets.partial[ S ]
//         new Mut( tgt, targetLevel, shape )
//      }
   }

//   def audioElem[ S <: Sys[ S ]]( artifact: Artifact, spec: AudioFileSpec, offset: Expr[ S, Long ], gain: Expr[ S, Double ])
//                            ( implicit tx: S#Tx ) : Audio[ S ] = ???

   def modifiable[ S <: Sys[ S ]]( implicit tx: S#Tx ) : Modifiable[ S ] = {
      ???
//      implicit val time = Longs
//      BiPin.Modifiable( _.changed ) // ( tx, Elem.serializer[ S ], Longs )
   }

   private final class ElemSer[ S <: Sys[ S ]] extends EventLikeSerializer[ S, ElemHolder[ S ]] {
//         def write( elem: Elem[ S ], out: DataOutput ) { elem.write( out )}

      private def readShape( in: DataInput ) : Env.ConstShape = {
         (in.readInt(): @switch) match {
            case stepShape.id    => stepShape
            case linShape.id     => linShape
            case expShape.id     => expShape
            case sinShape.id     => sinShape
            case welchShape.id   => welchShape
            case curveShape.id   => curveShape( in.readFloat() )
            case sqrShape.id     => sqrShape
            case cubShape.id     => cubShape
            case other           => sys.error( "Unexpected shape ID " + other )
         }
      }

      def readConstant( in: DataInput )( implicit tx: S#Tx ) : ElemHolder[ S ] = {
         ???
//         (in.readUnsignedByte(): @switch) match {
//            case Mono.cookie =>
//               require( in.readUnsignedByte() == 3, "Expected constant Expr" )   // XXX TODO bad... should have Expr.Const.cookie
//               val targetLevel   = Doubles.serializer[ S ].readConstant( in )
//               val shape         = readShape( in )
//               Mono.Const( targetLevel, shape )
//
//            case Synthesis.cookie =>
//               synthesis[ S ]
//
////               case Embedded.cookie =>
//
//            case other => sys.error( "Unexpected cookie " + other )
//         }
      }

      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : ElemHolder[ S ] = {
         ???
//         (in.readUnsignedByte(): @switch) match {
//            case Mono.cookie =>
//               val targetLevel   = Doubles.readExpr( in, access )
//               val shape         = readShape( in )
//               new Mono.Mut( targets, targetLevel, shape )
//
//            case Embedded.cookie =>
////                  val ref           = Grapheme.read( in, access )
//
////                  val ref           = BiGroup.TimedElem.read[ S, Proc[ S ], Proc.Update[ S ]]( in, access )
//               val refID         = tx.readID( in, access )
//               val refSpan       = SpanLikes.readExpr( in, access )
//               val refVal        = Proc.read( in, access )
//               val ref           = BiGroup.TimedElem[ S, Proc[ S ], Proc.Update[ S ]]( refID, refSpan, refVal )
//
//               val key           = in.readString()
//               val offset        = Longs.readExpr( in, access )
//               new Embedded.Impl( targets, ref, key, offset )
//
//            case other => sys.error( "Unexpected cookie " + other )
//         }
      }
   }

   private final class Impl[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ],
                                             pin: BiPin.Modifiable[ S, ElemHolder[ S ], Elem.Update[ S ]])
   extends Modifiable[ S ] {
      override def toString = "Grapheme" + pin.id

      def modifiableOption : Option[ Modifiable[ S ]] = Some( this )

      private def wrap( elem: Elem[ S ])( implicit tx: S#Tx ) : ElemHolder[ S ] = ???

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