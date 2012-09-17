package de.sciss.synth
package proc
package impl

import de.sciss.lucre.{event => evt, DataOutput, stm, bitemp, DataInput}
import stm.{Serializer, Sys}
import bitemp.BiGroup
import evt.EventLikeSerializer
import annotation.switch
import expr.{Longs, SpanLikes, Doubles}
import de.sciss.lucre.expr.Expr

object GraphemeImpl {
   import Grapheme.{Elem, Modifiable}
   import Elem.Curve

   private val anySer = new Ser[ I ]

   def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Grapheme[ S ] = {
//      implicit val time = Longs
//      val pin = BiPin.read[ S, Elem[ S ], Elem.Update[ S ]]( _.changed )( in, access )
//      new Impl( pin )
      ???
   }

   implicit def serializer[ S <: Sys[ S ]] : Serializer[ S#Tx, S#Acc, Grapheme[ S ]] =
      anySer.asInstanceOf[ Serializer[ S#Tx, S#Acc, Grapheme[ S ]]]

   implicit def elemSerializer[ S <: Sys[ S ]] : EventLikeSerializer[ S, Elem[ S ]] =
      anyElemSer.asInstanceOf[ EventLikeSerializer[ S, Elem[ S ]]]

   private val anyElemSer = new Ser[ I ]

   private final class Ser[ S <: Sys[ S ]] extends Serializer[ S#Tx, S#Acc, Grapheme[ S ]] {
      def write( g: Grapheme[ S ], out: DataOutput ) {
         ???
//         g.pin.write( out )
      }

      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Grapheme[ S ] = {
////         implicit val elemSer = Elem.serializer[ S ]
//         implicit val time    = Longs
//         val pin = BiPin.read[ S, Elem[ S ], Elem.Update[ S ]]( _.changed )( in, access )
//         new Impl( pin )
         ???
      }
   }

   def curve[ S <: Sys[ S ]]( values: (Expr[ S, Double ], Env.ConstShape)* )( implicit tx: S#Tx ) : Curve[ S ] = {
      ???
//      if( targetLevel.isInstanceOf[ Expr.Const[ _, _ ]]) {
//         Const( targetLevel, shape )
//      } else {
//         val tgt = evt.Targets.partial[ S ]
//         new Mut( tgt, targetLevel, shape )
//      }
   }

   def modifiable[ S <: Sys[ S ]]( implicit tx: S#Tx ) : Modifiable[ S ] = {
      ???
//      implicit val time = Longs
//      BiPin.Modifiable( _.changed ) // ( tx, Elem.serializer[ S ], Longs )
   }

   private final class ElemSer[ S <: Sys[ S ]] extends EventLikeSerializer[ S, Elem[ S ]] {
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

      def readConstant( in: DataInput )( implicit tx: S#Tx ) : Elem[ S ] = {
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

      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Elem[ S ] = {
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

//   private final class Impl[ S <: Sys[ S ]]( val pin: BiPin[ S, Elem[ S ], Elem.Update[ S ]])
//   extends Grapheme[ S ] with scala.Proxy {
//      override def toString = "Grapheme" + pin.id
//
//      def self: Any = pin
//   }
}