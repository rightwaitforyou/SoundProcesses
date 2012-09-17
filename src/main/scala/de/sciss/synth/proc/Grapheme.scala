/*
 *  Grapheme.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.stm.{Serializer, Sys}
import de.sciss.lucre.expr.Expr
import de.sciss.synth.{cubShape, sqrShape, welchShape, sinShape, expShape, linShape, stepShape, curveShape, Env}
import de.sciss.lucre.bitemp.{BiGroup, BiPin}
import de.sciss.synth.expr.{SpanLikes, Spans, Longs, Doubles}
import de.sciss.lucre.{Writable, DataOutput, DataInput, event => evt}
import evt.{EventLikeSerializer, EventLike}
import annotation.switch
import collection.immutable.{IndexedSeq => IIdxSeq}

object Grapheme {
   type Update[ S <: Sys[ S ]] = BiPin.Update[ S, Elem[ S ], Elem.Update[ S ]]

//   implicit def serializer[ S <: Sys[ S ]] : Serializer[ S#Tx, S#Acc, Grapheme[ S ]] = {
//      implicit val elemSer = Elem.serializer[ S ]
//      implicit val time    = Longs
//      BiPin.serializer[ S, Elem[ S ], Elem.Update[ S ]]( _.changed )
//   }

   implicit def serializer[ S <: Sys[ S ]] : Serializer[ S#Tx, S#Acc, Grapheme[ S ]] =
      anySer.asInstanceOf[ Serializer[ S#Tx, S#Acc, Grapheme[ S ]]]

   private val anySer = new Ser[ I ]

   private final class Ser[ S <: Sys[ S ]] extends Serializer[ S#Tx, S#Acc, Grapheme[ S ]] {
      def write( g: Grapheme[ S ], out: DataOutput ) {
         g.pin.write( out )
      }

      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Grapheme[ S ] = {
//         implicit val elemSer = Elem.serializer[ S ]
         implicit val time    = Longs
         val pin = BiPin.read[ S, Elem[ S ], Elem.Update[ S ]]( _.changed )( in, access )
         new Impl( pin )
      }
   }

   private final class Impl[ S <: Sys[ S ]]( val pin: BiPin[ S, Elem[ S ], Elem.Update[ S ]])
   extends Grapheme[ S ] with scala.Proxy {
      override def toString = "Grapheme" + pin.id

      def self: Any = pin
   }

   object Value {

      /**
       * A monophonic constant value
       */
      final case class MonoConst( value: Float /*, stop: Long */)
      extends Value[ Nothing ] // { def numChannels = 1 }

      /**
       * A monophonic envelope segment
       *
       * @param start   value at start of segment
       * @param stop    target value of segment
       * @param dur     duration of segment in frames
       * @param shape   shape of segment
       */
      final case class MonoSegment( start: Float, stop: Float, dur: Long, shape: Env.ConstShape )
      extends Value[ Nothing ] // { def numChannels = 1 }

      /**
       * A real-time signal produced by another process
       *
       * @param timed   the source process of the signal
       * @param key     the scan key in the source process
       */
      final case class Sink[ S <: Sys[ S ]]( timed: TimedProc[ S ], key: String )
      extends Value[ S ]

      /**
       * The real-time signal produced (output) by this process
       */
      case object Source extends Value[ Nothing ]
   }

   /**
    * An evaluated and flattened scan element. This is either an immutable value such as a constant or
    * envelope segment, or a real-time signal, coming either from the same process (`Source`) or being
    * fed by another embedded process (`Sink`).
    */
   sealed trait Value[ +S ] {
//      def stop: Long
//      def numChannels: Int
   }

   object Elem {
      // Note: we do not need to carry along `elem` because the outer collection
      // (`BiPin`) already does that for us.
      sealed trait Update[ S <: Sys[ S ]] // { def elem: Elem[ S ]}
      final case class MonoChanged[ S <: Sys[ S ]]( /* elem: Mono[ S ], */ change: evt.Change[ Double ]) extends Update[ S ]
//      final case class EmbeddedChanged[ S <: Sys[ S ]]( /* elem: Embedded[ S ], */ refChange: Option[ Grapheme.Update[ S ]], offset: Long ) extends Update[ S ]
      final case class EmbeddedChanged[ S <: Sys[ S ]]( /* elem: Embedded[ S ], */ refChanges: IIdxSeq[ BiGroup.ElementUpdate[ Proc.Update[ S ]]], offset: Long ) extends Update[ S ]

      implicit def serializer[ S <: Sys[ S ]] : EventLikeSerializer[ S, Elem[ S ]] = anySer.asInstanceOf[ Ser[ S ]]

      private val anySer = new Ser[ I ]

      private final class Ser[ S <: Sys[ S ]] extends EventLikeSerializer[ S, Elem[ S ]] {
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
            (in.readUnsignedByte(): @switch) match {
               case Mono.cookie =>
                  require( in.readUnsignedByte() == 3, "Expected constant Expr" )   // XXX TODO bad... should have Expr.Const.cookie
                  val targetLevel   = Doubles.serializer[ S ].readConstant( in )
                  val shape         = readShape( in )
                  Mono.Const( targetLevel, shape )

               case Synthesis.cookie =>
                  synthesis[ S ]

//               case Embedded.cookie =>

               case other => sys.error( "Unexpected cookie " + other )
            }
         }

         def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Elem[ S ] = {
            (in.readUnsignedByte(): @switch) match {
               case Mono.cookie =>
                  val targetLevel   = Doubles.readExpr( in, access )
                  val shape         = readShape( in )
                  new Mono.Mut( targets, targetLevel, shape )

               case Embedded.cookie =>
//                  val ref           = Grapheme.read( in, access )

//                  val ref           = BiGroup.TimedElem.read[ S, Proc[ S ], Proc.Update[ S ]]( in, access )
                  val refID         = tx.readID( in, access )
                  val refSpan       = SpanLikes.readExpr( in, access )
                  val refVal        = Proc.read( in, access )
                  val ref           = BiGroup.TimedElem[ S, Proc[ S ], Proc.Update[ S ]]( refID, refSpan, refVal )

                  val key           = in.readString()
                  val offset        = Longs.readExpr( in, access )
                  new Embedded.Impl( targets, ref, key, offset )

               case other => sys.error( "Unexpected cookie " + other )
            }
         }
      }
   }
   sealed trait Elem[ S <: Sys[ S ]] extends Writable {
      def changed: EventLike[ S, Elem.Update[ S ], Elem[ S ]]
   }
   object Mono {
      private[Grapheme] final val cookie = 0

      def apply[ S <: Sys[ S ]]( targetLevel: Expr[ S, Double ], shape: Env.ConstShape = linShape )( implicit tx: S#Tx ) : Mono[ S ] = {
         if( targetLevel.isInstanceOf[ Expr.Const[ _, _ ]]) {
            Const( targetLevel, shape )
         } else {
            val tgt = evt.Targets.partial[ S ]
            new Mut( tgt, targetLevel, shape )
         }
      }

      def unapply[ S <: Sys[ S ]]( elem: Elem[ S ]) : Option[ (Expr[ S, Double ], Env.ConstShape) ] = {
         if( elem.isInstanceOf[ Mono[ _ ]]) {
            val mono = elem.asInstanceOf[ Mono[ S ]]
            Some( mono.targetLevel -> mono.shape )
         } else None
      }

      private[Grapheme] final case class Const[ S <: Sys[ S ]]( targetLevel: Expr[ S, Double ], shape: Env.ConstShape )
      extends Mono[ S ] with evt.Constant[ S ] {
         override def toString = "Mono(" + targetLevel + ", " + shape + ")"

         def changed: EventLike[ S, Elem.Update[ S ], Elem[ S ]] = evt.Dummy.apply
      }

      private[Grapheme] final class Mut[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ],
                                               val targetLevel: Expr[ S, Double ], val shape: Env.ConstShape )
      extends Mono[ S ] with evt.StandaloneLike[ S, Elem.Update[ S ], Elem[ S ]] {
         override def toString = "Mono(" + targetLevel + ", " + shape + ")"

         def changed: EventLike[ S, Elem.Update[ S ], Elem[ S ]] = this

         def reader: evt.Reader[ S, Elem[ S ]] = Elem.serializer[ S ]

         def connect()( implicit tx: S#Tx ) {
            evt.Intruder.--->( targetLevel.changed, this )
         }

         def disconnect()( implicit tx: S#Tx ) {
            evt.Intruder.-/->( targetLevel.changed, this )
         }

         protected def disposeData()( implicit tx: S#Tx ) {}

         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Elem.Update[ S ]] = {
            // XXX TODO ugly. Should have object Event { def unapply( ... )}
            evt.Intruder.pullUpdate(
               targetLevel.changed.asInstanceOf[ evt.NodeSelector[ S, evt.Change[ Double ]]], pull ).map( u =>
                  Elem.MonoChanged( /* this, */ u )
               )
         }
      }
   }
   sealed trait Mono[ S <: Sys[ S ]] extends Elem[ S ] {
      def targetLevel: Expr[ S, Double ]
      def shape: Env.ConstShape

      final protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( Mono.cookie )
         targetLevel.write( out )
         out.writeInt( shape.id )
         shape match {
            case cs: curveShape => out.writeFloat( cs.curvature )
            case _ => // only curveShape has an extra curvature argument
         }
      }
   }
//   final case class AudioFile[ S <: Sys[ S ]]( f: File, offset: Expr[ S, Long ]) extends Elem[ S ]
//   final case class Graph[ S <: Sys[ S ]]( func: Expr[ S, SynthGraph ]) extends Elem[ S ]

   private val anySynthesis = Synthesis[ I ]()

   private def synthesis[ S <: Sys[ S ]] : Synthesis[ S ] = anySynthesis.asInstanceOf[ Synthesis[ S ]]

   object Synthesis {
      private[Grapheme] final val cookie = 1
   }
   final case class Synthesis[ S <: Sys[ S ]]() extends Elem[ S ] with evt.Constant[ S ] {
      def changed: EventLike[ S, Elem.Update[ S ], Elem[ S ]] = evt.Dummy.apply

      protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( Synthesis.cookie )
      }
   }
   object Embedded {
      private[Grapheme] final val cookie = 2

//      def apply[ S <: Sys[ S ]]( ref: Scan[ S ], offset: Expr[ S, Long ])( implicit tx: S#Tx ) : Embedded[ S ]

      def apply[ S <: Sys[ S ]]( ref: TimedProc[ S ], key: String, offset: Expr[ S, Long ])( implicit tx: S#Tx ) : Embedded[ S ] = {
         val tgt = evt.Targets[ S ] // XXX TODO partial? should reflect ref.targets I guess?
         new Impl( tgt, ref, key, offset )
      }

      def unapply[ S <: Sys[ S ]]( elem: Elem[ S ]) : Option[ (TimedProc[ S ], String, Expr[ S, Long ]) ] = {
         if( elem.isInstanceOf[ Embedded[ _ ]]) {
            val embedded = elem.asInstanceOf[ Embedded[ S ]]
            Some( (embedded.ref, embedded.key, embedded.offset) )
         } else None
      }

      private[Grapheme] final class Impl[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ],
                                                       val ref: TimedProc[ S ], val key: String, val offset: Expr[ S, Long ])
      extends Embedded[ S ] with evt.StandaloneLike[ S, Elem.Update[ S ], Elem[ S ]] {
         override def toString = "Embedded(" + ref + ", " + offset + ")"

         def changed: EventLike[ S, Elem.Update[ S ], Elem[ S ]] = this

         def reader: evt.Reader[ S, Elem[ S ]] = Elem.serializer[ S ]

         def connect()( implicit tx: S#Tx ) {
//            evt.Intruder.--->( ref.changed, this )
            evt.Intruder.--->( offset.changed, this )
         }

         def disconnect()( implicit tx: S#Tx ) {
//            evt.Intruder.-/->( ref.changed, this )
            evt.Intruder.-/->( offset.changed, this )
         }

         protected def disposeData()( implicit tx: S#Tx ) {}

         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Elem.Update[ S ]] = {
println( "WARNING: Span.Embedded pullUpdate not yet implemented" )
None
//            val refEvt  = ref.changed
//            val refUpd  = if( evt.Intruder.isSource( refEvt, pull )) evt.Intruder.pullUpdate( refEvt, pull ) else None
//            val offEvtL = offset.changed
//            val offUpd  = if( offEvtL.isInstanceOf[ evt.NodeSelector[ _, _ ]]) {   // XXX TODO ugly
//               val offEvt = offEvtL.asInstanceOf[ Event[ S, evt.Change[ Long ], Expr[ S, Long ]]]
//               if( evt.Intruder.isSource( offEvt, pull )) evt.Intruder.pullUpdate( offEvt, pull ) else None
//            } else None
//            val offVal  = offUpd.map( _.now ).getOrElse( offset.value )
//
//            Some( Elem.EmbeddedChanged( /* this, */ refUpd.getOrElse( IIdxSeq.empty ), offVal ))
         }
      }
   }
   sealed trait Embedded[ S <: Sys[ S ]] extends Elem[ S ] {
      def ref: TimedProc[ S ]
      def key: String
      def offset: Expr[ S, Long ]

      final protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( Embedded.cookie )
//         ref.write( out )

         ref.id.write( out )
         ref.span.write( out )
         ref.value.write( out )

         out.writeString( key )
         offset.write( out )
      }
   }

//   type Modifiable[ S <: Sys[ S ]] = BiPin.Expr.Modifiable[ S, Elem[ S ]]
   type Modifiable[ S <: Sys[ S ]] = BiPin.Modifiable[ S, Elem[ S ], Elem.Update[ S ]]

   object Modifiable {
      def apply[ S <: Sys[ S ]]( implicit tx: S#Tx ) : Modifiable[ S ] = {
         implicit val time = Longs
         BiPin.Modifiable( _.changed ) // ( tx, Elem.serializer[ S ], Longs )
      }

      /**
       * Extractor to check if a `Scan` is actually a `Scan.Modifiable`
       */
      def unapply[ S <: Sys[ S ]]( g: Grapheme[ S ]) : Option[ Modifiable[ S ]] = {
//         if( v.isInstanceOf[ Modifiable[ _ ]]) Some( v.asInstanceOf[ Modifiable[ S ]]) else None
         if( g.isInstanceOf[ BiPin.Modifiable[ _ , _ , _ ]]) Some( g.asInstanceOf[ Modifiable[ S ]]) else None
      }
   }

   def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Grapheme[ S ] = {
      implicit val time = Longs
      val pin = BiPin.read[ S, Elem[ S ], Elem.Update[ S ]]( _.changed )( in, access )
      new Impl( pin )
   }

//   def Elems[ S <: Sys[ S ]] : BiType[ Elem[ S ]] = anyElems.asInstanceOf[ BiType[ Elem[ S ]]]
//
//   private val anyElems = new ElemsImpl[ I ]
//
//   private final class ElemsImpl[ S <: Sys[ S ]] extends BiTypeImpl[ Elem[ S ]] {
//      private val typeID = 1000
//
//      /* protected */ def readValue( in: DataInput ) : Elem[ S ] = ??? // SpanLike.read( in )
//      /* protected */ def writeValue( value: Elem[ S ], out: DataOutput ) { ??? } // value.write( out )}
//
//      def readTuple[ S1 <: Sys[ S1 ]]( cookie: Int, in: DataInput, access: S1#Acc, targets: evt.Targets[ S1 ])( implicit tx: S1#Tx ) : Ex[ S1 ] =
//         (cookie /*: @switch */) match {
//            case _ => sys.error( "Invalid cookie " + cookie )
//         }
//   }
}
sealed trait Grapheme[ S <: Sys[ S ]] {
   def pin: BiPin[ S, Grapheme.Elem[ S ], Grapheme.Elem.Update[ S ]]
}