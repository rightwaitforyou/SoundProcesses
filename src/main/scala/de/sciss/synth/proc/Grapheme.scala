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
import de.sciss.lucre.bitemp.{Span, SpanLike, BiGroup, BiPin}
import de.sciss.synth.expr.{SpanLikes, Spans, Longs, Doubles}
import de.sciss.lucre.{Writable, DataOutput, DataInput, event => evt}
import evt.{EventLikeSerializer, EventLike}
import annotation.switch
import collection.immutable.{IndexedSeq => IIdxSeq}
import impl.{GraphemeImpl => Impl}
import de.sciss.synth.io.AudioFileSpec

object Grapheme {
   type Update[ S <: Sys[ S ]] = BiPin.Update[ S, Elem[ S ], Elem.Update[ S ]]

   implicit def serializer[ S <: Sys[ S ]] : Serializer[ S#Tx, S#Acc, Grapheme[ S ]] =
      Impl.serializer[ S ]

   object Value {
      /**
       * A mono- or polyphonic constant value
       */
      final case class Const( span: SpanLike, values: Double* )
      extends Value[ Nothing ] {
         def numChannels = values.size
      }

      /**
       * A mono- or polyphonic envelope segment
       *
       * @param span    the span value covered by this segment
       * @param values  a sequence of tuples, each consisting of the value at start of the segment,
       *                the target value of the segment, and the shape of the segment
       */
      final case class Segment( span: SpanLike, values: (Double, Double, Env.ConstShape)* )
      extends Value[ Nothing ] {
         def numChannels = values.size

         def from( start: Long ) : Segment = {
            val newSpan = span.intersect( Span.from( start ))
            val newValues: IIdxSeq[ (Double, Double, Env.ConstShape) ] = ???
            Segment( newSpan, newValues: _* )
         }
      }
   }

   /**
    * An evaluated and flattened scan element. This is either an immutable value such as a constant or
    * envelope segment, or a real-time signal, coming either from the same process (`Source`) or being
    * fed by another embedded process (`Sink`).
    */
   sealed trait Value[ +S ] {
//      def stop: Long
      def numChannels: Int
      def span: SpanLike
   }

   object Elem {
      // Note: we do not need to carry along `elem` because the outer collection
      // (`BiPin`) already does that for us.
      sealed trait Update[ S <: Sys[ S ]] // { def elem: Elem[ S ]}
//      final case class MonoChanged[ S <: Sys[ S ]]( /* elem: Mono[ S ], */ change: evt.Change[ Double ]) extends Update[ S ]
////      final case class EmbeddedChanged[ S <: Sys[ S ]]( /* elem: Embedded[ S ], */ refChange: Option[ Grapheme.Update[ S ]], offset: Long ) extends Update[ S ]
//      final case class EmbeddedChanged[ S <: Sys[ S ]]( /* elem: Embedded[ S ], */ refChanges: IIdxSeq[ BiGroup.ElementUpdate[ Proc.Update[ S ]]], offset: Long ) extends Update[ S ]

      implicit def serializer[ S <: Sys[ S ]] : EventLikeSerializer[ S, Elem[ S ]] = Impl.elemSerializer[ S ]

      object Curve {
         def apply[ S <: Sys[ S ]]( values: (Expr[ S, Double ], Env.ConstShape)* )( implicit tx: S#Tx ) : Curve[ S ] =
            Impl.curveElem( values: _* )

         def unapplySeq[ S <: Sys[ S ]]( elem: Elem[ S ]) : Option[ Seq[ (Expr[ S, Double ], Env.ConstShape) ]] = {
            if( elem.isInstanceOf[ Curve[ _ ]]) {
               val c = elem.asInstanceOf[ Curve[ S ]]
               Some( c.values )
            } else None
         }

//         private[Grapheme] final case class Const[ S <: Sys[ S ]]( targetLevel: Expr[ S, Double ], shape: Env.ConstShape )
//         extends Mono[ S ] with evt.Constant[ S ] {
//            override def toString = "Mono(" + targetLevel + ", " + shape + ")"
//
//            def changed: EventLike[ S, Elem.Update[ S ], Elem[ S ]] = evt.Dummy.apply
//         }
//
//         private[Grapheme] final class Mut[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ],
//                                                  val targetLevel: Expr[ S, Double ], val shape: Env.ConstShape )
//         extends Mono[ S ] with evt.StandaloneLike[ S, Elem.Update[ S ], Elem[ S ]] {
//            override def toString = "Mono(" + targetLevel + ", " + shape + ")"
//
//            def changed: EventLike[ S, Elem.Update[ S ], Elem[ S ]] = this
//
//            def reader: evt.Reader[ S, Elem[ S ]] = Elem.serializer[ S ]
//
//            def connect()( implicit tx: S#Tx ) {
//               evt.Intruder.--->( targetLevel.changed, this )
//            }
//
//            def disconnect()( implicit tx: S#Tx ) {
//               evt.Intruder.-/->( targetLevel.changed, this )
//            }
//
//            protected def disposeData()( implicit tx: S#Tx ) {}
//
//            def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Elem.Update[ S ]] = {
//               // XXX TODO ugly. Should have object Event { def unapply( ... )}
//               evt.Intruder.pullUpdate(
//                  targetLevel.changed.asInstanceOf[ evt.NodeSelector[ S, evt.Change[ Double ]]], pull ).map( u =>
//                     Elem.MonoChanged( /* this, */ u )
//                  )
//            }
//         }
      }
      trait Curve[ S <: Sys[ S ]] extends Elem[ S ] {
         def values: Seq[ (Expr[ S, Double ], Env.ConstShape) ]

//         final protected def writeData( out: DataOutput ) {
//            out.writeUnsignedByte( Mono.cookie )
//            targetLevel.write( out )
//            out.writeInt( shape.id )
//            shape match {
//               case cs: curveShape => out.writeFloat( cs.curvature )
//               case _ => // only curveShape has an extra curvature argument
//            }
//         }
      }

      object Audio {
         def apply[ S <: Sys[ S ]]( artifact: Artifact, spec: AudioFileSpec, offset: Expr[ S, Long ], gain: Expr[ S, Double ])
                                  ( implicit tx: S#Tx ) : Audio[ S ] =
            Impl.audioElem( artifact, spec, offset, gain )

         def unapply[ S <: Sys[ S ]]( elem: Elem[ S ]) : Option[ (Artifact, AudioFileSpec, Expr[ S, Long ], Expr[ S, Double ]) ] = {
            if( elem.isInstanceOf[ Audio[ _ ]]) {
               val a = elem.asInstanceOf[ Audio[ S ]]
               Some( (a.artifact, a.spec, a.offset, a.gain) )
            } else None
         }
      }
      trait Audio[ S <: Sys[ S ]] extends Elem[ S ] {
         def artifact: Artifact
         def spec: AudioFileSpec
         def offset: Expr[ S, Long ]
         def gain: Expr[ S, Double ]
      }

      // XXX TODO: if we get too ambitious:
      // trait Embed[ S <: Sys[ S ]] { def peer: Grapheme[ S ], offset: Expr[ S, Long ]}
      // trait Graph[ S <: Sys[ S ]] { def fun: GraphFunction }
      // trait Proc[ S <: Sys[ S ]] { def proc: proc.Proc[ S ]; def scanKey: String }
   }
   sealed trait Elem[ S <: Sys[ S ]] extends Writable {
      def changed: EventLike[ S, Elem.Update[ S ], Elem[ S ]]
   }

//   object Embedded {
//      private[Grapheme] final val cookie = 2
//
////      def apply[ S <: Sys[ S ]]( ref: Scan[ S ], offset: Expr[ S, Long ])( implicit tx: S#Tx ) : Embedded[ S ]
//
//      def apply[ S <: Sys[ S ]]( ref: TimedProc[ S ], key: String, offset: Expr[ S, Long ])( implicit tx: S#Tx ) : Embedded[ S ] = {
//         val tgt = evt.Targets[ S ] // XXX TODO partial? should reflect ref.targets I guess?
//         new Impl( tgt, ref, key, offset )
//      }
//
//      def unapply[ S <: Sys[ S ]]( elem: Elem[ S ]) : Option[ (TimedProc[ S ], String, Expr[ S, Long ]) ] = {
//         if( elem.isInstanceOf[ Embedded[ _ ]]) {
//            val embedded = elem.asInstanceOf[ Embedded[ S ]]
//            Some( (embedded.ref, embedded.key, embedded.offset) )
//         } else None
//      }
//
//      private[Grapheme] final class Impl[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ],
//                                                       val ref: TimedProc[ S ], val key: String, val offset: Expr[ S, Long ])
//      extends Embedded[ S ] with evt.StandaloneLike[ S, Elem.Update[ S ], Elem[ S ]] {
//         override def toString = "Embedded(" + ref + ", " + offset + ")"
//
//         def changed: EventLike[ S, Elem.Update[ S ], Elem[ S ]] = this
//
//         def reader: evt.Reader[ S, Elem[ S ]] = Elem.serializer[ S ]
//
//         def connect()( implicit tx: S#Tx ) {
////            evt.Intruder.--->( ref.changed, this )
//            evt.Intruder.--->( offset.changed, this )
//         }
//
//         def disconnect()( implicit tx: S#Tx ) {
////            evt.Intruder.-/->( ref.changed, this )
//            evt.Intruder.-/->( offset.changed, this )
//         }
//
//         protected def disposeData()( implicit tx: S#Tx ) {}
//
//         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Elem.Update[ S ]] = {
//println( "WARNING: Span.Embedded pullUpdate not yet implemented" )
//None
////            val refEvt  = ref.changed
////            val refUpd  = if( evt.Intruder.isSource( refEvt, pull )) evt.Intruder.pullUpdate( refEvt, pull ) else None
////            val offEvtL = offset.changed
////            val offUpd  = if( offEvtL.isInstanceOf[ evt.NodeSelector[ _, _ ]]) {   // XXX TODO ugly
////               val offEvt = offEvtL.asInstanceOf[ Event[ S, evt.Change[ Long ], Expr[ S, Long ]]]
////               if( evt.Intruder.isSource( offEvt, pull )) evt.Intruder.pullUpdate( offEvt, pull ) else None
////            } else None
////            val offVal  = offUpd.map( _.now ).getOrElse( offset.value )
////
////            Some( Elem.EmbeddedChanged( /* this, */ refUpd.getOrElse( IIdxSeq.empty ), offVal ))
//         }
//      }
//   }
//   sealed trait Embedded[ S <: Sys[ S ]] extends Elem[ S ] {
//      def ref: TimedProc[ S ]
//      def key: String
//      def offset: Expr[ S, Long ]
//
//      final protected def writeData( out: DataOutput ) {
//         out.writeUnsignedByte( Embedded.cookie )
////         ref.write( out )
//
//         ref.id.write( out )
//         ref.span.write( out )
//         ref.value.write( out )
//
//         out.writeString( key )
//         offset.write( out )
//      }
//   }

   trait Modifiable[ S <: Sys[ S ]] extends Grapheme[ S ] {
      def add(    time: Expr[ S, Long ], elem: Elem[ S ])( implicit tx: S#Tx ) : Unit
      def remove( time: Expr[ S, Long ], elem: Elem[ S ])( implicit tx: S#Tx ) : Boolean
      def clear()( implicit tx: S#Tx ) : Unit
   }

   object Modifiable {
      def apply[ S <: Sys[ S ]]( implicit tx: S#Tx ) : Modifiable[ S ] = Impl.modifiable[ S ]

      /**
       * Extractor to check if a `Grapheme` is actually a `Grapheme.Modifiable`
       */
      def unapply[ S <: Sys[ S ]]( g: Grapheme[ S ]) : Option[ Modifiable[ S ]] = {
         if( g.isInstanceOf[ Modifiable[ _ ]]) Some( g.asInstanceOf[ Modifiable[ S ]]) else None
      }
   }

   def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Grapheme[ S ] = Impl.read( in, access )
}
trait Grapheme[ S <: Sys[ S ]] extends evt.Node[ S ] {
   def modifiableOption : Option[ Grapheme.Modifiable[ S ]]

   def at( time: Long )( implicit tx: S#Tx ) : Option[ Grapheme.Elem[ S ]]
   def valueAt( time: Long )( implicit tx: S#Tx ) : Option[ Grapheme.Value[ S ]]
}