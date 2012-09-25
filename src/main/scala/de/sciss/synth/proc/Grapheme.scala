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

package de.sciss.synth
package proc

import de.sciss.lucre.{bitemp, stm, expr, DataInput, event => evt}
import stm.Serializer
import expr.Expr
import bitemp.Span
import collection.immutable.{IndexedSeq => IIdxSeq}
import impl.{GraphemeImpl => Impl}
import io.AudioFileSpec
import evt.{Event, Sys}

object Grapheme {
   // If necessary for some views, we could eventually add the Elems, too,
   // like `changes: IIdxSeq[ (Elem[ S ], Value) ]`. Then the question would be
   // if Elem should have an id method? I.e. we'll have `add( elem: Elem[ S ]) : StoredElem[ S ]`
   // where `trait StoredElem[ S <: Sys[ S ]] { def elem: Elem[ S ]; def id: S#ID }`?
   final case class Update[ S <: Sys[ S ]]( grapheme: Grapheme[ S ], changes: IIdxSeq[ Value ])

   implicit def serializer[ S <: Sys[ S ]] : Serializer[ S#Tx, S#Acc, Grapheme[ S ]] =
      Impl.serializer[ S ]

   object Value {
      /**
       * A mono- or polyphonic constant value
       */
      final case class Const( span: Span.HasStart, values: Double* ) extends Value {
         def numChannels = values.size
      }

      /**
       * A mono- or polyphonic envelope segment
       *
       * @param span    the span value covered by this segment
       * @param values  a sequence of tuples, each consisting of the value at start of the segment,
       *                the target value of the segment, and the shape of the segment
       */
      final case class Segment( span: Span.HasStart, values: (Double, Double, Env.ConstShape)* ) extends Value {
         def numChannels = values.size

         def from( start: Long ) : Segment = {
            val newSpan = span.intersect( Span.from( start )).nonEmptyOption.getOrElse {
               throw new IllegalArgumentException(
                  "Segment.from - start position " + start + " lies outside of span " + span )
            }
            val newValues  = span match {
               case Span( oldStart, oldStop) =>
                  val pos = (start - oldStart).toDouble / (oldStop - oldStart)
                  values.map { case (oldStartVal, stopVal, shape) =>
                     val newStartVal = shape.levelAt( pos.toFloat, oldStartVal.toFloat, stopVal.toFloat).toDouble
                     (newStartVal, stopVal, shape)
                  }

               case _ => values // either of start or stop is infinite, therefore interpolation does not make sense
            }
            Segment( newSpan, newValues: _* )
         }
      }

      final case class Audio( span: Span.HasStart, artifact: Artifact, spec: AudioFileSpec, offset: Long, gain: Double )
      extends Value {
         def numChannels = spec.numChannels
      }
   }

   /**
    * An evaluated and flattened scan element. This is either an immutable value such as a constant or
    * envelope segment, or a real-time signal, coming either from the same process (`Source`) or being
    * fed by another embedded process (`Sink`).
    */
   sealed trait Value {
      def numChannels: Int
      def span: Span.HasStart
   }

   object Elem {
      // Note: we do not need to carry along `elem` because the outer collection
      // (`BiPin`) already does that for us.
//      sealed trait Update[ S ] // { def elem: Elem[ S ]}
//      final case class MonoChanged[ S <: Sys[ S ]]( /* elem: Mono[ S ], */ change: evt.Change[ Double ]) extends Update[ S ]
////      final case class EmbeddedChanged[ S <: Sys[ S ]]( /* elem: Embedded[ S ], */ refChange: Option[ Grapheme.Update[ S ]], offset: Long ) extends Update[ S ]
//      final case class EmbeddedChanged[ S <: Sys[ S ]]( /* elem: Embedded[ S ], */ refChanges: IIdxSeq[ BiGroup.ElementUpdate[ Proc.Update[ S ]]], offset: Long ) extends Update[ S ]

      final case class Curve[ S <: Sys[ S ]]( values: (Expr[ S, Double ], Env.ConstShape)* ) extends Elem[ S ] {
         def isConstant : Boolean = values.forall { tup => Expr.isConst( tup._1 )}
         def numChannels = values.size
      }

      final case class Audio[ S <: Sys[ S ]]( artifact: Artifact, spec: AudioFileSpec, offset: Expr[ S, Long ], gain: Expr[ S, Double ])
      extends Elem[ S ] {
         def isConstant : Boolean = Expr.isConst( offset ) && Expr.isConst( gain )
         def numChannels = spec.numChannels
      }

      // XXX TODO: if we get too ambitious:
      // trait Embed[ S <: Sys[ S ]] { def peer: Grapheme[ S ], offset: Expr[ S, Long ]}
      // trait Graph[ S <: Sys[ S ]] { def fun: GraphFunction }
      // trait Proc[ S <: Sys[ S ]] { def proc: proc.Proc[ S ]; def scanKey: String }
   }
   sealed trait Elem[ S ] { def numChannels: Int }

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
   /**
    * The idea of all traits which distinguish between read-only and modifiable sub-type is that
    * eventually the super-type acts somewhat like an expression. It might be possible to map
    * a grapheme with operators, and it might be preferable to avoid having to have the modifiable
    * operations in the mapped object (cf. `Expr` versus `Expr.Var`).
    */
   def modifiableOption : Option[ Grapheme.Modifiable[ S ]]

   def at( time: Long )( implicit tx: S#Tx ) : Option[ Grapheme.Elem[ S ]]
   def valueAt( time: Long )( implicit tx: S#Tx ) : Option[ Grapheme.Value ]
   def nearestEventAfter( time: Long )( implicit tx: S#Tx ) : Option[ Long ]

   def changed: Event[ S, Grapheme.Update[ S ], Grapheme[ S ]]
}