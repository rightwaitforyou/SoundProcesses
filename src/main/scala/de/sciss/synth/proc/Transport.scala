package de.sciss.synth
package proc

import de.sciss.lucre.{bitemp, expr, stm, event => evt, data}
import bitemp.{SpanLike, BiGroup, Chronos}
import expr.Expr
import stm.{Cursor, Serializer, Sys}
import collection.immutable.{IndexedSeq => IIdxSeq}
import evt.Event
import data.Iterator
//import imp.{TransportImpl => Impl}

object Transport {
   def apply[ S <: Sys[ S ]]( group: ProcGroup[ S ], sampleRate: Double = 44100 )
                            ( implicit tx: S#Tx, cursor: Cursor[ S ]) : ProcTransport[ S ] =
      ??? // impl.TransportImpl( group, sampleRate )

   implicit def serializer[ S <: Sys[ S ]]( implicit cursor: Cursor[ S ]): Serializer[ S#Tx, S#Acc, ProcTransport[ S ]] =
      ??? // impl.TransportImpl.serializer( cursor )

   sealed trait Update[ S <: Sys[ S ], Elem, U ] { def transport: Transport[ S, Elem, U ]}

   final case class Advance[ S <: Sys[ S ], Elem, U ]( transport: Transport[ S, Elem, U ], playing: Boolean,
                                                       time: Long,
                                                       added:   IIdxSeq[ (SpanLike, BiGroup.TimedElem[ S, Elem ])],
                                                       removed: IIdxSeq[ (SpanLike, BiGroup.TimedElem[ S, Elem ])],
                                                       changes: IIdxSeq[ (SpanLike, BiGroup.TimedElem[ S, Elem ], U) ])
   extends Update[ S, Elem, U ] {
      override def toString =
         (if( playing ) "Advance" else "Seek") + "(" + transport + ", " + time +
            (if( added.nonEmpty )   added.mkString(   ", added = ",   ",", "" ) else "") +
            (if( removed.nonEmpty ) removed.mkString( ", removed = ", ",", "" ) else "") +
            (if( changes.nonEmpty )  changes.mkString(  ", changes = ",  ",", "" ) else "") + ")"
   }

   final case class Play[ S <: Sys[ S ], Elem, U ]( transport: Transport[ S, Elem, U ]) extends Update[ S, Elem, U ]
   final case class Stop[ S <: Sys[ S ], Elem, U ]( transport: Transport[ S, Elem, U ]) extends Update[ S, Elem, U ]

   // particular update for ProcTransport
   object Proc {
      sealed trait Update[ +S ]
      final case class Changed[ S <: Sys[ S ]]( peer: proc.Proc.Update[ S ]) extends Update[ S ]
      final case class GraphemesChanged( map: Map[ String, Grapheme.Value ]) extends Update[ Nothing ]
   }
}
trait Transport[ S <: Sys[ S ], Elem, U ] extends evt.Node[ S ] with Chronos[ S ] {
   def id: S#ID

   def seek( time: Long )( implicit tx: S#Tx ) : Unit
   def playing( implicit tx: S#Tx ) : Expr[ S, Boolean ]
   def playing_=( expr: Expr[ S, Boolean ])( implicit tx: S#Tx ) : Unit

   def sampleRate: Double

   def iterator( implicit tx: S#Tx ) : Iterator[ S#Tx, (SpanLike, BiGroup.TimedElem[ S, Elem ])]

   def group: BiGroup[ S, Elem, U ]

   def changed: Event[ S, Transport.Update[ S, Elem, U ], Transport[ S, Elem, U ]]

   // unfortunately this needs to go in the API because of the self-access problem
   private[proc] def eventReached( valid: Int, newLogical: Long, oldFrame: Long, newFrame: Long,
                                   hasProcEvent: Boolean, hasParEvent: Boolean )( implicit tx: S#Tx ) : Unit

//   def play()( implicit time: Chronos[ S ]) : Unit
//   def stop()( implicit time: Chronos[ S ]) : Unit
}
