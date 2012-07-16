package de.sciss.synth.proc

import de.sciss.lucre.expr.{SpanLike, BiGroup, Expr, Chronos}
import de.sciss.lucre.stm.{Cursor, TxnSerializer, Writer, Disposable, Sys}
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.lucre.event.Event
import de.sciss.collection.txn
import de.sciss.lucre.{event => evt}

object Transport {
   def apply[ S <: Sys[ S ], A ]( group: ProcGroup[ S ], sampleRate: Double = 44100, self: => S#Entry[ A ])
                                ( implicit tx: S#Tx, cursor: Cursor[ S ],
                                  selfView: A => Transport[ S, Proc[ S ]]) : Transport[ S, Proc[ S ]] =
      impl.TransportImpl( group, sampleRate, self )

   implicit def serializer[ S <: Sys[ S ], A ]( self: => S#Entry[ A ])( implicit cursor: Cursor[ S ],
                                                                        selfView: A => Transport[ S, Proc[ S ]]) : TxnSerializer[ S#Tx, S#Acc, Transport[ S, Proc[ S ]]] =
         impl.TransportImpl.serializer( self )

   sealed trait Update[ S <: Sys[ S ], Elem ] { def transport: Transport[ S, Elem ]}

   final case class Advance[ S <: Sys[ S ], Elem ]( transport: Transport[ S, Elem ], playing: Boolean,
                                                    time: Long,
                                                    added:   IIdxSeq[ (SpanLike, Elem) ],
                                                    removed: IIdxSeq[ (SpanLike, Elem) ],
                                                    params:  IIdxSeq[ (SpanLike, Elem, Map[ String, Param ])])
   extends Update[ S, Elem ] {
      override def toString =
         (if( playing ) "Advance" else "Seek") + "(" + transport + ", " + time +
            (if( added.nonEmpty )   added.mkString(   ", added = ",   ",", "" ) else "") +
            (if( removed.nonEmpty ) removed.mkString( ", removed = ", ",", "" ) else "") +
            (if( params.nonEmpty )  params.mkString(  ", params = ",  ",", "" ) else "") + ")"
   }

   final case class Play[ S <: Sys[ S ], Elem ]( transport: Transport[ S, Elem ]) extends Update[ S, Elem ]
   final case class Stop[ S <: Sys[ S ], Elem ]( transport: Transport[ S, Elem ]) extends Update[ S, Elem ]
}
trait Transport[ S <: Sys[ S ], Elem ] extends evt.Node[ S ] with Chronos[ S ] {
   def id: S#ID

   def seek( time: Long )( implicit tx: S#Tx ) : Unit
   def playing( implicit tx: S#Tx ) : Expr[ S, Boolean ]
   def playing_=( expr: Expr[ S, Boolean ])( implicit tx: S#Tx ) : Unit

   def sampleRate: Double

   def iterator( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, (SpanLike, Elem) ]

   def changed: Event[ S, Transport.Update[ S, Elem ], Transport[ S, Elem ]]

   // unfortunately this needs to go in the API because of the self-access problem
   private[proc] def eventReached( valid: Int, newLogical: Long, oldFrame: Long, newFrame: Long,
                                   hasProcEvent: Boolean, hasParEvent: Boolean )( implicit tx: S#Tx ) : Unit

//   def play()( implicit time: Chronos[ S ]) : Unit
//   def stop()( implicit time: Chronos[ S ]) : Unit
}
