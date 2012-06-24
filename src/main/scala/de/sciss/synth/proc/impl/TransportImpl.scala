package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.expr.{SpanLike, Span, Expr}
import de.sciss.lucre.{DataInput, DataOutput}
import de.sciss.lucre.{event => evt}
import evt.Event
import de.sciss.synth.expr.Booleans
import de.sciss.collection.txn
import collection.immutable.{IndexedSeq => IIdxSeq}
import actors.Actor

object TransportImpl {
   def apply[ S <: Sys[ S ]]( group: ProcGroup[ S ], sampleRate: Double )
                            ( implicit tx: S#Tx /*, longs: BiType[ Long ]*/) : Transport[ S, Proc[ S ]] = {
      val targets    = evt.Targets[ S ]
      val playingVar = Booleans.newVar[ S ]( Booleans.newConst( false ))
      val lastTime   = tx.newLongVar( targets.id, 0L )
      new Impl[ S ]( targets, group, sampleRate, playingVar, lastTime )
   }

   implicit def serializer[ S <: Sys[ S ]] : evt.NodeSerializer[ S, Transport[ S, Proc[ S ]]] =
      new Ser[ S ]

   private final class Ser[ S <: Sys[ S ]] extends evt.NodeSerializer[ S, Transport[ S, Proc[ S ]]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Transport[ S, Proc[ S ]] = {
         val group      = ProcGroupX.read( in, access )
         val sampleRate = in.readDouble()
         val playingVar = Booleans.readVar( in, access )
         val lastTime   = tx.readLongVar( targets.id, in )
         new Impl( targets, group, sampleRate, playingVar, lastTime )
      }
   }

//   private final class TimeExpr[ S <: Sys[ S ]]( protected val targets: Targets[ S ], t: Impl[ S ])
//   extends Expr.Node[ S, Long ]
//   with Root[ S, Change[ Long ]] {
//      protected def writeData( out: DataOutput ) {
//         out.writeUnsignedByte( 4 )
//         t.write( out )
//      }
//
//      def value( implicit tx: S#Tx ) : Long = sys.error( "TODO" )
//
//      protected def reader : Reader[ S, Expr[ S, Long ]] = Longs.serializer[ S ]
//   }

   private lazy val Runner = new Actor {
      override def toString = "Transport.Runner"

      def act() {

      }
   }

   private def flatSpans[ S <: Sys[ S ]]( in: (SpanLike, IIdxSeq[ (Expr[ S, SpanLike ], Proc[ S ])])) : IIdxSeq[ (SpanLike, Proc[ S ])] = {
      val span = in._1
      in._2.map { case (_, proc) => (span, proc) }
   }

   private final class Impl[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ], group: ProcGroup[ S ],
                                             val sampleRate: Double, playingVar: Expr.Var[ S, Boolean ], lastTime: S#Var[ Long ])
   extends Transport[ S, Proc[ S ]]
   with evt.Trigger.Impl[ S, Transport.Update[ S, Proc[ S ]], Transport.Update[ S, Proc[ S ]], Transport[ S, Proc[ S ]]]
   with evt.StandaloneLike[ S, Transport.Update[ S, Proc[ S ]], Transport[ S, Proc[ S ]]]
   with evt.Root[ S, Transport.Update[ S, Proc[ S ]]]
   {
      override def toString() = "Transport(" + sampleRate + ")" + id

//      private val timeExpr    = new TimeExpr( Targets[ S ]( tx0 ), this )
//      private val systemRef   = System.currentTimeMillis()

      protected def writeData( out: DataOutput ) {
         group.write( out )
         out.writeDouble( sampleRate )
         playingVar.write( out )
         lastTime.write( out )
      }

      protected def disposeData()( implicit tx: S#Tx ) {
         playingVar.dispose()
      }

      def iterator( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, (SpanLike, Proc[ S ])] =
         group.intersect( time ).flatMap( flatSpans )

      def seek( time: Long )( implicit tx: S#Tx ) {
         val old = lastTime.get
         if( time != old ) {
            lastTime.set( time )
            val (remStart, remStop, addStart, addStop) = if( time > old ) {
               // ... those which end in the interval (LRP, t] && begin <= LRP must be removed ...
               // ... those which begin in the interval (LRP, t] && end > t must be added ...
               val skipInt = Span( old + 1, time + 1 )
//               (Span.All, skipInt, skipInt, Span.from( time + 1 ))
               (Span.until( old + 1 ), skipInt, skipInt, Span.from( time + 1 ))
            } else {
               // ... those which begin in the interval (t, LRP] && end > LRP must be removed ...
               // ... those which end in the interval (t, LRP] && begin <=t must be added ...
               val skipInt = Span( time + 1, old + 1 )
//               (skipInt, Span.All, Span.until( time + 1 ), skipInt)
               (skipInt, Span.from( old + 1 ), Span.until( time + 1 ), skipInt)
            }
            val removed = group.rangeSearch( remStart, remStop ).flatMap( flatSpans )
            val added   = group.rangeSearch( addStart, addStop ).flatMap( flatSpans )
            if( removed.nonEmpty || added.nonEmpty ) {
               fire( Transport.Seek( this, time, added.toIndexedSeq, removed.toIndexedSeq ))
            }
         }
      }

      def playing( implicit tx: S#Tx ) : Expr[ S, Boolean ] = playingVar.get

      def playing_=( expr: Expr[ S, Boolean ])( implicit tx: S#Tx ) {
         val wasPlaying = playingVar.get.value
         val isPlaying  = expr.value
         playingVar.set( expr )
         if( wasPlaying != isPlaying ) {
            if( isPlaying ) play() else stop()
         }
      }

      private def play()( implicit tx: S#Tx ) {
         fire( Transport.Play( this ))
//         group.nearestEventAfter()
      }

      private def stop()( implicit tx: S#Tx ) {
         fire( Transport.Stop( this ))
      }

      def time( implicit tx: S#Tx ) : Long = lastTime.get   // XXX // Expr[ S, Long ] = timeExpr

      // ---- event stuff ----


//      def connect()( implicit tx: S#Tx ) {
//
//      }
//
//      def disconnect()( implicit tx: S#Tx ) {
//
//      }
//
//      def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Transport.Update[ S, Proc[ S ]]] = {
//         sys.error( "TODO" )
//      }

      def changed : Event[ S, Transport.Update[ S, Proc[ S ]], Transport[ S, Proc[ S ]]] = this

      protected def reader: evt.Reader[ S, Transport[ S, Proc[ S ]]] = serializer[ S ]
   }
}
