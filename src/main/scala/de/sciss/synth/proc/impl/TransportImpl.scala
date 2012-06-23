package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.DataOutput
import de.sciss.lucre.event.{Event, Reader, Targets, Change, Root}
import de.sciss.synth.expr.{Longs, Booleans}

object TransportImpl {
   def apply[ S <: Sys[ S ]]( group: ProcGroup[ S ], sampleRate: Double )
                            ( implicit tx: S#Tx /*, longs: BiType[ Long ]*/) : Transport[ S, Proc[ S ]] =
      new Impl[ S ]( group, sampleRate, tx )

   private final class TimeExpr[ S <: Sys[ S ]]( protected val targets: Targets[ S ], t: Impl[ S ])
   extends Expr.Node[ S, Long ]
   with Root[ S, Change[ Long ]] {
      protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( 4 )
         t.write( out )
      }

      def value( implicit tx: S#Tx ) : Long = sys.error( "TODO" )

      protected def reader : Reader[ S, Expr[ S, Long ]] = Longs.serializer[ S ]
   }

   private final class Impl[ S <: Sys[ S ]]( group: ProcGroup[ S ],
                                             val sampleRate: Double, tx0: S#Tx )
   extends Transport[ S, Proc[ S ]] {
      override def toString = "Transport(" + sampleRate + ")"

      private val timeExpr    = new TimeExpr( Targets[ S ]( tx0 ), this )
      private val playingVar  = Booleans.newVar[ S ]( Booleans.newConst( false ))( tx0 )
      private val systemRef   = System.currentTimeMillis()

      def write( out: DataOutput ) {
         playingVar.write( out )
      }

      def dispose()( implicit tx: S#Tx ) {
         playingVar.dispose()
      }

      def seek( time: Long )( implicit tx: S#Tx ) {
         sys.error( "TODO" )
      }

      def playing( implicit tx: S#Tx ) : Expr[ S, Boolean ] = playingVar.get

      def playing_=( expr: Expr[ S, Boolean ])( implicit tx: S#Tx ) {
         val wasPlaying = playingVar.get.value
         val isPlaying  = expr.value
         playingVar.set( expr )
         if( wasPlaying != isPlaying ) {
         }
      }

      def time : Expr[ S, Long ] = timeExpr

      def changed : Event[ S, Transport.Update[ S, Proc[ S ]], Transport[ S, Proc[ S ]]] = sys.error( "TODO" )
   }
}
