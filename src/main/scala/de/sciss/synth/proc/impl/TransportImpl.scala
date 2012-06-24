package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.{DataInput, DataOutput}
import de.sciss.lucre.{event => evt}
import evt.Event
import de.sciss.synth.expr.Booleans
import de.sciss.collection.txn
import concurrent.stm.{Ref => STMRef}
import annotation.tailrec
import collection.immutable.{IndexedSeq => IIdxSeq}

object TransportImpl {
   def apply[ S <: Sys[ S ]]( group: ProcGroup[ S ], sampleRate: Double )
                            ( implicit tx: S#Tx /*, longs: BiType[ Long ]*/) : Transport[ S, Proc[ S ]] = {
      val targets    = evt.Targets[ S ]
      val playingVar = Booleans.newVar[ S ]( Booleans.newConst( false ))
      new Impl[ S ]( targets, group, sampleRate, playingVar )
   }

   implicit def serializer[ S <: Sys[ S ]] : evt.NodeSerializer[ S, Transport[ S, Proc[ S ]]] =
      new Ser[ S ]

   private final class Ser[ S <: Sys[ S ]] extends evt.NodeSerializer[ S, Transport[ S, Proc[ S ]]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Transport[ S, Proc[ S ]] = {
         val group      = ProcGroupX.read( in, access )
         val sampleRate = in.readDouble()
         val playingVar = Booleans.readVar( in, access )
         new Impl( targets, group, sampleRate, playingVar )
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

   private def flatMap[ S <: Sys[ S ], A, B ]( it: txn.Iterator[ S#Tx, IIdxSeq[ A ]])( fun: A => B )
                                             ( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, B ] = {
      val res = new FlatMap( it, fun )
      res.init()
      res
   }

   private final class FlatMap[ S <: Sys[ S ], A, B ]( it: txn.Iterator[ S#Tx, IIdxSeq[ A ]], fun: A => B )
   extends txn.Iterator[ S#Tx, B ] {
      private val coll        = STMRef( (IIdxSeq.empty[ A ], -1) )
      private var hasNextVar  = false

      def init()( implicit tx: S#Tx ) {
         val tup = findNext()
         coll.set( tup )( tx.peer )
         hasNextVar = tup._2 >= 0
      }

      @tailrec private def findNext()( implicit tx: S#Tx ) : (IIdxSeq[ A ], Int) = {
         if( !it.hasNext ) (IIdxSeq.empty, -1) else {
            val n = it.next()
            if( n.nonEmpty ) (n, 0) else findNext()
         }
      }

      def hasNext : Boolean = hasNextVar

      def next()( implicit tx: S#Tx ) : B = {
         implicit val itx = tx.peer
         val (seq, idx) = coll.get
         val res  = fun( seq( idx ))
         val idx1 = idx + 1
         if( idx1 < seq.size ) {
            coll.set( (seq, idx1) )
         } else {
            init()
         }
         res
      }
   }

   private final class Impl[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ], group: ProcGroup[ S ],
                                             val sampleRate: Double, playingVar: Expr.Var[ S, Boolean ])
   extends Transport[ S, Proc[ S ]]
   with evt.Trigger.Impl[ S, Transport.Update[ S, Proc[ S ]], Transport.Update[ S, Proc[ S ]], Transport[ S, Proc[ S ]]]
   with evt.StandaloneLike[ S, Transport.Update[ S, Proc[ S ]], Transport[ S, Proc[ S ]]]
   {
      override def toString() = "Transport(" + sampleRate + ")" + id

//      private val timeExpr    = new TimeExpr( Targets[ S ]( tx0 ), this )
//      private val playingVar  = Booleans.newVar[ S ]( Booleans.newConst( false ))( tx0 )
//      private val systemRef   = System.currentTimeMillis()

      protected def writeData( out: DataOutput ) {
         group.write( out )
         out.writeDouble( sampleRate )
         playingVar.write( out )
      }

      protected def disposeData()( implicit tx: S#Tx ) {
         playingVar.dispose()
      }

      def iterator( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, Proc[ S ]] = flatMap( group.intersect( time ).map( _._2 ))( _._2 )

      def seek( time: Long )( implicit tx: S#Tx ) {
//         sys.error( "TODO" )
      }

      def playing( implicit tx: S#Tx ) : Expr[ S, Boolean ] = playingVar.get

      def playing_=( expr: Expr[ S, Boolean ])( implicit tx: S#Tx ) {
         val wasPlaying = playingVar.get.value
         val isPlaying  = expr.value
         playingVar.set( expr )
         if( wasPlaying != isPlaying ) {
         }
      }

      def time( implicit tx: S#Tx ) : Long = 0L // Expr[ S, Long ] = timeExpr

      // ---- event stuff ----


      def connect()( implicit tx: S#Tx ) {

      }

      def disconnect()( implicit tx: S#Tx ) {

      }

      def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Transport.Update[ S, Proc[ S ]]] = {
         sys.error( "TODO" )
      }

      def changed : Event[ S, Transport.Update[ S, Proc[ S ]], Transport[ S, Proc[ S ]]] = this

      protected def reader: evt.Reader[ S, Transport[ S, Proc[ S ]]] = serializer[ S ]
   }
}
