package de.sciss.synth.proc

import de.sciss.lucre.stm.{InMemory, Cursor, Mutable, Sys, Writer, TxnSerializer}
import de.sciss.lucre.{DataInput, DataOutput}
import java.util.concurrent.{TimeUnit, Executors, ScheduledExecutorService}
import concurrent.stm.Txn

object SelfAccessTest extends App {
   inMem()

   def inMem() {
      implicit val sys = InMemory()
      new SelfAccessTest( sys )
   }
}
class SelfAccessTest[ S <: Sys[ S ]]( system: S )( implicit cursor: Cursor[ S ]) {
   lazy val pool : ScheduledExecutorService = Executors.newScheduledThreadPool( 1 ) // > 0 to prevent immediate VM shutdown

   object Counter {
      def apply()( implicit tx: S#Tx, cursor: Cursor[ S ]) : Counter = {
         new Impl {
            val id   = tx.newID()
            val cnt  = tx.newIntVar( id, 0 )
            val play = tx.newBooleanVar( id, init = false )
            val csr  = cursor
            val self = tx.newVar[ Counter ]( id, this )
         }
      }

      implicit def serializer( implicit cursor: Cursor[ S ]) : TxnSerializer[ S#Tx, S#Acc, Counter ] = new Ser( cursor )

      private final class Ser( cursor: Cursor[ S ]) extends TxnSerializer[ S#Tx, S#Acc, Counter ] {
         ser =>

         def write( c: Counter, out: DataOutput ) { c.write( out )}
         def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Counter = {
            new Impl {
               val id      = tx.readID( in, access )
               val cnt     = tx.readIntVar( id, in )
               val play    = tx.readBooleanVar( id, in )
               val csr     = cursor
               val self    = tx.readVar[ Counter ]( id, in )( ser )
            }
         }
      }

      private abstract class Impl
      extends Counter with Mutable[ S ] with Runnable {
         me =>

         protected def csr: Cursor[ S ]
         protected def cnt: S#Var[ Int ]
         protected def play: S#Var[ Boolean ]
         protected def self: S#Var[ Counter ]

         override def toString = "Counter" + id

         protected def writeData( out: DataOutput ) {
            cnt.write( out )
            play.write( out )
         }

         protected def disposeData()( implicit tx: S#Tx ) {
            cnt.dispose()
            play.dispose()
         }

         def run() {
            csr.step { implicit tx =>
               self.get.step()
            }
         }

         def step()( implicit tx: S#Tx ) {
            if( play.get ) {
               cnt.transform( _ + 1 )
               implicit val itx = tx.peer
               Txn.beforeCommit { implicit itx =>
                  val v = value()
                  Txn.afterCommit { _ =>
                     println( "Count of " + me + " = " + v )
                  }
               }
               spawn()
            }
         }

         def start()( implicit tx: S#Tx ) {
            val wasPlaying = play.get
            if( !wasPlaying ) {
               play.set( true )
               spawn()
            }
         }

         def stop()( implicit tx: S#Tx ) {
            val wasPlaying = play.get
            if( wasPlaying ) {
               play.set( false )
            }
         }

         private def spawn()( implicit tx: S#Tx ) {
            implicit val itx = tx.peer
            Txn.afterCommit { _ =>
               pool.schedule( this, 1, TimeUnit.SECONDS )
            }
         }

         def value()( implicit tx: S#Tx ) : Int = cnt.get
      }
   }
   sealed trait Counter extends Writer {
      def start()( implicit tx: S#Tx ) : Unit
      def stop()( implicit tx: S#Tx ) : Unit
      def value()( implicit tx: S#Tx ) : Int

      // quasi-private
      def step()( implicit tx: S#Tx ) : Unit
   }

   println( "Start" )

   val access = system.root { implicit tx => Counter.apply() }
   cursor.step { implicit tx =>
      access.get.start()
   }

   pool.schedule( new Runnable {
      def run() {
         cursor.step { implicit tx =>
            implicit val itx = tx.peer
            Txn.afterCommit { _ =>
               println( "Stop" )
               pool.shutdown()
               sys.exit( 0 )
            }
            access.get.stop()
         }
      }
   }, 10, TimeUnit.SECONDS )
}