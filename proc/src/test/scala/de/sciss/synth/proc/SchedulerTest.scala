package de.sciss.synth.proc

import java.util.concurrent.{TimeUnit, Executors}
import concurrent.stm.{TxnLocal, Txn => ScalaTxn, TxnExecutor, InTxn, Ref => STMRef}

object SchedulerTest extends App {
   val pool    = Executors.newScheduledThreadPool( 1 )
   val txnTime = TxnLocal( System.currentTimeMillis() )
   val valid   = STMRef( 0 )

   def t[ A ]( fun: InTxn => A ) = TxnExecutor.defaultAtomic( fun )

   def schedule( delay: Long )( code: InTxn => Unit )( implicit tx: InTxn ) {
      val v          = valid()
      val logical    = txnTime()
      val jitter     = System.currentTimeMillis() - logical
      val effective  = math.max( 0L, delay - jitter )
      ScalaTxn.afterCommit { _ =>
         pool.schedule( new Runnable {
            def run() { t { implicit tx =>
               if( v == valid() ) {
                  txnTime() = logical + delay
                  code( tx )
               }
            }}
         }, effective, TimeUnit.MILLISECONDS )
      }
   }

   def stop()( implicit tx: InTxn ) {
      valid += 1
   }

   def io( code: => Unit )( implicit tx: InTxn ) {
      ScalaTxn.afterCommit( _ => code )
   }

   println( "Run." )
   t { implicit tx =>
      io( println( "0.0\"" ))
      schedule( 1000 ) { implicit tx =>
         io( println( "1.0\"" ))
         schedule( 500 ) { implicit tx =>
            io( println( "1.5\"" ))
            schedule( 1500 ) { implicit tx =>
               io {
                  println( "3.0\" -- woop. should have been stopped" )
//                  pool.shutdown()
               }
            }
         }
      }
   }
   t { implicit tx =>
      schedule( 2000 ) { implicit tx =>
         io( println( "(independant)" ))
         stop()
         schedule( 1000 ) { _ => pool.shutdown() }
      }
   }
}
