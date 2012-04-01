package de.sciss.synth.proc

import de.sciss.synth
import synth._
import ugen._
import de.sciss.lucre.expr.Expr
import expr._
import de.sciss.lucre.{DataInput, DataOutput}
import de.sciss.confluent.Confluent
import de.sciss.lucre.stm.impl.BerkeleyDB
import java.io.File
import de.sciss.lucre.stm.{Durable, TxnSerializer, Cursor, Sys, InMemory}
import de.sciss.confluent.KSys

object PaperTest extends App {
//   def main( args: Array[ String ]) {
//      implicit val system: InMemory = InMemory()
//      run[ InMemory ]()

   {
      val dir        = File.createTempFile( "database", "db" )
      dir.delete()
      val store      = BerkeleyDB.factory( dir )
      implicit val s = Confluent( store )
      run[ Confluent ]
   }

//      implicit val s = Durable( store )
//      run[ Durable ]
//   }

//   object Access {
//
//   }
//   trait Access[ S <: Sys[ S ]] {
//      def group : S#Var[ ProcGroup[ S ]]
//      def freq  : Expr.Var[ S, Double ]
//   }

   def run[ S <: KSys[ S ]]()( implicit system: S, cursor: Cursor[ S ]) {
      implicit val whyOhWhy   = ProcGroup.serializer[ S ]
      implicit val whyOhWhy2  = Proc.serializer[ S ]
      implicit object doubleVarSerializer extends TxnSerializer[ S#Tx, S#Acc, Expr.Var[ S, Double ]] {
         def write( v: Expr.Var[ S, Double ], out: DataOutput ) { v.write( out )}
         def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Expr.Var[ S, Double ] =
            Doubles.readVar[ S ]( in, access )
      }

      def newGroup()(  implicit tx: S#Tx ) : ProcGroup[ S ] = ProcGroup.empty
      def newProc()(   implicit tx: S#Tx ) : Proc[ S ]      = Proc()

      val access = system.root { implicit tx => newGroup() }

      def newAccess[ A ]( block: => A )( implicit tx: S#Tx, ser: TxnSerializer[ S#Tx, S#Acc, A ]) : S#Entry[ A ] = {
         val v = tx.newVar( access.get.id, /* tx.newID(), */ block )
         tx.system.asEntry( v )
      }
      def exprVar( init: Double )( implicit tx: S#Tx ) : Expr.Var[ S, Double ] = Doubles.newVar[ S ]( init )

      val freqVar = cursor.step { implicit tx => newAccess( exprVar( 50.0 ))}

      val (v1, proc1) = cursor.step { implicit tx =>
         val group   = access.get
         val p       = newProc()
         p.freq      = freqVar.get
         p.graph     = {
            val f = "freq".kr(50)   // fundamental frequency
            val p = 20              // number of partials per channel
            val m = Mix.tabulate(p) { i =>
               FSinOsc.ar(f * (i+1)) *
                  (LFNoise1.kr(Seq(Rand(2, 10), Rand(2, 10))) * 0.02).max(0)
            }
            Out.ar( 0, m )
         }
         group.add( p )
         tx.inputAccess -> newAccess( p )
      }

      val v2 = cursor.step { implicit tx =>
         val p    = proc1.get
         p.freq   = freqVar.get * 1.4
         tx.inputAccess
      }

      Auralization.run[ S ]( access )

      (new Thread {
         override def run() {
            Thread.sleep( 4000L )
            cursor.step { implicit tx =>
               freqVar.get.set( 60.0  )
            }
         }
      }).start()

//      Server.run { s =>
//         val sd = SynthDef( "test", gr.expand )
//         sd.play
//         Thread.sleep( 4000 )
//         sys.exit( 0 )
//      }
   }
}
