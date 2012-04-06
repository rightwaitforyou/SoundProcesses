package de.sciss.synth.proc

import de.sciss.synth
import synth._
import expr.{Doubles, ExprImplicits}
import ugen._
import de.sciss.lucre.expr.Expr
//import expr._
import de.sciss.lucre.{DataInput, DataOutput}
import de.sciss.confluent.Confluent
import de.sciss.lucre.stm.impl.BerkeleyDB
import java.io.File
import de.sciss.lucre.stm.{TxnSerializer, Cursor}
import de.sciss.confluent.KSys

object PaperTest extends App {
   val DRY = true

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
      val imp = new ExprImplicits[ S ]
      import imp._

      implicit object doubleVarSerializer extends TxnSerializer[ S#Tx, S#Acc, Expr.Var[ S, Double ]] {
         def write( v: Expr.Var[ S, Double ], out: DataOutput ) { v.write( out )}
         def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Expr.Var[ S, Double ] =
            Doubles.readVar[ S ]( in, access )
      }

      def newGroup()(  implicit tx: S#Tx ) : ProcGroup[ S ] = ProcGroup.empty
      def newProc()(   implicit tx: S#Tx ) : Proc[ S ]      = Proc()

      def log( what: => String ) {
         println( "____PAPER____ " + what )
      }

      val access = system.root { implicit tx =>
         log( "newGroup" )
         val g = newGroup()
         if( DRY ) {
            log( "react to new group" )
            g.changed.reactTx { implicit tx => (e: ProcGroup.Update[ S ]) => println( "____OBSERVE____ " + e )}
         }
         g
      }

      def newAccess[ A ]( block: => A )( implicit tx: S#Tx, ser: TxnSerializer[ S#Tx, S#Acc, A ]) : S#Entry[ A ] = {
         val v = tx.newVar( access.get.id, /* tx.newID(), */ block )
         tx.system.asEntry( v )
      }
      def exprVar( init: Double )( implicit tx: S#Tx ) : Expr.Var[ S, Double ] = Doubles.newVar[ S ]( init )

      val freqVar = cursor.step { implicit tx =>
         log( "freq = exprVar( 50.0 )" )
         val freq = exprVar( 50.0 )
         log( "newAccess( freq )" )
         newAccess( freq )
      }

      val proc1 = cursor.step { implicit tx =>
         log( "access group" )
         val group   = access.get
         log( "p = newProc()" )
         val p       = newProc()
         log( "access freqVar" )
         val freq    = freqVar.get
         log( "p.freq = freqVar" )
         p.freq      = freq
         p.graph     = {
            val f = "freq".kr       // fundamental frequency
            val p = 20              // number of partials per channel
            val m = Mix.tabulate(p) { i =>
               FSinOsc.ar(f * (i+1)) *
                  (LFNoise1.kr(Seq(Rand(2, 10), Rand(2, 10))) * 0.02).max(0)
            }
            Out.ar( 0, m )
         }
         log( "group.add( p )" )
         group.add( p )
         log( "newAccess( p )" )
         newAccess( p )
      }

      val v1 = cursor.step { implicit tx =>
         log( "access p" )
         val p    = proc1.get
         log( "access freqVar" )
         val freq    = freqVar.get
         log( "p.freq = freqVar * 1.4" )
         p.freq   = freq * 1.4
         tx.inputAccess
      }

//      println( "v1 = " + v1 )

      def meldStep() {
         cursor.step { implicit tx =>
            log( "access group" )
            val group   = access.get
            log( "p1 = p.meld( v1 )" )
            val p1   = proc1.meld( v1 )
            log( "group.add( p1 )" )
            group.add( p1 )
         }
      }

      def freqStep() {
         cursor.step { implicit tx =>
            log( "access freqVar" )
            val freq = freqVar.get
            log( "freqVar.set( 40.0 )" )
            freq.set( 40.0 )
         }
      }

      if( DRY ) {
         meldStep()
         freqStep()

      } else  {
         Auralization.run[ S ]( access )

         (new Thread {
            override def run() {
               Thread.sleep( 4000L )
               meldStep()
               Thread.sleep( 4000L )
               freqStep()
            }
         }).start()
      }
   }
}
