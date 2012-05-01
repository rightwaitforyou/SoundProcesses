package de.sciss.synth.proc

import de.sciss.synth
import synth._
import expr.{Doubles, ExprImplicits}
import ugen._
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.{LucreSTM, DataInput, DataOutput}

import de.sciss.lucre.stm.impl.BerkeleyDB
import java.io.File
import de.sciss.confluent.{TemporalObjects, Confluent}
import de.sciss.lucre.stm.{Sys, TxnSerializer, Cursor}

object PaperTest2 extends App {
   val DRY = true

   LucreSTM.showEventLog            = true
   TemporalObjects.showConfluentLog = true
   TemporalObjects.showPartialLog   = true

//   def main( args: Array[ String ]) {
//      implicit val system: InMemory = InMemory()
//      run[ InMemory ]()

   {
      val dir        = File.createTempFile( "database", "db" )
      dir.delete()
      val store      = BerkeleyDB.factory( dir )
      implicit val s = Confluent( store )
      new Thread {
         override def run() {
            new Example2
         }
      } start()
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

   class Helper[ S <: Sys[ S ]]( imp: ExprImplicits[ S ]) {
      import imp._

      implicit val whyOhWhy   = ProcGroup.serializer[ S ]
      implicit val whyOhWhy2  = Proc.serializer[ S ]

      implicit object doubleVarSerializer extends TxnSerializer[ S#Tx, S#Acc, Expr.Var[ S, Double ]] {
         def write( v: Expr.Var[ S, Double ], out: DataOutput ) { v.write( out )}
         def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Expr.Var[ S, Double ] =
            Doubles.readVar[ S ]( in, access )
      }

      def newGroup()(  implicit tx: S#Tx ) : ProcGroup[ S ] = ProcGroup.empty
      def newProc()(   implicit tx: S#Tx ) : Proc[ S ]      = Proc()

      def newAccess[ A ]( block: => A )( implicit tx: S#Tx, ser: TxnSerializer[ S#Tx, S#Acc, A ]) : S#Entry[ A ] = {
         val v = tx.newVar( /* access.get.id, */ tx.newID(), block )
         tx.system.asEntry( v )
      }
      def exprVar( init: Double )( implicit tx: S#Tx ) : Expr.Var[ S, Double ] = Doubles.newVar[ S ]( init )

      implicit def accessGet[ A ]( access: S#Entry[ A ])( implicit tx: S#Tx ) : A = access.get

      def sleep( secs: Double ) { Thread.sleep( (secs * 1000 + 0.5).toLong )}

      final class AccessOps[ A ]( obj: A )( implicit tx: S#Tx, ser: TxnSerializer[ S#Tx, S#Acc, A ]) {
         def asAccess: S#Entry[ A ] = {
            val v = tx.newVar( /* access.get.id, */ tx.newID(), obj )
            tx.system.asEntry( v )
         }
      }

      implicit def accessOps[ A ]( obj: A )( implicit tx: S#Tx, ser: TxnSerializer[ S#Tx, S#Acc, A ]) : AccessOps[ A ] =
         new AccessOps[ A ]( obj )
   }
}

class Example2(implicit s: Confluent, c: Cursor[Confluent]) {
  type S = Confluent

  val imp = new ExprImplicits[S]
  val helper = new PaperTest2.Helper[S](imp)
  import helper._
  import imp._

  val group = s.root(newGroup()(_))
  Auralization.run[S, ProcGroup[S]](group)

  val freq = c.step { implicit tx =>
    exprVar(50.0).asAccess
  }

  val w = c.step { implicit tx =>
    val p   = newProc()
    p.freq  = freq
    p.graph = {
      val m = Mix.tabulate(20) { i =>
        FSinOsc.ar("freq".kr * (i + 1)) *
          (LFNoise1.kr(Seq(Rand(2, 10), Rand(2, 10))) * 0.02).max(0)
      }
      Out.ar(0, m)
    }
    group add p
    p.asAccess
  }
  val v1 = c.step(_.inputAccess)

  sleep(4.0)
  c.step { implicit tx =>
    w.freq = freq * 1.4
  }

  sleep(4.0)
  c.step { implicit tx =>
    group.add(w meld v1)
  }

  sleep(4.0)
  c.step { implicit tx =>
    freq.get set 40.0
  }
}