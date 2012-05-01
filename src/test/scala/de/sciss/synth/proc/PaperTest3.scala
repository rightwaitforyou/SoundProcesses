package de.sciss.synth.proc

import de.sciss.synth
import synth._
import expr.{Doubles, ExprImplicits}
import ugen._
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.{LucreSTM, DataInput, DataOutput}

import de.sciss.lucre.stm.impl.BerkeleyDB
import java.io.File
import de.sciss.confluent.{TemporalObjects, Confluent, KSys}
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.lucre.stm.{Serializer, TxnSerializer, Cursor}

object PaperTest3 extends App {
   val DRY = false

   LucreSTM.showEventLog            = true
//   TemporalObjects.showConfluentLog = true
//   TemporalObjects.showPartialLog   = true

   {
      val dir        = File.createTempFile( "database", "db" )
      dir.delete()
      val store      = BerkeleyDB.factory( dir )
      implicit val s = Confluent( store )
      run[ Confluent ]
   }

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

      object Access {
         def apply( g: ProcGroup[ S ]) : Access = new Impl( g, IIdxSeq.empty )

         implicit def group( a: Access ) : ProcGroup[ S ] = a.group

         private val varsSer = TxnSerializer.indexedSeq[ S#Tx, S#Acc, Expr.Var[ S, Double ]]

         implicit object Ser extends TxnSerializer[ S#Tx, S#Acc, Access ] {
            def write( v: Access, out: DataOutput ) {
               v.group.write( out )
               varsSer.write( v.vars, out )
            }

            def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Access = {
               val g       = ProcGroup.read[ S ]( in, access )
               val vars    = varsSer.read( in, access )
               new Impl( g, vars )
            }
         }

//         trait Handle {
//            def get( implicit tx: S#Tx ) : Expr.Var[ S, Double ]
//         }

         private final class Impl( val group: ProcGroup[ S ], val vars: IIdxSeq[ Expr.Var[ S, Double ]])
         extends Access {
            override def toString = "Access"
            def addVar( v: Expr.Var[ S, Double ]) : Access = new Impl( group, vars :+ v )
         }
      }
      trait Access {
         def group : ProcGroup[ S ]
         def vars  : IIdxSeq[ Expr.Var[ S, Double ]]
         def addVar( v: Expr.Var[ S, Double ]) : Access
      }

      implicit val whyOhWhy3 = Access.Ser
      val access = system.root { implicit tx =>
         log( "newGroup" )
         val g = newGroup()
         if( DRY ) {
            log( "react to new group" )
            g.changed.reactTx { implicit tx => (e: ProcGroup.Update[ S ]) => println( "____OBSERVE____ " + e )}
         }
         Access( g )
      }

//      def newAccess[ A ]( block: => A )( implicit tx: S#Tx, ser: TxnSerializer[ S#Tx, S#Acc, A ]) : S#Entry[ A ] = {
//         val v = tx.newVar( access.get.id, /* tx.newID(), */ block )
//         tx.system.asEntry( v )
//      }

//      def newAccess( v: Expr.Var[ S, Double ])( implicit tx: S#Tx ) : S#Entry[ A ] = {
//         val v = tx.newVar( access.get.id, /* tx.newID(), */ block )
//         tx.system.asEntry( v )
//      }
      def exprVar( init: Double )( implicit tx: S#Tx ) : Expr.Var[ S, Double ] = Doubles.newVar[ S ]( init )

      cursor.step { implicit tx =>
         log( "freq = exprVar( 50.0 )" )
         val freq = exprVar( 50.0 )
         log( "newAccess( freq )" )
         access.transform( _.addVar( freq ))
      }

      def groupGet()( implicit tx: S#Tx ) : ProcGroup[ S ] = access.get.group
      def freqVar()( implicit tx: S#Tx ) : Expr.Var[ S, Double ] = access.get.vars.head
      def procGet()( implicit tx: S#Tx ) : Proc[ S ] = groupGet().iterator.next()
      def procMeld( version: S#Acc )( implicit tx: S#Tx ) : Proc[ S ] = {
         access.meld( version ).group.iterator.next()
      }

      cursor.step { implicit tx =>
         log( "access group" )
         val group   = groupGet()
         log( "p = newProc()" )
         val p       = newProc()
         log( "access freqVar" )
         val freq    = freqVar()
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
      }

      val v1 = cursor.step { implicit tx =>
         log( "access p" )
         val p    = procGet()
         log( "access freqVar" )
         val freq    = freqVar()
         log( "p.freq = freqVar * 1.4" )
         val newFreq = freq * 1.4
         p.freq   = newFreq
         tx.inputAccess
      }

      val v2 = cursor.step( _.inputAccess )

//      println( "v1 = " + v1 )

      def meldStep( version: S#Acc = v1 ) {
         cursor.step { implicit tx =>
            log( "access group" )
            val group   = groupGet()
            log( "p1 = p.meld( v1 )" )
            val p1   = procMeld( version )
            log( "group.add( p1 )" )
            group.add( p1 )
         }
      }

      def freqStep( f: Double = 40.0 ) {
         cursor.step { implicit tx =>
            log( "access freqVar" )
            val freq = freqVar()
            log( "freqVar.set( " + f + " )" )
            freq.set( f ) // 40.0
         }
      }

      if( DRY ) {
         meldStep()
         freqStep()

      } else  {
         implicit val whyOhWhy4 = Access.group _
         Auralization.run[ S, Access ]( access )

         (new Thread {
            override def run() {
               Thread.sleep( 4000L )
               meldStep( v1 )
               Thread.sleep( 4000L )
               freqStep( 80.0 )
               Thread.sleep( 4000L )
               freqStep( 60.0 )
               Thread.sleep( 4000L )

               val v3 = cursor.step { implicit tx =>
                  val p       = procGet()
                  val freq    = freqVar()
                  val newFreq = freq * (1.4 * 1.4)
                  p.freq      = newFreq
                  tx.inputAccess
               }

               Thread.sleep( 4000L )
//               meldStep( v2 )
               meldStep( v3 )
               Thread.sleep( 4000L )

               // the following fails:
               // freq.set tests if the var is connected (`val con = targets.nonEmpty`)
               // and that fails when reading the children:
               // `No value for <26 @ 5,7,9,9>`

               // the problem seems to be the following:
               // freqVar is partially persistent, including the children var.
               // however, selectors are stored and retrieved using writeVal / readVal
               // (`Targets.readAndExpand[ S ]( in, access )`
               //  --> `tx.readVal[ VirtualNode[ S ]]( id )`)
               // --- and this is probably wrong, because it goes back to confluent ids ???
               freqStep( 50.0 )
            }
         }).start()
      }
   }
}