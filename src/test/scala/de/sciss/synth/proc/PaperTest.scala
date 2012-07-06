//package de.sciss.synth.proc
//
//import de.sciss.synth
//import synth._
//import expr.{Doubles, ExprImplicits}
//import ugen._
//import de.sciss.lucre.{LucreSTM, DataInput, DataOutput}
//
//import de.sciss.lucre.stm.impl.BerkeleyDB
//import java.io.File
//import de.sciss.lucre.stm.{TxnSerializer, Cursor}
//import de.sciss.confluent.{TemporalObjects, Confluent, KSys}
//import de.sciss.lucre.expr.{Span, Chronos, Expr}
//
//object PaperTest extends App {
//   val DRY = false
//
////   LucreSTM.showEventLog            = true
////   TemporalObjects.showConfluentLog = true
////   TemporalObjects.showPartialLog   = true
//
////   def main( args: Array[ String ]) {
////      implicit val system: InMemory = InMemory()
////      run[ InMemory ]()
//
//   {
//      val dir        = File.createTempFile( "database", "db" )
//      dir.delete()
//      val store      = BerkeleyDB.factory( dir )
//      implicit val s = Confluent( store )
//      run[ Confluent ]
//   }
//
////      implicit val s = Durable( store )
////      run[ Durable ]
////   }
//
////   object Access {
////
////   }
////   trait Access[ S <: Sys[ S ]] {
////      def group : S#Var[ ProcGroup[ S ]]
////      def freq  : Expr.Var[ S, Double ]
////   }
//
//   def run[ S <: KSys[ S ]]()( implicit system: S, cursor: Cursor[ S ]) {
//      implicit val whyOhWhy   = ProcGroupX.varSerializer[ S ]
//      implicit val whyOhWhy2  = Proc.serializer[ S ]
//      val imp = new ExprImplicits[ S ]
//      import imp._
//
//      implicit object doubleVarSerializer extends TxnSerializer[ S#Tx, S#Acc, Expr.Var[ S, Double ]] {
//         def write( v: Expr.Var[ S, Double ], out: DataOutput ) { v.write( out )}
//         def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Expr.Var[ S, Double ] =
//            Doubles.readVar[ S ]( in, access )
//      }
//
//      implicit val ts = Chronos[ S ]( 0L )
//
//      def newGroup()(  implicit tx: S#Tx ) : ProcGroupX.Var[ S ]   = ProcGroupX.newVar
//      def newProc()(   implicit tx: S#Tx ) : Proc[ S ]            = Proc()
//
//      def log( what: => String ) {
//         println( "______ACT______ " + what )
//      }
//
//      var logStepCnt = 0
//      def logStep() {
//         val versionID = if( logStepCnt == 0 ) "v0 (root)" else "v" + logStepCnt
//         println( "\n____VERSION____ " + versionID )
//         logStepCnt += 1
//      }
//
//
//      logStep()
//      val access = system.root { implicit tx =>
//         log( "newGroup" )
//         val g = newGroup()
//         if( DRY ) {
//            log( "react to new group" )
//            g.changed.reactTx { implicit tx => (e: ProcGroupX.Update[ S ]) => println( "____OBSERVE____ " + e )}
//         }
//         g
//      }
//
//      def newAccess[ A ]( block: => A )( implicit tx: S#Tx, ser: TxnSerializer[ S#Tx, S#Acc, A ]) : S#Entry[ A ] = {
//         val v = tx.newVar( access.get.id, /* tx.newID(), */ block )
//         tx.system.asEntry( v )
//      }
//      def exprVar( init: Double )( implicit tx: S#Tx ) : Expr.Var[ S, Double ] = Doubles.newVar[ S ]( init )
//
//      logStep()
//      val freqVar = cursor.step { implicit tx =>
//         log( "freq = exprVar( 50.0 )" )
//         val freq = exprVar( 50.0 )
//         log( "newAccess( freq )" )
//         newAccess( freq )
//      }
//
//      logStep()
//      val proc1 = cursor.step { implicit tx =>
////         log( "access group" )
//         val group   = access.get
//         log( "p = newProc()" )
//         val p       = newProc()
////         log( "access freqVar" )
//         val freq    = freqVar.get
//         log( "p.freq = freqVar" )
//         p.freq      = freq
//         p.graph     = {
//            val f = "freq".kr       // fundamental frequency
//            val p = 20              // number of partials per channel
//            val m = Mix.tabulate(p) { i =>
//               FSinOsc.ar(f * (i+1)) *
//                  (LFNoise1.kr(Seq(Rand(2, 10), Rand(2, 10))) * 0.02).max(0)
//            }
//            Out.ar( 0, m )
//         }
//         log( "group.add( p )" )
//         group.add( Span.All, p )
//         log( "newAccess( p )" )
//         newAccess( p )
//      }
//
//      logStep()
//      val v1 = cursor.step { implicit tx =>
////         log( "access p" )
//         val p    = proc1.get
////         log( "access freqVar" )
//         val freq    = freqVar.get
//         log( "p.freq = freqVar * 1.4" )
//         val newFreq = freq * 1.4
//         p.freq   = newFreq
//         tx.inputAccess
//      }
//
////      val v2 = cursor.step( _.inputAccess )
//
////      println( "v1 = " + v1 )
//
//      def meldStep( version: S#Acc = v1 ) {
//         logStep()
//         cursor.step { implicit tx =>
////            log( "access group" )
//            val group   = access.get
//            log( "p' = p.meld( " + version + " )" )
//            val p1   = proc1.meld( version )
//println( "......yields " + p1 )
//            log( "group.add( p' )" )
//            group.add( Span.All, p1 )
//         }
//      }
//
//      def freqStep( f: Double = 40.0 ) {
//         logStep()
//         cursor.step { implicit tx =>
////            log( "access freqVar" )
//            val freq = freqVar.get
//            log( "freqVar.set( " + f + " )" )
//            freq.set( f ) // 40.0
//         }
//      }
//
//      if( DRY ) {
//         meldStep()
//         freqStep()
//
//      } else  {
//         logStep()
//         AuralPresentation.run[ S, ProcGroupX.Var[ S ]]( access )
//
//         (new Thread {
//            override def run() {
//               Thread.sleep( 6000L )
//               meldStep( v1 )
//               Thread.sleep( 4000L )
//               freqStep( 80.0 )
//               Thread.sleep( 4000L )
//               freqStep( 60.0 )
//               Thread.sleep( 4000L )
//
//               logStep()
//               val v3 = cursor.step { implicit tx =>
//                  val p       = proc1.get
//                  val freq    = freqVar.get
//                  val newFreq = freq * (1.4 * 1.4)
//                  log( "p.freq = freqVar * (1.4 * 1.4)" )
//                  p.freq      = newFreq
//                  tx.inputAccess
//               }
//
//               Thread.sleep( 4000L )
////               meldStep( v2 )
//               meldStep( v3 )
//               Thread.sleep( 4000L )
//LucreSTM.showEventLog = true
//               freqStep( 50.0 )
//
//               Thread.sleep( 4000L )
////               val procs = cursor.step { implicit tx =>
////                  val group = access.get
//////                  group.iterator.toList.map( p => (p, p.freq.value) )
////                  group.iterator.toList.flatMap { case (_, seq) => seq.map { case (_, p) => (p, p.freq.value) }}
////               }
////               println( "\nFinal list of procs in group is " + procs.mkString( "[ ", ", ", " ]" ))
//            }
//         }).start()
//      }
//   }
//}
