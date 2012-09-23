package de.sciss.synth.proc

import de.sciss.lucre.{stm, bitemp, expr}
import stm.{Serializer, Cursor, Sys, InMemory}
import stm.impl.BerkeleyDB
import bitemp.{BiType, BiGroup, BiPin, Chronos, Span, SpanLike}
import expr.Expr
import java.awt.{BorderLayout, EventQueue}
import javax.swing.{WindowConstants, JFrame}
//import de.sciss.nuages.VisualInstantPresentation
import de.sciss.synth
import de.sciss.confluent.Confluent
import java.io.File
import concurrent.stm.{Txn => STMTxn, Ref => STMRef}
import synth.expr.{SpanLikes, Longs, ExprImplicits}
import synth.SynthGraph

object VisTest {
   def apply() : VisTest[ InMemory ] = {
      implicit val system = InMemory()
      new VisTest( system )
   }

   def dataDir = new File( new File( sys.props( "user.home" ), "sound_processes" ), "db" )

   def conf() : VisTest[ Confluent ] = {
      val dir              = dataDir
      dir.mkdirs()
      val store            = BerkeleyDB.factory( dir )
      implicit val system  = Confluent( store )
      new VisTest( system )
   }

   def wipe( sure: Boolean = false ) {
      if( !sure ) return
      dataDir.listFiles().foreach( _.delete() )
      dataDir.delete()
   }

   def main( args: Array[ String ]) {
//      TemporalObjects.showConfluentLog = true
      val vis = VisTest.conf()
      import vis._
      add()
      aural()
      Thread.sleep(8000L)
      play()
   }
}
final class VisTest[ Sy <: Sys[ Sy ]]( system: Sy )( implicit cursor: Cursor[ Sy ]) extends ExprImplicits[ Sy ] {
   type S  = Sy
   type Tx = S#Tx

   def t[ A ]( fun: S#Tx => A ) : A = {
      val peer = STMTxn.findCurrent
      require( peer.isEmpty, peer )
      cursor.step( fun )
   }

//   private type PG = ProcGroup.Modifiable[ S ]
   private type PG = BiGroup.Modifiable[ S, Proc[ S ], Proc.Update[ S ]]
   type Acc = PG // (PG, ProcTransport[ S ])

   object Implicits {
//      implicit def procVarSer: Serializer[ S#Tx, S#Acc, PG ] = ProcGroup.Modifiable.serializer[ S ]
      implicit val spanLikes: BiType[ SpanLike ] = SpanLikes
      implicit val procVarSer: Serializer[ S#Tx, S#Acc, PG ] = BiGroup.Modifiable.serializer[ S, Proc[ S ], Proc.Update[ S ]]( _.changed )
//      implicit val accessTransport: Acc => Transport[ S, Proc[ S ]] = _._2
//      implicit val transportSer: Serializer[ S#Tx, S#Acc, ProcTransport[ S ]] = ??? // Transport.serializer[ S ]( cursor )
   }

   import Implicits._

   lazy val access: S#Entry[ Acc ] = system.root { implicit tx =>
      implicit def longType = Longs
      val g = ProcGroup_.Modifiable[ S ]
      g.changed.react { upd =>
         println( "Group observed: " + upd )
      }
      g // (g, tr)
   }

   access // initialize !

   val trans = cursor.step { implicit tx =>
      val g = access.get
      val tr = Transport( g )
      tr.react { upd =>
         println( "Transport observed: " + upd )
      }
      tr
   }
//      val trv  = tx.newVar[ Transport[ S, Proc[ S ]]]( tr.id, tr )


//   val groupAccess:     Source[ S#Tx, ProcGroup.Modifiable[ S ]] = Source.map( access )( _._1 )
//   val transportAccess: Source[ S#Tx, Transport[ S, Proc[ S ]]]   = Source.map( access )( _._2 )

   def group( implicit tx: S#Tx ) : ProcGroup_.Modifiable[ S ] = access.get // ._1
//   def trans( implicit tx: S#Tx ) : ProcTransport[ S ]         = access.get._2

   def proc( name: String )( implicit tx: S#Tx ) : Proc[ S ] = {
      implicit val chr: Chronos[ S ] = Chronos(0L)
      val p = Proc[ S ]
      p.name_=( name )
      p.graph_=( SynthGraph {
         import synth._
         import ugen._
         val f = "freq".kr       // fundamental frequency
         val p = 20              // number of partials per channel
         val m = Mix.tabulate(p) { i =>
            FSinOsc.ar(f * (i+1)) *
               (LFNoise1.kr(Seq(Rand(2, 10), Rand(2, 10))) * 0.02).max(0)
         }
//         Out_~( "sig", WhiteNoise.ar )
//         SinOsc.ar( In_~( "freq", 441 ))
//         Out_!( "trig", Dust.kr( 1 ))
//         Decay.ar( In_!( "trig2" ))
//         Trigger( "silence", DetectSilence.ar( m ))

         Out.ar( 0, m )
      })

//      p.trigger( "silence" )

//      p.playing_=( true )
      p
   }

   def next( time: Long ) : Option[ Long ] = t { implicit tx =>
      group.nearestEventAfter( time )
   }

   def prev( time: Long ) : Option[ Long ] = t { implicit tx =>
      group.nearestEventBefore( time )
   }

   def clear() { t { implicit tx =>
      group.clear()
   }}

   def add( span: SpanLike = Span( 3*44100, 6*44100 ), name: String = "Proc" ) {
      t { implicit tx =>
         val p = proc( name )
         group.add( span, p )
      }
   }

   def play() {
//      t { implicit tx => trans.playing = true }
      t { implicit tx => trans.play() }
   }

   def stop() {
//      t { implicit tx => trans.playing = false }
      t { implicit tx => trans.stop() }
   }

   def rewind() { seek( 0L )}

   def seek( pos: Long ) { t { implicit tx =>
//      trans.playing = false
      trans.stop()
      trans.seek( pos )
   }}

   def within( span: SpanLike ) = t { implicit tx =>
      group.intersect( span ).toIndexedSeq
   }

   def range( start: SpanLike, stop: SpanLike ) = t { implicit tx =>
      group.rangeSearch( start, stop ).toIndexedSeq
   }

   def defer( thunk: => Unit ) { EventQueue.invokeLater( new Runnable {
      def run() { thunk }
   })}

//   private var frameVar: JFrame = null
//   def frame = frameVar
//
//   private val visVar = STMRef( Option.empty[ VisualInstantPresentation[ S ]])
//
//   def gui() { t { implicit tx =>
//      implicit val itx = tx.peer
//      if( visVar().isEmpty ) {
//         val vis = VisualInstantPresentation( trans )
//         visVar.set( Some( vis ))
//         STMTxn.afterCommit( _ => defer {
//            val f    = new JFrame( "Vis" )
//            frameVar = f
//            val cp   = f.getContentPane
//            cp.add( vis.view, BorderLayout.CENTER )
//            f.pack()
//            f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
//            f.setLocationRelativeTo( null )
//            f.setLocation( f.getX, 0 )
//            f.setAlwaysOnTop( true )
//            f.setVisible( true )
//         })
//      }
//   }}

   private val auralVar = STMRef( Option.empty[ AuralPresentation[ S ]])

   def aural() { t { implicit tx =>
      implicit val itx = tx.peer
      if( auralVar().isEmpty ) {
         val as = AuralSystem()
         as.start()  // XXX TODO non-transactional
         auralVar.set( Some( AuralPresentation.run( trans, as )))
      }
   }}

   def pr( time: Long = 4 * 44100 )( implicit tx: S#Tx ) = group.intersect( time ).next._2.head.value

   def addFreq( time: Expr[ S, Long ] = 0, freq: Expr[ S, Param ]) {
      ???
//      t { implicit tx =>
//         pr().scans.get( "freq" ) match {
//            case Some( BiPin.Modifiable( v )) => v.add( time, Scan_.Mono( freq ))
//            case _ =>
//         }
//      }
   }

   implicit def richNum( d: Double ) : RichDouble = new RichDouble( d )

   final class RichDouble private[VisTest]( d: Double ) {
      def sec : Long = (d * 44100).toLong
   }
}
