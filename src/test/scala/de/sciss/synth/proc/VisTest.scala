package de.sciss.synth.proc

import de.sciss.lucre.stm.{Cursor, Sys, InMemory}
import de.sciss.synth.expr.{Longs, ExprImplicits}
import de.sciss.lucre.expr.{Chronos, Span, SpanLike}
import java.awt.{BorderLayout, EventQueue}
import javax.swing.{WindowConstants, JFrame}
import de.sciss.nuages.VisualInstantPresentation
import de.sciss.synth
import de.sciss.confluent.{TemporalObjects, Confluent}
import de.sciss.lucre.stm.impl.BerkeleyDB
import java.io.File
import concurrent.stm.{Txn => STMTxn}

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
final class VisTest[ S <: Sys[ S ]]( system: S )( implicit cursor: Cursor[ S ]) extends ExprImplicits[ S ] {
   def t[ A ]( fun: S#Tx => A ) : A = {
      val peer = STMTxn.findCurrent
      require( peer.isEmpty, peer )
      cursor.step( fun )
   }

   type Acc = (ProcGroupX.Var[ S ], Transport[ S, Proc[ S ]])

   object Implicits {
      implicit val procVarSer       = ProcGroupX.varSerializer[ S ]
      implicit val accessTransport  = (tup: Acc) => tup._2
      implicit val transportSer     = Transport.serializer[ S, Acc ]( access ) // ( cursor, _._2 )
   }

   import Implicits._

   lazy val access: S#Entry[ Acc ] = system.root { implicit tx =>
      implicit def longType = Longs
      val g = ProcGroupX.newVar[ S ]
      g.changed.react { upd =>
         println( "Group observed: " + upd )
      }
      val tr = Transport( g, self = access )
      tr.changed.react { upd =>
         println( "Transport observed: " + upd )
      }
//      val trv  = tx.newVar[ Transport[ S, Proc[ S ]]]( tr.id, tr )
      (g, tr)
   }

   access // initialize !

   def group( implicit tx: S#Tx ) : ProcGroupX.Var[ S ]        = access.get._1
   def trans( implicit tx: S#Tx ) : Transport[ S, Proc[ S ]]   = access.get._2

   def proc( name: String )( implicit tx: S#Tx ) : Proc[ S ] = {
      implicit val chr: Chronos[ S ] = Chronos(0L)
      val p    = Proc[ S ]()
      p.name   = name
      p.graph     = {
         import synth._
         import ugen._
         val f = "freq".kr       // fundamental frequency
         val p = 20              // number of partials per channel
         val m = Mix.tabulate(p) { i =>
            FSinOsc.ar(f * (i+1)) *
               (LFNoise1.kr(Seq(Rand(2, 10), Rand(2, 10))) * 0.02).max(0)
         }
         Out.ar( 0, m )
      }
      p.playing = true
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
      t { implicit tx => trans.playing = true }
   }

   def stop() {
      t { implicit tx => trans.playing = false }
   }

   def rewind() { seek( 0L )}

   def seek( pos: Long ) { t { implicit tx =>
      trans.playing = false
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

   private var frameVar: JFrame = null
   def frame = frameVar

   def gui() {
      if( frame == null ) defer {
         val f    = new JFrame( "Vis" )
         frameVar = f
         val cp   = f.getContentPane
         val vis  = VisualInstantPresentation( access )
         cp.add( vis.view, BorderLayout.CENTER )
         f.pack()
         f.setDefaultCloseOperation( WindowConstants.EXIT_ON_CLOSE )
         f.setLocationRelativeTo( null )
         f.setLocation( f.getX, 0 )
         f.setAlwaysOnTop( true )
         f.setVisible( true )
      }
   }

   private var auralVar: AuralPresentation[ S ] = null

   def aural() {
      if( auralVar == null ) auralVar = t { implicit tx =>
         AuralPresentation.run( access )
      }
   }

   implicit def richNum( d: Double ) : RichDouble = new RichDouble( d )

   final class RichDouble private[VisTest]( d: Double ) {
      def sec : Long = (d * 44100).toLong
   }
}
