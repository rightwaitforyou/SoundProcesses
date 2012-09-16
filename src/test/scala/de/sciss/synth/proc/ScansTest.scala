package de.sciss.synth
package proc

import de.sciss.lucre.stm.{Durable, InMemory, Sys}
import expr.{Longs, Doubles, ExprImplicits}
import de.sciss.lucre.bitemp.{Chronos, Span}
import de.sciss.lucre.stm.impl.BerkeleyDB
import java.io.File

object ScansTest extends App {
   type S = Durable
   val imp  = new ExprImplicits[ S ]
   import imp._

   args.headOption match {
      case Some( "--prelim" ) => preliminaryTest()
      case _ => run()
   }

   def makeSys() : Durable = {
      val dir = File.createTempFile( "scans", "db" )
      dir.delete()
      Durable( BerkeleyDB.factory( dir ))
   }

   def preliminaryTest() {
      val sys  = makeSys()
      val scan = sys.step { implicit tx =>
         val _scan = Scan_.Modifiable[ S ]
         _scan.changed.react { upd =>
            println( "OBSERVED: " + upd )
         }
         _scan
      }

      sys.step { implicit tx =>
         scan.add(     0L, Scan_.Mono( 441 ))
         scan.add( 10000L, Scan_.Mono( 333 ))
         scan.add(  5000L, Scan_.Synthesis() )
         val v = Doubles.newVar[ S ]( 666 )
         scan.add( 15000L, Scan_.Mono( v ))
         v.set( 777 )
      }

//      println( "\n---step2---\n" )
//
//      sys.step { implicit tx =>
//         val sc2 = Scan_.Modifiable[ S ]
//         val v2 = Longs.newVar[ S ]( 20000L )
//         scan.add( 20000L, Scan_.Embedded( sc2, v2 ))
//         sc2.add( 0L, Scan_.Mono( 888 ))
//         v2.set( 30000L )
//      }
   }

   def run() {
      implicit val sys = makeSys()
      lazy val server: AuralSystem = AuralSystem().start().whenStarted { _ =>
//         Thread.sleep( 1000 )
         sys.step { implicit tx =>
            val group   = ProcGroup_.Modifiable[ S ]
            val transp  = Transport( group )
            /* val view = */ AuralPresentation.run( transp, server )
            test( group )
            transp.playing_=( true )
         }
      }
      server
//      Thread.sleep( 1000 )
   }

   def test /* [ S <: Sys[ S ]] */( group: ProcGroup_.Modifiable[ S ])( implicit tx: S#Tx ) {

      SoundProcesses.showLog = true

      val p1 = Proc[ S ]()
      val p2 = Proc[ S ]()

      val t1 = 1 /* 4 */ * 44100L   // XXX TODO eventually should appear later
      val t2 = 1 * 44100L // XXX TODO must currently be greater than current transport position

      val tp1 = group.add( Span.from( t1 ), p1 )
      group.add( Span.from( t2 ), p2 )

      val fScan1: Scan[ S ] = Scan_.Modifiable[ S ]
      p1.scans.add( "out", fScan1 )
      val fScan2: Scan[ S ] = Scan_.Modifiable[ S ]
      p2.scans.add( "freq", fScan2 )
//      for( s1 <- p1.scans.get( "out" ); Scan_.Modifiable( s2 ) <- p2.scans.get( "freq" )) {
//         s2.add( 0L, Scan_.Embedded( s1, 0L ))
//      }
      for( Scan_.Modifiable( s2 ) <- p2.scans.get( "freq" )) {
         s2.add( 0L, Scan_.Embedded( tp1, "out", 0L ))
      }

      for( Scan_.Modifiable( s1 ) <- p1.scans.get( "out" )) {
//         s1.add( 0L, Scan_.Mono( 441 ))
         s1.add( 0L, Scan_.Synthesis() )  // XXX TODO should be written by the scan.Out itself
      }

      import ugen._
      import graph.scan

      p1.graph_=( SynthGraph {
         scan( "out" ) := SinOsc.ar( 100 ).linexp( -1, 1, 30, 3000 )
      })

      p2.graph_=( SynthGraph {
         val freq = scan( "freq" ).ar( 333 )
//         freq.poll
         Out.ar( 0, SinOsc.ar( freq ))
      })

//      {
//         implicit val chr = Chronos[ S ]( t1 )
//         p1.playing_=( true )
//      }
//
//      {
//         implicit val chr = Chronos[ S ]( t2 )
//         p2.playing_=( true )
//      }
   }
}
