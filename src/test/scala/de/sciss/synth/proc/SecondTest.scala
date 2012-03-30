package de.sciss.synth.proc

import de.sciss.synth
import synth.{SynthDef, Server, expr}
import expr._
import de.sciss.lucre.stm.{Sys, Cursor, InMemory}
import synth._; import ugen._

object SecondTest {
   def main( args: Array[ String ]) {
      implicit val system: InMemory = InMemory()
      run[ InMemory ]()
   }

   def run[ S <: Sys[ S ]]()( implicit system: S, cursor: Cursor[ S ]) {
      implicit val whyOhWhy = ProcGroup.serializer[ S ]

      def group()( implicit tx: S#Tx ) : ProcGroup[ S ] = ProcGroup.empty
      def proc()(  implicit tx: S#Tx ) : Proc[ S ]      = Proc()

      val access = system.root { implicit tx => group() }

      def addProc( name: String )( graph: => Any ) {
         cursor.step { implicit tx =>
            val group   = access.get
            val p       = proc()
            p.name      = name
            group.add( p )
            p.graph     = graph
         }
      }

      addProc( "one" ) {
         val f = LFSaw.kr( 0.4 ).madd( 24, LFSaw.kr( Seq( 8, 7.23 )).madd( 3, 80 )).midicps
         val c = CombN.ar( SinOsc.ar( f ) * 0.04, 0.2, 0.2, 4 )
         Out.ar( 0, c )
      }

      Auralization.run( access )

      (new Thread {
         override def run() {
            Thread.sleep( 2000L )
            addProc( "two" ) {
               val f = RLPF.ar( LFPulse.ar( SinOsc.kr( 0.2 ).madd( 10, 21 ), 0.1 ), 100, 0.1 ).clip2( 0.4 )
               Out.ar( 0, Seq( f, f ))
            }
            Thread.sleep( 2000L )
            cursor.step { implicit tx =>
               val group = access.get
               val procs = group.iterator.toIndexedSeq
               procs.foreach( group.remove( _ ))
            }
            Thread.sleep( 2000L )
            sys.exit( 0 )
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
