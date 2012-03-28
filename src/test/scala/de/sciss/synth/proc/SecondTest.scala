package de.sciss.synth.proc

import de.sciss.synth
import synth.{SynthDef, Server, expr}
import expr._
import de.sciss.lucre.stm.{Sys, Cursor, InMemory}

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

      cursor.step { implicit tx =>
         val group   = access.get
         val p       = proc()
         group.add( p )
         p.graph = {
            import synth._; import ugen._
            val f = LFSaw.kr( 0.4 ).madd( 24, LFSaw.kr( Seq( 8, 7.23 )).madd( 3, 80 )).midicps
            val c = CombN.ar( SinOsc.ar( f ) * 0.04, 0.2, 0.2, 4 )
            Out.ar( 0, c )
         }
      }

//      Server.run { s =>
//         val sd = SynthDef( "test", gr.expand )
//         sd.play
//         Thread.sleep( 4000 )
//         sys.exit( 0 )
//      }
   }
}
