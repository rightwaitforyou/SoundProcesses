package de.sciss.synth.proc

import de.sciss.lucre.stm.Sys
import de.sciss.synth._
import ugen._

object Signal {
   implicit def toGE( signal: Signal ) : GE = sys.error( "TODO" )
}
trait Signal {
   def <<( ge: GE ) {}
}

abstract class Proc1[ S <: Sys[ S ]]( name: String, tx0: S#Tx ) {
   final protected def graph( fun: => Unit ) {}
   final protected def defaultOut : Signal = sys.error( "TODO" )
   final protected def signal( name: String ) : Signal = sys.error( "TODO" )
}

class MyProc1[ S <: Sys[ S ]]( tx: S#Tx ) extends Proc1( "oscillator", tx ) {
   val freq = signal( "freq" )
   val out  = defaultOut

   graph {
      out << SinOsc.ar( freq )
   }
}

trait Test1[ S <: Sys[ S ]] {
   def test( implicit tx: S#Tx ) {
      val p = new MyProc1( tx )
      p.freq << 441
   }
}