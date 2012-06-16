package de.sciss.synth.proc

import de.sciss.lucre.expr.{Expr, Chronos}
import de.sciss.lucre.stm.{Writer, Disposable, Sys}

object Transport {
   def apply[ S <: Sys[ S ]]( group: ProcGroup[ S ], sampleRate: Double = 44100 )
                            ( implicit tx: S#Tx /*, longs: BiType[ Long ]*/) : Transport[ S ] =
      impl.TransportImpl( group, sampleRate )
}
trait Transport[ S <: Sys[ S ]] extends Chronos[ S ] with Writer with Disposable[ S#Tx ] {
   def seek( time: Long )( implicit tx: S#Tx ) : Unit
   def playing( implicit tx: S#Tx ) : Expr[ S, Boolean ]
   def playing_=( expr: Expr[ S, Boolean ])( implicit tx: S#Tx ) : Unit

   def sampleRate: Double

//   def play()( implicit time: Chronos[ S ]) : Unit
//   def stop()( implicit time: Chronos[ S ]) : Unit
}
