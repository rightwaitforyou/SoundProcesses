package de.sciss.synth.proc

import de.sciss.lucre.stm.Sys

trait Transport[ S <: Sys[ S ]] {
   def play()( implicit tx: S#Tx, time: TimeSource[ S ]) : Unit
   def stop()( implicit tx: S#Tx, time: TimeSource[ S ]) : Unit
}
