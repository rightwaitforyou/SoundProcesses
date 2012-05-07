package de.sciss.synth.proc

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.expr.TimeSource

trait Transport[ S <: Sys[ S ]] {
   def play()( implicit time: TimeSource[ S ]) : Unit
   def stop()( implicit time: TimeSource[ S ]) : Unit
}
