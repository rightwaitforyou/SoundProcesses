package de.sciss.synth.proc

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.expr.Chronos

trait Transport[ S <: Sys[ S ]] {
   def play()( implicit time: Chronos[ S ]) : Unit
   def stop()( implicit time: Chronos[ S ]) : Unit
}
