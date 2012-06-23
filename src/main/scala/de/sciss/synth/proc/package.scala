package de.sciss.synth

import de.sciss.lucre.stm.{Sys, InMemory}
import de.sciss.lucre.expr.BiGroup

package object proc {
   private[proc] type I = InMemory

   type ProcGroup[ S <: Sys[ S ]] = BiGroup[ S, Proc[ S ], Proc.Update[ S ]]
}