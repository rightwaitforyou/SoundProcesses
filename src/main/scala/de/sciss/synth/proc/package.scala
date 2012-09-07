package de.sciss.synth

import de.sciss.lucre.stm.{Sys, InMemory}
import de.sciss.lucre.bitemp.{BiPin, BiGroup}

package object proc {
   private[proc] type I = InMemory

   type ProcGroup[ S <: Sys[ S ]] = BiGroup[ S, Proc[ S ], Proc.Update[ S ]]
   type Param = Double

//   type ScanElem[ S <: Sys[ S ]] = de.sciss.synth.proc.Scan.Elem

//   type Scan[ S <: Sys[ S ]] = BiPin.Expr[ S, Scan_.Elem[ S ]]
   type Scan[ S <: Sys[ S ]] = BiPin[ S, Scan_.Elem[ S ], Scan_.Elem.Update[ S ]]
}