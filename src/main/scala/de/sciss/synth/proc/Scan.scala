package de.sciss.synth.proc

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.expr.Expr
import de.sciss.synth.Env

object Scan_ {
   sealed trait Elem[ S <: Sys[ S ]]
   final case class Mono[ S <: Sys[ S ]]( targetLevel: Expr[ S, Double ], shape: Env.Shape ) extends Elem[ S ]
//   final case class AudioFile[ S <: Sys[ S ]]( f: File, offset: Expr[ S, Long ]) extends Elem[ S ]
//   final case class Graph[ S <: Sys[ S ]]( func: Expr[ S, SynthGraph ]) extends Elem[ S ]
   final case class Synthesis[ S <: Sys[ S ]]() extends Elem[ S ]
   final case class Embedded[ S <: Sys[ S ]]( ref: Scan[ S ], offset: Expr[ S, Long ]) extends Elem[ S ]
}
