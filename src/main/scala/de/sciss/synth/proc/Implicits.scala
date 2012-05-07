package de.sciss.synth.proc

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.event.NodeSerializer

class Implicits[ S <: Sys[ S ]] {
   implicit val procSerializer : NodeSerializer[ S, Proc[ S ]] = Proc.serializer[ S ]
}
