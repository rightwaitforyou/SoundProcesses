package de.sciss.lucre.event

import de.sciss.lucre.stm.Sys

// get access to package private members
object Peek {
  def targets[S <: Sys[S]](in: Node[S])(implicit tx: S#Tx): Children[S] = in._targets.children // .children
}
