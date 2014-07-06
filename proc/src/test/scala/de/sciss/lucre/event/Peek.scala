package de.sciss.lucre.event

object Peek {
  def targets[S <: Sys[S]](in: Node[S])(implicit tx: S#Tx) = in.children
}
