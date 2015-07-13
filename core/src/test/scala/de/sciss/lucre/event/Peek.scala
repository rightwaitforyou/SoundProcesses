package de.sciss.lucre.event

// get access to package private members
object Peek {
  def targets[S <: Sys[S]](in: Node[S])(implicit tx: S#Tx): Children[S] = in.children
}
