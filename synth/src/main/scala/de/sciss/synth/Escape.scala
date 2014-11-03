package de.sciss.synth

import java.io.DataOutputStream

object Escape {
  def write(graph: UGenGraph, dos: DataOutputStream): Unit = graph.write(dos)
}
