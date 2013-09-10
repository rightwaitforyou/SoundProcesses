package de.sciss.lucre.synth

case class ProcEdge(source: AuralProc, sourceKey: String, sink: AuralProc, sinkKey: String)
  extends Topology.Edge[AuralProc] {
  
  def sourceVertex = source
  def targetVertex = sink
}