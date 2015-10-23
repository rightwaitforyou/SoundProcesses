package de.sciss.synth.proc
package impl

import de.sciss.lucre.synth.{BusNodeSetter, NodeRef, Sys, Txn}
import de.sciss.synth.ControlABusMap
import de.sciss.synth.proc.graph.Attribute

object AuralInputImpl {
  def attr[S <: Sys[S]](nodeRef: NodeRef, key: String, source: AuralOutput[S])(implicit tx: S#Tx): AuralInput[S] = {
    val controlName = Attribute.controlName(key)
    val m           = BusNodeSetter.mapper(controlName = controlName, bus = source.bus, node = nodeRef.node)
    new AttrImpl(nodeRef, source, key, m)
  }

  private final class AttrImpl[S <: Sys[S]](val nodeRef: NodeRef, source: AuralOutput[S], key: String,
                                            mapper: BusNodeSetter)
    extends AuralInput[S] {

    override def toString: String = s"AuralInput($nodeRef, $source, $key)"

    def add   ()(implicit tx: Txn): Unit = {
      source.addSink(this)
      mapper.add()
    }

    def remove()(implicit tx: Txn): Unit = {
      mapper.remove()
      nodeRef.node.mapan(ControlABusMap.Multi(mapper.controlName, -1, source.bus.numChannels))
      source.removeSink(this)
    }
  }
}
