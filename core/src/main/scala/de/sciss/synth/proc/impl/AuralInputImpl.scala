/*
 *  AuralInputImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.synth.{BusNodeSetter, Node, NodeRef, Sys, Txn}
import de.sciss.synth.ControlABusMap
import de.sciss.synth.proc.graph.Attribute

object AuralInputImpl {
  def attr[S <: Sys[S]](data: AuralObj.ProcData[S], key: String, node: Node,
                        source: AuralOutput[S])(implicit tx: S#Tx): AuralInput[S] = {
    val controlName = Attribute.controlName(key)
    val mapper      = BusNodeSetter.mapper(controlName = controlName, bus = source.bus, node = node)
    new AttrImpl(data, source, node, key, mapper)
  }

  private final class AttrImpl[S <: Sys[S]](data: AuralObj.ProcData[S], source: AuralOutput[S],
                                            node: Node, key: String, mapper: BusNodeSetter)
    extends AuralInput[S] {

    override def toString: String = s"AuralInput($data, $source, $key)"

    // private[this] val mapper = Ref.make[BusNodeSetter]()

    def add   ()(implicit tx: Txn): Unit = {
      implicit val itx = tx.peer
      mapper.add()
      source.addSink(this)
      // mapper()        = m
    }

    def remove()(implicit tx: Txn): Unit = {
      implicit val itx = tx.peer
      // val m = mapper.swap(null)
      mapper.remove()
      // A `mapan` from a bus to which is not written doesn't
      // clear the control, so we end up with a noise loop from
      // whatever was in the last buffer. To avoid that, we
      // remove the `mapan` explicitly here.
      node.mapan(ControlABusMap.Multi(mapper.controlName, -1, source.bus.numChannels))
      source.removeSink(this)
    }

    def nodeRef(implicit tx: Txn): Option[NodeRef] = data.nodeOption
  }
}
