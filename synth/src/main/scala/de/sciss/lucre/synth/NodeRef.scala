/*
 *  NodeRef.scala
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

package de.sciss.lucre.synth

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.impl.{NodeRefImpl => Impl}
import de.sciss.synth.ControlSet
import de.sciss.topology.Topology

object NodeRef {
  def Group(name: String, in0: AuralNode)(implicit tx: Txn): Group = Impl.Group(name, in0)

  object Var {
    def apply(init: NodeRef.Full): Var = Impl.Var(init)
  }
  trait Var extends NodeRef.Full with stm.Source[Txn, NodeRef.Full] with stm.Sink[Txn, NodeRef.Full]

  trait Full extends NodeRef with Disposable[Txn] {
//    def addAttrResources(key: String, values: List[Disposable[Txn]])(implicit tx: Txn): Unit
//
//    /** Removes and frees resources associated with the attribute specified by `key` */
//    def removeAttrResources(key: String)(implicit tx: Txn): Unit

    def addUser       (user: DynamicUser )(implicit tx: Txn): Unit
    def removeUser    (user: DynamicUser )(implicit tx: Txn): Unit

    def addResource   (resource: Resource)(implicit tx: Txn): Unit
    def removeResource(resource: Resource)(implicit tx: Txn): Unit

    def addControl(pair: ControlSet)(implicit tx: Txn): Unit
  }

  trait Group extends Full with Disposable[Txn] {
    def addInstanceNode   (n: AuralNode)(implicit tx: Txn): Unit
    def removeInstanceNode(n: AuralNode)(implicit tx: Txn): Boolean
    def instanceNodes(implicit tx: Txn): Iterator[AuralNode]
  }

  final case class Edge(source: NodeRef, sink: NodeRef)
    extends Topology.Edge[NodeRef] {

    def sourceVertex = source
    def targetVertex = sink
  }
}
trait NodeRef {
  def server: Server
  def node(implicit tx: Txn): Node
}