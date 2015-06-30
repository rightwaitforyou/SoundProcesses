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

import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.impl.{NodeRefImpl => Impl}
import de.sciss.topology.Topology

object NodeRef {
  // def apply(n: Node): NodeRef = Impl(n)
  def Group(name: String, in0: Full)(implicit tx: Txn): Group = Impl.Group(name, in0)

  trait Full extends NodeRef with Disposable[Txn] {
    def addAttrResources(key: String, values: List[Disposable[Txn]])(implicit tx: Txn): Unit

    /** Removes and frees resources associated with the attribute specified by `key` */
    def removeAttrResources(key: String)(implicit tx: Txn): Unit
  }

  trait Group extends Full with Disposable[Txn] {
    def addInstanceNode   (n: Full)(implicit tx: Txn): Unit
    def removeInstanceNode(n: Full)(implicit tx: Txn): Boolean
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