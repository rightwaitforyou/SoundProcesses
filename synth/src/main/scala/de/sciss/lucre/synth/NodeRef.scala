/*
 *  NodeRef.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
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

object NodeRef {
  // def apply(n: Node): NodeRef = Impl(n)
  def Group(name: String, in0: NodeRef)(implicit tx: Txn): Group = Impl.Group(name, in0)

  trait Group extends NodeRef with Disposable[Txn] {
    def addInstanceNode   (n: NodeRef)(implicit tx: Txn): Unit
    def removeInstanceNode(n: NodeRef)(implicit tx: Txn): Boolean
  }
}
trait NodeRef {
  def server: Server
  def node(implicit tx: Txn): Node
}