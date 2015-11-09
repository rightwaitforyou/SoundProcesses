/*
 *  NodeGroupRef.scala
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

import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.{NodeRef, Txn}
import de.sciss.synth.proc.impl.{NodeGroupRefImpl => Impl}

object NodeGroupRef {
  def apply(name: String, in0: AuralNode)(implicit tx: Txn): NodeGroupRef = {
    val res = new Impl(name, in0)
    in0.server.addVertex(res)
    res
  }
}
trait NodeGroupRef extends NodeRef.Full with Disposable[Txn] {
  def addInstanceNode   (n: AuralNode)(implicit tx: Txn): Unit
  def removeInstanceNode(n: AuralNode)(implicit tx: Txn): Boolean
  def instanceNodes(implicit tx: Txn): Iterator[AuralNode]
}