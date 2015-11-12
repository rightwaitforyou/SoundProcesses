///*
// *  NodeGroupRefImpl.scala
// *  (SoundProcesses)
// *
// *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
// *
// *	This software is published under the GNU General Public License v2+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.synth.proc
//package impl
//
//import de.sciss.lucre.stm.TxnLike
//import de.sciss.lucre.synth
//import de.sciss.lucre.synth.{Resource, DynamicUser, Node, Txn, NodeRef}
//import de.sciss.synth.{ControlSet, addBefore}
//
//import scala.concurrent.stm.Ref
//
//// dynamically flips between single proc and multiple procs
//// (wrapping them in one common group)
//final class NodeGroupRefImpl(name: String, in0: AuralNode) extends NodeGroupRef {
//  import TxnLike.peer
//
//  val server = in0.server
//
//  override def toString = name
//
//  private val instancesRef  = Ref(in0 :: Nil)
//  private val nodeRef       = Ref[NodeRef](in0)
//
//  def node(implicit tx: Txn): Node = nodeRef().node
//
//  def addInstanceNode(n: AuralNode)(implicit tx: Txn): Unit = {
//    val old = instancesRef.getAndTransform(n :: _)
//    old match {
//      case single :: Nil =>
//        val g = synth.Group(single.node, addBefore)
//        nodeRef() = g
//        single.node.moveToHead(g)
//        n     .node.moveToHead(g)
//
//      case _ =>
//    }
//  }
//
//  def removeInstanceNode(n: AuralNode)(implicit tx: Txn): Boolean = {
//    val after = instancesRef.transformAndGet(_.filterNot(_ == n))
//    after match {
//      case single :: Nil =>
//        val group = nodeRef.swap(single).node
//        single.node.moveBefore(group)
//        group.free()
//        false
//
//      case Nil  =>
//        dispose()
//        true
//
//      case _ => false
//    }
//  }
//
//  def instanceNodes(implicit tx: Txn): Iterator[AuralNode] = instancesRef().iterator
//
//  def addUser(user: DynamicUser)(implicit tx: Txn): Unit =
//    instancesRef().foreach(_.addUser(user))
//
//  def removeUser(user: DynamicUser)(implicit tx: Txn): Unit =
//    instancesRef().foreach(_.removeUser(user))
//
//  def addResource(resource: Resource)(implicit tx: Txn): Unit =
//    instancesRef().foreach(_.addResource(resource))
//
//  def removeResource(resource: Resource)(implicit tx: Txn): Unit =
//    instancesRef().foreach(_.removeResource(resource))
//
//  def addControl(pair: ControlSet)(implicit tx: Txn): Unit =
//    instancesRef().foreach(_.addControl(pair))
//
//  def dispose()(implicit tx: Txn): Unit = {
//    if (instancesRef.swap(Nil).size > 1) {
//      val group = nodeRef.swap(null).node
//      group.free()
//    }
//    server.removeVertex(this)
//  }
//}
