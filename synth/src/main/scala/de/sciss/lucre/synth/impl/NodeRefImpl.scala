///*
// *  NodeRefImpl.scala
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
//package de.sciss.lucre.synth
//package impl
//
//import de.sciss.lucre.stm.TxnLike
//import de.sciss.lucre.synth.NodeRef.Full
//import de.sciss.synth.ControlSet
//
//import scala.concurrent.stm.Ref
//
//object NodeRefImpl {
//  def Var(init: Full): NodeRef.Var = new VarImpl(init.server, Ref(init))
//
//  private final class VarImpl(val server: Server, ref: Ref[Full]) extends NodeRef.Var {
//    import TxnLike.peer
//
//    def apply ()       (implicit tx: Txn): Full = ref()
//    def update(v: Full)(implicit tx: Txn): Unit = ref() = v
//
//    def node(implicit tx: Txn): Node = ref().node
//
//    def dispose()(implicit tx: Txn): Unit = ref().dispose()
//
//    def addUser   (user: DynamicUser)(implicit tx: Txn): Unit = ref().addUser   (user)
//    def removeUser(user: DynamicUser)(implicit tx: Txn): Unit = ref().removeUser(user)
//
//    def addResource   (resource: Resource)(implicit tx: Txn): Unit = ref().addResource   (resource)
//    def removeResource(resource: Resource)(implicit tx: Txn): Unit = ref().removeResource(resource)
//
//    def addControl(pair: ControlSet)(implicit tx: Txn): Unit = ref().addControl(pair)
//  }
//}