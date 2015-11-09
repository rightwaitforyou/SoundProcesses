///*
// *  AuralInput.scala
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
//
//import de.sciss.lucre.synth.{Node, Txn, NodeRef, DynamicUser, Sys}
//import impl.{AuralInputImpl => Impl}
//
//object AuralInput {
//  def attr1[S <: Sys[S]](data: AuralObj.ProcData[S], key: String, node: Node,
//                        source: AuralOutput[S])(implicit tx: S#Tx): AuralInput[S] =
//    Impl.attr(data, key, node, source)
//}
//
///** The aural representation of _a proc as an input_. Perhaps it should
//  * be renamed to `AuralInputProc`.
//  */
//trait AuralInput[S <: Sys[S]] extends /* Disposable[S#Tx] with */ DynamicUser {
//  //   def addSource   (view: AuralOutput[S])(implicit tx: S#Tx): Unit
//  //   def removeSource(view: AuralOutput[S])(implicit tx: S#Tx): Unit
//
//  // def sourceUpdated(view: AuralOutput[S])(implicit tx: S#Tx): Unit
//
//  def nodeRef(implicit tx: Txn): Option[NodeRef]
//}