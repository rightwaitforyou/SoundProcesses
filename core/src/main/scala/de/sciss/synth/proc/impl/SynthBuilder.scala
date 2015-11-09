/*
 *  SynthBuilder.scala
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

package de.sciss.synth.proc.impl

import de.sciss.lucre.synth.{AuralNode, DynamicUser, Node, NodeRef, Resource, Server, Synth, Sys, Txn}
import de.sciss.synth.proc.Proc
import de.sciss.synth.{ControlSet, addToHead}

/** An object used in the last phase of playing a process. It has
  * an instantiated synth and allows the addition of controls, buses, users
  * and dependencies.
  *
  * @see  [[AuralProcImpl]]
  */
final class SynthBuilder(synth: Synth)
  extends NodeRef.Full {

  override def toString = s"SynthBuilder($synth)"

  private[this] val setMap = Vector.newBuilder[ControlSet]

  /** Users are elements which must be added after the
    * aural proc synth is started, and removed when it stops.
    */
  private[this] var users         = List.empty[DynamicUser]
  /** resources are dependencies in terms of synth bundle spawning,
    * and will be disposed by the aural proc.
    */
  private[this] var dependencies  = List.empty[Resource]

  private[this] var finished1 = false

  /** finishes building the `AuralNode`. Does not yet add the
    * users which is done through `finish2`, the reason being
    * that the caller may want to store the `AuralNode` as its
    * node-ref in the meantime, so users may find it.
    * Not pretty...
    */
  def finish1()(implicit tx: Txn): AuralNode = {
    require (!finished1)
    finished1      = true
    users         = users.reverse
    dependencies  = dependencies.reverse

    // XXX TODO
    val server  = synth.server
    val group   = server.defaultGroup

    val node = AuralNode(synth, users = users, resources = dependencies)

    // wrap as AuralProc and save it in the identifier map for later lookup
    synth.play(target = group, addAction = addToHead, args = setMap.result(),
      dependencies = dependencies)

//    users                  .foreach(_.add())
//    attrMap.foreach(_._2._1.foreach(_.add()))
    node
  }

  private[this] var finished2 = false

  /** Second stage of finishing is to add all users. */
  def finish2()(implicit tx: Txn): Unit = {
    require (!finished2)
    finished2      = true
    users.foreach(_.add())
  }

//  def addUser    (user    : DynamicUser): Unit = keyedUsers     ::= user
//  def addResource(resource: Resource   ): Unit = keyedResources ::= resource

  def addUser   (user: DynamicUser)(implicit tx: Txn): Unit = users ::= user
  def removeUser(user: DynamicUser)(implicit tx: Txn): Unit = users   = users.filterNot(_ == user)

  def addResource   (resource: Resource)(implicit tx: Txn): Unit = dependencies ::= resource
  def removeResource(resource: Resource)(implicit tx: Txn): Unit = dependencies.filterNot(_ == resource)

  def addControl (pair: ControlSet  )(implicit tx: Txn): Unit = setMap += pair

  def dispose()(implicit tx: Txn): Unit = {
    users       .foreach(_.dispose())
    dependencies.foreach(_.dispose())
  }

  def node(implicit tx: Txn): Node = synth

  def server: Server = synth.server
}

/** An object used in the preparatory phase of playing a process. It allows
  * the addition of asynchronous processors.
  */
final class AsyncProcBuilder[S <: Sys[S]](val obj: Proc[S]) {
  var resources = List.empty[AsyncResource[S]]
}