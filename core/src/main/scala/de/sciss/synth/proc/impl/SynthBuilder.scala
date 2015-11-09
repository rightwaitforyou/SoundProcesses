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

import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.{Server, NodeRef, Node, AuralNode, Txn, AudioBus, Resource, DynamicUser, Synth, Sys}
import de.sciss.synth.{addToHead, ControlSet}
import de.sciss.synth.proc.{NodeDependencyBuilder, TimeRef, Proc}

/** An object used in the last phase of playing a process. It has
  * an instantiated synth and allows the addition of controls, buses, users
  * and dependencies.
  *
  * @see  [[AuralProcImpl]]
  */
final class SynthBuilder[S <: Sys[S]](val obj: Proc[S], val synth: Synth, val timeRef: TimeRef)
  extends NodeDependencyBuilder[S] {

  override def toString = s"SynthBuilder($obj, $synth, $timeRef)"

  val setMap        = Vector.newBuilder[ControlSet]

  /** Users are elements which must be added after the
    * aural proc synth is started, and removed when it stops.
    */
  var users         = List.empty[DynamicUser]
  /** resources are dependencies in terms of synth bundle spawning,
    * and will be disposed by the aural proc.
    */
  var dependencies  = List.empty[Resource]

  var outputBuses   = Map.empty[String, AudioBus]
  var inputBuses    = Map.empty[String, AudioBus]

  private var attrMap = Map.empty[String, (List[DynamicUser], List[Resource])]

  /** finishes building the `AuralNode`. Does not yet add the
    * users which is done through `finish2`, the reason being
    * that the caller may want to store the `AuralNode` as its
    * node-ref in the meantime, so users may find it.
    * Not pretty...
    */
  def finish1()(implicit tx: Txn): AuralNode = {
    // XXX TODO
    val server  = synth.server
    val group   = server.defaultGroup

    val attrMap1 = attrMap.map { case (key, value) => (key, value._1 ::: value._2) }

    val node = AuralNode(synth, inputBuses = inputBuses, outputBuses = outputBuses,
      resources = users ::: dependencies, attrMap = attrMap1)

    // wrap as AuralProc and save it in the identifier map for later lookup
    synth.play(target = group, addAction = addToHead, args = setMap.result(),
      dependencies = dependencies)

//    users                  .foreach(_.add())
//    attrMap.foreach(_._2._1.foreach(_.add()))
    node
  }

  /** Second stage of finishing is to add all users. */
  def finish2()(implicit tx: Txn): Unit = {
    users                  .foreach(_.add())
    attrMap.foreach(_._2._1.foreach(_.add()))
  }

  // copies the node-dependency-builder stuff to a map entry
  def storeKey(key: String): Unit =
    if (keyedUsers.nonEmpty || keyedResources.nonEmpty) {
      attrMap += key -> (keyedUsers -> keyedResources)
      keyedUsers      = Nil
      keyedResources  = Nil
    }

  // ---- node-dependency-builder ----

  // def node = synth

  def addControl(pair: ControlSet): Unit = setMap += pair

  private var keyedUsers      = List.empty[DynamicUser]
  private var keyedResources  = List.empty[Resource   ]

  def addUser    (user    : DynamicUser): Unit = keyedUsers     ::= user
  def addResource(resource: Resource   ): Unit = keyedResources ::= resource

  def addUser   (user: DynamicUser)(implicit tx: Txn): Unit = ???
  def removeUser(user: DynamicUser)(implicit tx: Txn): Unit = ???

  def addResource(resource: Resource)(implicit tx: Txn): Unit = ???
  def addControl (pair: ControlSet  )(implicit tx: Txn): Unit = ???

  def dispose()(implicit tx: Txn): Unit = ???

  def node(implicit tx: Txn): Node = synth

  def server: Server = synth.server
}

/** An object used in the preparatory phase of playing a process. It allows
  * the addition of asynchronous processors.
  */
final class AsyncProcBuilder[S <: Sys[S]](val obj: Proc[S]) {
  var resources = List.empty[AsyncResource[S]]
}

final class SynthUpdater[S <: Sys[S]](val obj: Proc[S], node0: Node, key: String, nodeRef: NodeRef.Full,
                                      val timeRef: TimeRef)
  extends NodeDependencyBuilder[S] {

  private var setMap          = Vector.empty[ControlSet]

  private var keyedUsers      = List.empty[DynamicUser]
  private var keyedResources  = List.empty[Resource   ]

  def addControl(pair: ControlSet): Unit = setMap :+= pair

  def addUser    (user    : DynamicUser): Unit = keyedUsers     ::= user
  def addResource(resource: Resource   ): Unit = keyedResources ::= resource


  def addUser   (user: DynamicUser)(implicit tx: Txn): Unit = ???
  def removeUser(user: DynamicUser)(implicit tx: Txn): Unit = ???

  def addResource(resource: Resource)(implicit tx: Txn): Unit = ???
  def addControl (pair: ControlSet  )(implicit tx: Txn): Unit = ???

  def dispose()(implicit tx: Txn): Unit = ???

  def node(implicit tx: Txn): Node = node0

  def server: Server = node0.server

  def finish()(implicit tx: Txn): Unit = {
    if (setMap.nonEmpty) node.set(setMap: _*)
    if (keyedUsers.nonEmpty || keyedResources.nonEmpty) {
      ??? // nodeRef.addAttrResources(key, keyedUsers ::: keyedResources)
      keyedUsers.foreach(_.add())
    }
  }
}