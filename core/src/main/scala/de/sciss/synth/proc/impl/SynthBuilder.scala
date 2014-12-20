/*
 *  SynthBuilder.scala
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

package de.sciss.synth.proc.impl

import de.sciss.lucre.synth.{NodeRef, Node, AuralNode, Txn, AudioBus, Resource, DynamicUser, Synth, Sys}
import de.sciss.synth.{addToHead, ControlSet}
import de.sciss.synth.proc.{NodeDependencyBuilder, TimeRef, Proc}

/** An object used in the last phase of playing a process. It has
  * an instantiated synth and allows the addition of controls, buses, users
  * and dependencies.
  *
  * @see  [[AuralProcImpl]]
  */
final class SynthBuilder[S <: Sys[S]](val obj: Proc.Obj[S], val synth: Synth, val timeRef: TimeRef)
  extends NodeDependencyBuilder[S] {

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

  def finish()(implicit tx: Txn): AuralNode = {
    // XXX TODO
    val server  = synth.server
    val group   = server.defaultGroup

    val attrMap1 = attrMap.map { case (key, value) => (key, value._1 ::: value._2) }

    val node = AuralNode(synth, inputBuses = inputBuses, outputBuses = outputBuses,
      resources = users ::: dependencies, attrMap = attrMap1)

    // wrap as AuralProc and save it in the identifier map for later lookup
    synth.play(target = group, addAction = addToHead, args = setMap.result(),
      dependencies = dependencies)

    users                  .foreach(_.add())
    attrMap.foreach(_._2._1.foreach(_.add()))
    node
  }

  // copies the node-dependency-builder stuff to a map entry
  def storeKey(key: String): Unit =
    if (keyedUsers.nonEmpty || keyedResources.nonEmpty) {
      attrMap += key -> (keyedUsers -> keyedResources)
      keyedUsers      = Nil
      keyedResources  = Nil
    }

  // ---- node-dependency-builder ----

  def node = synth

  def addControl(pair: ControlSet): Unit = setMap += pair

  private var keyedUsers      = List.empty[DynamicUser]
  private var keyedResources  = List.empty[Resource   ]

  def addUser    (user    : DynamicUser): Unit = keyedUsers     ::= user
  def addResource(resource: Resource   ): Unit = keyedResources ::= resource
}

/** An object used in the preparatory phase of playing a process. It allows
  * the addition of asynchronous processors.
  */
final class AsyncProcBuilder[S <: Sys[S]](val obj: Proc.Obj[S]) {
  var resources = List.empty[AsyncResource[S]]
}

final class SynthUpdater[S <: Sys[S]](val obj: Proc.Obj[S], val node: Node, key: String, nodeRef: NodeRef.Full)
  extends NodeDependencyBuilder[S] {

  private var setMap = Vector.empty[ControlSet]

  private var keyedUsers      = List.empty[DynamicUser]
  private var keyedResources  = List.empty[Resource   ]

  def addControl(pair: ControlSet): Unit = setMap :+= pair

  def addUser    (user    : DynamicUser): Unit = keyedUsers     ::= user
  def addResource(resource: Resource   ): Unit = keyedResources ::= resource

  def finish()(implicit tx: Txn): Unit = {
    if (setMap.nonEmpty) node.set(setMap: _*)
    if (keyedUsers.nonEmpty || keyedResources.nonEmpty) {
      nodeRef.addAttrResources(key, keyedUsers ::: keyedResources)
      keyedUsers.foreach(_.add())
    }
  }
}