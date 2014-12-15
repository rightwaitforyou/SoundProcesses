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

import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.{AuralNode, Txn, AudioBus, Resource, DynamicUser, Synth, Sys}
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

  private var attrMap = Map.empty[String, List[Disposable[Txn]]]

  def finish()(implicit tx: Txn): AuralNode = {
    // XXX TODO
    val server  = synth.server
    val group   = server.defaultGroup

    val node = AuralNode(synth, inputBuses = inputBuses, outputBuses = outputBuses,
      resources = users ::: dependencies)

    // wrap as AuralProc and save it in the identifier map for later lookup
    synth.play(target = group, addAction = addToHead, args = setMap.result(),
      dependencies = dependencies)

    if (users.nonEmpty) users.foreach(_.add())
    node
  }

  // copies the node-dependency-builder stuff to a map entry
  def storeKey(key: String): Unit =
    if (keyedUsers.nonEmpty || keyedResources.nonEmpty) {
      val disp = keyedUsers ::: keyedResources
      attrMap += key -> disp
      keyedUsers      = Nil
      keyedResources  = Nil
    }

  // ---- node-dependency-builder ----

  def node = synth

  def addControl(pair: ControlSet): Unit = setMap += pair

  var keyedUsers      = List.empty[DynamicUser]
  var keyedResources  = List.empty[Resource   ]

  def addUser    (user    : DynamicUser): Unit = keyedUsers     ::= user
  def addResource(resource: Resource   ): Unit = keyedResources ::= resource
}

/** An object used in the preparatory phase of playing a process. It allows
  * the addition of asynchronous processors.
  */
final class AsyncProcBuilder[S <: Sys[S]](val obj: Proc.Obj[S]) {
  var resources = List.empty[AsyncResource[S]]
}