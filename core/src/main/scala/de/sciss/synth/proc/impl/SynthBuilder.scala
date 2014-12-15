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

import de.sciss.lucre.synth.{AudioBus, Resource, DynamicUser, Synth, Sys}
import de.sciss.synth.ControlSet
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

  // ---- node dependency builder ----

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