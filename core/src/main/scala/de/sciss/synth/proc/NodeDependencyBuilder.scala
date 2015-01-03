/*
 *  NodeDependencyBuilder.scala
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

import de.sciss.lucre.synth.{Node, DynamicUser, Resource, Sys}
import de.sciss.synth.ControlSet

trait NodeDependencyBuilder[S <: Sys[S]] {
  // def server: Server
  def obj: Obj[S]

  def node: Node

  def addControl (pair    : ControlSet ): Unit
  def addUser    (user    : DynamicUser): Unit
  def addResource(resource: Resource   ): Unit
}
