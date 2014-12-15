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
