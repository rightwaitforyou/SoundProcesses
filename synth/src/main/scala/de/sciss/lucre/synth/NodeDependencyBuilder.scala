package de.sciss.lucre.synth

import de.sciss.synth.ControlSet

trait NodeDependencyBuilder {
  // def server: Server

  def setControl (pair    : ControlSet ): Unit
  def addUser    (user    : DynamicUser): Unit
  def addResource(resource: Resource   ): Unit
}
