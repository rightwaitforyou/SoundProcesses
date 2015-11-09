/*
 *  AuralNode.scala
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

import de.sciss.lucre.synth.{DynamicUser, Group, NodeRef, Resource, Server, Synth, Txn}
import impl.{AuralNodeImpl => Impl}

object AuralNode {
  def apply(timeRef: TimeRef, wallClock: Long, synth: Synth)(implicit tx: Txn): Builder =
    Impl(timeRef, wallClock, synth)

  trait Builder extends AuralNode {
    def play()(implicit tx: Txn): Unit
  }
}

trait AuralNode extends NodeRef.Full {
  def server: Server

  def timeRef: TimeRef

  def shiftTo(newWallClock: Long): TimeRef

  /** Retrieves the main group of the Proc, or returns None if a group has not yet been assigned. */
  def groupOption(implicit tx: Txn): Option[Group]

  /** Retrieves the main group of the Proc. If this group has not been assigned yet,
    * this method will create a new group. */
  def group()(implicit tx: Txn): Group

  def group_=(value: Group)(implicit tx: Txn): Unit

  def preGroup()(implicit tx: Txn): Group

  // def getInputBus (key: String): Option[AudioBus]
  // def getOutputBus(key: String): Option[AudioBus]
}