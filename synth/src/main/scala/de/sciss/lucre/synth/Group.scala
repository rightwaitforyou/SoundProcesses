/*
 *  Group.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package synth

import de.sciss.synth.{addToHead, AddAction, Group => SGroup}
import impl.{GroupImpl => Impl}

object Group {
  def apply(target: Node /* = server.defaultGroup */, addAction: AddAction = addToHead)(implicit tx: Txn): Group = {
    val server  = target.server
    val nodeID  = server.nextNodeID()
    val g       = new Impl(server, SGroup(server.peer, nodeID))(online0 = false)
    g.play(target, addAction)
    g
  }

  private[synth] def wrap(server: Server, peer: SGroup): Group = {
    require(server.peer == peer.server)
    new Impl(server, peer)(online0 = true)
  }
}

trait Group extends Node {
  def peer: SGroup
  def freeAll()(implicit tx: Txn): Unit
}