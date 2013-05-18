/*
 *  Group.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.synth.{addToHead, AddAction, Group => SGroup}
import impl.{GroupImpl => Impl}

object Group {
  def apply(target: Node /* = server.defaultGroup */, addAction: AddAction = addToHead)(implicit tx: Txn): Group = {
    val server  = target.server
    val nodeID  = server.nextNodeID()
    val g       = new Impl(server, SGroup(server.peer, nodeID))
    g.play(target, addAction)
    g
  }

    private[proc] def wrap(server: Server, peer: SGroup): Group = {
      require(server.peer == peer.server)
      new Impl(server, peer)
    }
}

trait Group extends Node {
  def peer: SGroup
  def freeAll(audible: Boolean = true)(implicit tx: Txn): Unit
}