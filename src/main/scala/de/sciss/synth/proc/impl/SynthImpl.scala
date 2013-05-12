/*
 *  SynthImpl.scala
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
package impl

import de.sciss.synth.{ControlSetMap, AddAction, Synth => SSynth}

private[proc] final case class SynthImpl(peer: SSynth, definition: SynthDef) extends NodeImpl with Synth {
  override def toString = s"Synth(id${peer.id}, def=${definition.name})"

  def server: Server = definition.server

  def play(target: Node, args: Seq[ControlSetMap], addAction: AddAction, dependencies: List[Resource])
          (implicit tx: Txn) {

    val s = server
    require(target.server == s && target.isOnline)
    if (dependencies.nonEmpty) {
      dependencies.foreach(r => require(r.server == s && r.isOnline))
    }
    tx.addMessage(this, peer.newMsg(definition.name, target.peer, args, addAction),
      audible = true,
      dependencies = target :: definition :: dependencies)

    //      peer.register()   // ok to call multiple times
  }
}