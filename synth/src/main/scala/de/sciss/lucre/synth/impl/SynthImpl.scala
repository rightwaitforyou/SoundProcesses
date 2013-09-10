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

package de.sciss.lucre.synth
package impl

import scala.collection.immutable.{Seq => ISeq}
import de.sciss.synth.{Synth => SSynth, AddAction, ControlSetMap}

final case class SynthImpl(peer: SSynth, definition: SynthDef) extends NodeImpl with Synth {
  override def toString = s"Synth(id=${peer.id}, def=${definition.name})"

  def server: Server = definition.server

  def play(target: Node, args: ISeq[ControlSetMap], addAction: AddAction, dependencies: List[Resource])
          (implicit tx: Txn): Unit = {

    val s = server
    requireOffline()
    require(target.server == s && target.isOnline, s"Target $target must be running and using the same server")
    if (dependencies.nonEmpty) {
      dependencies.foreach(r => require(r.server == s && r.isOnline,
        s"Dependency $r must be running and using the same server"))
    }
    tx.addMessage(this, peer.newMsg(definition.name, target.peer, args, addAction),
      audible = true,
      dependencies = target :: definition :: dependencies)

    setOnline(value = true)
  }
}