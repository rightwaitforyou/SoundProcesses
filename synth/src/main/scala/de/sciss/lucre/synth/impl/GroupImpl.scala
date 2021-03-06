/*
 *  GroupImpl.scala
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

package de.sciss.lucre.synth
package impl

import de.sciss.synth.{Group => SGroup, AddAction}

final case class GroupImpl(server: Server, peer: SGroup)(override protected val online0: Boolean)
  extends NodeImpl with Group {

  override def toString = s"Group($peer)"

  def play(target: Node, addAction: AddAction)(implicit tx: Txn): Unit = {
    requireOffline()
    require(target.isOnline        , s"Target $target must be running")
    require(target.server == server, s"Target $target must be using the same server")

    tx.addMessage(this, peer.newMsg(target.peer, addAction), dependencies = target :: Nil)
    setOnline(value = true)
  }

  def freeAll()(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.freeAllMsg)
  }
}