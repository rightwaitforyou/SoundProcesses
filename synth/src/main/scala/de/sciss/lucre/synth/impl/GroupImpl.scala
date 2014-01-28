/*
 *  GroupImpl.scala
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

package de.sciss.lucre.synth
package impl

import de.sciss.synth.{Group => SGroup, AddAction}

final case class GroupImpl(server: Server, peer: SGroup)(override protected val online0: Boolean)
  extends NodeImpl with Group {

  override def toString = "Group(" + peer.toString + ")"

  def play(target: Node, addAction: AddAction)(implicit tx: Txn): Unit = {
    requireOffline()
    require(target.server == server && target.isOnline, s"Target $target must be running and using the same server")

    // THERE IS CURRENTLY A PROBLEM EXHIBITED BY TEST3: BASICALLY --
    // since newMsg is not audible, it might be placed in the first bundle, but then
    // since moveAfterMsg is audible, the target of this group's newMsg might be
    // moved, ending up in moveAfterMsg following the g_new message, leaving this
    // group in the wrong place of the graph.
    //
    // We thus try out a workaround by declaring a group's newMsg also audible...
    //      tx.add( group.newMsg( target.node, addAction ), Some( (RequiresChange, isOnline, true) ), false,
    //              Map( target.isOnline -> true ))
    tx.addMessage(this, peer.newMsg(target.peer, addAction), audible = true,
      dependencies = target :: Nil)
    setOnline(value = true)
  }

  def freeAll(audible: Boolean)(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.freeAllMsg, audible = audible)
  }
}