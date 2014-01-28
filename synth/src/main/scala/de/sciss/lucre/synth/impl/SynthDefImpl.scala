/*
 *  SynthDefImpl.scala
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

import de.sciss.synth.{SynthDef => SSynthDef}

final case class SynthDefImpl(server: Server, peer: SSynthDef) extends ResourceImpl with SynthDef {
  override def toString = s"SynthDef(${peer.name})"

  def name: String = peer.name

  /** Actually checks if the def is already online.
    * Only if that is not the case, the receive message will be queued.
    */
  def recv()(implicit tx: Txn): Unit = {
    requireOffline()
    tx.addMessage(this, peer.recvMsg, audible = false)
    setOnline(value = true)
  }

  def dispose()(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.freeMsg, audible = false)
    setOnline(value = false)
  }
}