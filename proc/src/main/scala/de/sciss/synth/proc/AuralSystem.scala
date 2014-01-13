/*
 *  AuralSystem.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
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

import impl.{AuralSystemImpl => Impl}
import de.sciss.lucre.synth.{Server, Txn}

object AuralSystem {
  def apply(): AuralSystem = Impl()

  def start(config: Server.Config = Server.Config(), connect: Boolean = false): AuralSystem =
    apply().start(config, connect = connect)

  def offline(server: Server.Offline): AuralSystem = {
    val res = apply()
    res.offline(server)
    res
  }

  trait Client {
    def started(s: Server): Unit
    def stopped()         : Unit
  }
}
/** An `AuralSystem` is the logical representation of a sound synthesis server, whether running or not.
  * To use an aural system, a client connects via `addClient`. The client will be notified when the
   * server is up and running.
  */
trait AuralSystem {
  import AuralSystem.Client

  /** Boots the server. This method must not be called from within a transaction. */
  def start  (config: Server.Config = Server.Config(), connect: Boolean = false): AuralSystem
  private[proc] def offline(server: Server.Offline): Unit

  /** Quits the server. This method must not be called from within a transaction. */
  def stop(): AuralSystem

  /** Adds a client to the system. It is safe to call this method both inside and
    * outside of a transaction. If called inside a transaction, this is transaction
    * safe (no duplicate registration if the transaction is retried).
    *
    * @param  c the client to register. If the server is already running, the client
    *           will _not_ be immediately notified.
    */
  def addClient   (c: Client): Unit

  /** Removes a client to the system. It is safe to call this method both inside and
    * outside of a transaction. If called inside a transaction, this is transaction
    * safe.
    *
    * @param  c the client to unregister. It is allowed to call this method even if
    *           the client was already unregistered.
    */
  def removeClient(c: Client): Unit

  /** Registeres a callback to be invoked when the server has been booted.
    * If the server is already running, this has no effect. This method is transaction safe.
    */
  def whenStarted(fun: Server => Unit): Unit

  def serverOption(implicit tx: Txn): Option[Server]
}
