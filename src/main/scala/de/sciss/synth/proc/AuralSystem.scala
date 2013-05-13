/*
 *  AuralSystem.scala
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

import impl.{AuralSystemImpl => Impl}
import de.sciss.lucre.stm

object AuralSystem {
  def apply(): AuralSystem = Impl()

  def start(config: Server.Config = Server.Config(), connect: Boolean = false, schoko: Int): AuralSystem =
    apply().start(config, connect = connect, schoko = schoko)

  def offline(server: Server.Offline, schoko: Int): AuralSystem = {
    val res = apply()
    res.offline(server, schoko = schoko)
    res
  }

  trait Client {
    def started(s: Server): Unit
    def stopped()         : Unit
  }
}

trait AuralSystem {
  import AuralSystem.Client

  def start  (config: Server.Config = Server.Config(), connect: Boolean = false, schoko: Int): AuralSystem
  private[proc] def offline(server: Server.Offline, schoko: Int): Unit

  def stop(schoko: Int): AuralSystem

  def addClient   (c: Client): Unit
  def removeClient(c: Client): Unit

  def whenStarted(fun: Server => Unit): Unit

  def serverOption(implicit tx: Txn): Option[Server]
}
