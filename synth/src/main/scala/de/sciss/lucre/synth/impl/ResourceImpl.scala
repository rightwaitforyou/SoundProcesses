/*
 *  ResourceImpl.scala
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

package de.sciss.lucre.synth
package impl

import scala.concurrent.stm.Ref

trait ResourceImpl extends Resource {
  import Resource.TimeStamp

  private val timeStampRef = Ref(0)

  protected def online0: Boolean = false

  private val stateOnline = Ref(initialValue = online0)

  final def isOnline(implicit tx: Txn): Boolean = stateOnline.get(tx.peer)
  final protected def setOnline(value: Boolean)(implicit tx: Txn): Unit = stateOnline.set(value)(tx.peer)

  final def timeStamp(implicit tx: Txn): TimeStamp = timeStampRef.get(tx.peer)

  final def timeStamp_=(value: TimeStamp)(implicit tx: Txn): Unit = timeStampRef.set(value)(tx.peer)

  final protected def requireOnline ()(implicit tx: Txn): Unit = require( isOnline, "must be online")
  final protected def requireOffline()(implicit tx: Txn): Unit = require(!isOnline, "must be offline")

  // final protected def require(p: Boolean): Unit = require(p, "")

  final protected def require(p: Boolean, message: => String): Unit = {
    if (!p) {
      val user  = message
      val msg   = if (user.isEmpty) s"$this is not in required state" else s"$this - $user"
      log(msg)
      throw new IllegalStateException(msg)
    }
  }

  //   final def addDependent( dependent: Resource )( implicit tx: Txn ): Unit = {
  //      require( dependent.server == server, "Dependency uses divergent server : " + dependent )
  //      dependentsRef.add( dependent )( tx.peer )
  //   }
  //
  //   final def removeDependent( dependent: Resource )( implicit tx: Txn ): Unit = {
  //      dependentsRef.remove( dependent )( tx.peer )
  //   }
}