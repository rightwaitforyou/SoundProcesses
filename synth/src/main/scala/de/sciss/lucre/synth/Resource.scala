/*
 *  Resource.scala
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

package de.sciss.lucre
package synth

import de.sciss.lucre.stm.Disposable

object Resource {
  type TimeStamp = Int

  trait Source {
    def resource(implicit tx: Txn): Resource
  }
}

trait Resource extends Disposable[Txn] {
  import Resource.TimeStamp

  def isOnline(implicit tx: Txn): Boolean

  def server: Server

  private[synth] def timeStamp                    (implicit tx: Txn): TimeStamp
  private[synth] def timeStamp_=(value: TimeStamp)(implicit tx: Txn): Unit

  //   private[proc] def addDependent(    dependent: Resource )( implicit tx: Txn ) : Unit
  //   private[proc] def removeDependent( dependent: Resource )( implicit tx: Txn ) : Unit
}