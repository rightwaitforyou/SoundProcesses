/*
 *  Resource.scala
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