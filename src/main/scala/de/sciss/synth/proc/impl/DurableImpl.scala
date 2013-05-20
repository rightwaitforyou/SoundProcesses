/*
 *  DurableImpl.scala
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

import concurrent.stm.InTxn
import de.sciss.lucre.{event => evt, stm}
import stm.{DataStoreFactory, DataStore}

private[proc] object DurableImpl {
  def apply(factory: DataStoreFactory[DataStore], mainName: String, eventName: String): Durable = {
    val mainStore   = factory.open(mainName)
    val eventStore  = factory.open(eventName)
    new System(mainStore, eventStore)
  }

  def apply(mainStore: DataStore, eventStore: DataStore): Durable = new System(mainStore, eventStore)

  private final class TxnImpl(val system: System, val peer: InTxn)
    extends stm.impl.DurableImpl.TxnMixin[Durable] with evt.impl.DurableImpl.DurableTxnMixin[Durable]
    with ProcTxnFullImpl[Durable] with Durable.Txn {

    lazy val inMemory: /* evt. */ InMemory#Tx = system.inMemory.wrap(peer)

    override def toString = "proc.Durable#Tx@" + hashCode.toHexString
  }

  private final class System(protected val store: DataStore, protected val eventStore: DataStore)
    extends evt.impl.DurableImpl.DurableMixin[Durable, evt.InMemory] with Durable
    with evt.impl.ReactionMapImpl.Mixin[Durable] {
    private type S = Durable
    val inMemory: /* evt. */ InMemory = /* evt. */ InMemory()

    def wrap(peer: InTxn): S#Tx = new TxnImpl(this, peer)

    override def toString = "proc.Durable@" + hashCode.toHexString
  }
}