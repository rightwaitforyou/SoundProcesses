/*
 *  DurableImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm.DataStore
import de.sciss.lucre.synth.InMemory
import de.sciss.lucre.synth.impl.TxnFullImpl
import de.sciss.lucre.stm

import scala.concurrent.stm.InTxn

private[proc] object DurableImpl {
  def apply(factory: DataStore.Factory, mainName: String, eventName: String): Durable = {
    val mainStore   = factory.open(mainName)
    val eventStore  = factory.open(eventName, overwrite = true)
    new System(mainStore, eventStore)
  }

  def apply(mainStore: DataStore, eventStore: DataStore): Durable = new System(mainStore, eventStore)

  private final class TxnImpl(val system: System, val peer: InTxn)
    extends stm.impl.DurableImpl.TxnMixin[Durable] with stm.impl.DurableImpl.TxnMixin[Durable]
    with TxnFullImpl[Durable] with Durable.Txn {

    lazy val inMemory: /* evt. */ InMemory#Tx = system.inMemory.wrap(peer)

    override def toString = s"proc.Durable#Tx@${hashCode.toHexString}"
  }

  private final class System(protected val store: DataStore, protected val eventStore: DataStore)
    extends stm.impl.DurableImpl.Mixin[Durable, /* stm. */ InMemory]
    with Durable {
    // with evt.impl.ReactionMapImpl.Mixin[Durable] {

    private type S = Durable

    val inMemory: /* evt. */ InMemory = /* evt. */ InMemory()
    def inMemoryTx(tx: Tx): I#Tx = tx.inMemory

    def wrap(peer: InTxn): S#Tx = new TxnImpl(this, peer)

    override def toString = s"proc.Durable@${hashCode.toHexString}"
  }
}