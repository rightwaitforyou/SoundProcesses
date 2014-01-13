/*
 *  ConfluentImpl.scala
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
package impl

import de.sciss.lucre.{stm, confluent, event => evt}
import confluent.reactive.impl.ConfluentReactiveImpl
import stm.{DataStore, DataStoreFactory}
import concurrent.stm.InTxn
import de.sciss.lucre.synth.impl.TxnFullImpl
import de.sciss.lucre.synth.InMemory

/*
 * XXX TODO: don't repeat yourself. Should factor out more stuff in ConfluentReactiveImpl
 */
private[proc] object ConfluentImpl {
  private type S = Confluent

  def apply(storeFactory: DataStoreFactory[DataStore]): S = {
    // We can share the event store between confluent and durable, because there are
    // no key collisions. Durable uses 32-bit ints exclusively, and confluent maintains
    // a DurablePersistentMap, which uses 32 + 64 bit keys.

    // val durable = evt.Durable(storeFactory, eventName = "d-evt")
    val mainStoreD  = storeFactory.open("d-main")
    val eventStore  = storeFactory.open("event", overwrite = true)  // shared between durable + confluent

    val durable     = /* evt. */ Durable(mainStore = mainStoreD, eventStore = eventStore)
    new System(storeFactory, eventStore, durable)
  }

  private sealed trait TxnImpl extends Confluent.Txn with ConfluentReactiveImpl.TxnMixin[S] with TxnFullImpl[S] {
    final lazy val inMemory: /* evt. */ InMemory#Tx = system.inMemory.wrap(peer)
  }

  private final class RegularTxn(val system: S, val durable: /* evt. */ Durable#Tx,
                                 val inputAccess: S#Acc, val isRetroactive: Boolean,
                                 val cursorCache: confluent.Cache[S#Tx])
    extends confluent.impl.ConfluentImpl.RegularTxnMixin[S, evt.Durable] with TxnImpl {

    lazy val peer = durable.peer
  }

  private final class RootTxn(val system: S, val peer: InTxn)
    extends confluent.impl.ConfluentImpl.RootTxnMixin[S, stm.Durable] with TxnImpl {

    lazy val durable: /* evt. */ Durable#Tx = {
      log("txn durable")
      system.durable.wrap(peer)
    }
  }

  private final class System(protected val storeFactory: DataStoreFactory[DataStore],
                             protected val eventStore: DataStore, val durable: /* evt. */ Durable)
    extends confluent.reactive.impl.ConfluentReactiveImpl.Mixin[S]
    with Confluent {

    def inMemory              = durable.inMemory
    def durableTx (tx: S#Tx)  = tx.durable
    def inMemoryTx(tx: S#Tx)  = tx.inMemory

    protected def wrapRegular(dtx: /* evt. */ Durable#Tx, inputAccess: S#Acc, retroactive: Boolean,
                              cursorCache: confluent.Cache[S#Tx]) =
      new RegularTxn(this, dtx, inputAccess, retroactive, cursorCache)

    protected def wrapRoot(peer: InTxn) = new RootTxn(this, peer)
  }
}