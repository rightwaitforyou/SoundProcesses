/*
 *  ConfluentImpl.scala
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

import de.sciss.lucre.{stm, confluent, event => evt}
import confluent.reactive.impl.ConfluentReactiveImpl
import stm.{DataStore, DataStoreFactory}
import concurrent.stm.InTxn

/*
 * XXX TODO: don't repeat yourself. Should factor out more stuff in ConfluentReactiveImpl
 */
private[proc] object ConfluentImpl {
   private type S = Confluent

   def apply( storeFactory: DataStoreFactory[ DataStore ]) : S = {
      val durable = stm.Durable( storeFactory )
      new System( storeFactory, durable )
   }

   private sealed trait TxnImpl extends Confluent.Txn with ConfluentReactiveImpl.TxnMixin[ S ] with ProcTxnFullImpl[ S ] {
      final lazy val inMemory: stm.InMemory#Tx = system.inMemory.wrap( peer )
   }

   private final class RegularTxn( val system: S, val durable: stm.Durable#Tx,
                                   val inputAccess: S#Acc, val cursorCache: confluent.Cache[ S#Tx ])
   extends confluent.impl.ConfluentImpl.RegularTxnMixin[ S, stm.Durable ] with TxnImpl {
      lazy val peer = durable.peer
   }

   private final class RootTxn( val system: S, val peer: InTxn )
   extends confluent.impl.ConfluentImpl.RootTxnMixin[ S, stm.Durable ] with TxnImpl {
      lazy val durable: stm.Durable#Tx = {
         log( "txn durable" )
         system.durable.wrap( peer )
      }
   }

   private final class System( protected val storeFactory: DataStoreFactory[ DataStore ], val durable: stm.Durable )
   extends confluent.impl.ConfluentImpl.Mixin[ S ]
   with evt.impl.ReactionMapImpl.Mixin[ S ]
   with Confluent {
      def inMemory               = durable.inMemory
      def durableTx(  tx: S#Tx ) = tx.durable
      def inMemoryTx( tx: S#Tx ) = tx.inMemory

      private val eventStore  = storeFactory.open( "event", overwrite = true )
      private val eventVarMap = confluent.DurablePersistentMap.newConfluentIntMap[ S ]( eventStore, this, isOblivious = true )
      val eventCache : confluent.CacheMap.Durable[ S, Int, confluent.DurablePersistentMap[ S, Int ]] =
         confluent.impl.DurableCacheMapImpl.newIntCache( eventVarMap )

      protected def wrapRegular( dtx: stm.Durable#Tx, inputAccess: S#Acc, cursorCache: confluent.Cache[ S#Tx ]) =
         new RegularTxn( this, dtx, inputAccess, cursorCache )

      protected def wrapRoot( peer: InTxn ) = new RootTxn( this, peer )
   }
}