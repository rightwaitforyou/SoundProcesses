package de.sciss.synth.proc
package impl

import de.sciss.lucre.{confluent, stm, event => evt}
import stm.{DataStoreFactory, DataStore}
import concurrent.stm.InTxn
import confluent.{CacheMap, DurablePersistentMap, Cache}
import confluent.impl.DurableCacheMapImpl

object SysImpl {
   private type S = Sys

   def apply( storeFactory: DataStoreFactory[ DataStore ]) : S = {
      // tricky: before `durable` was a `val` in `System`, this caused
      // a NPE with `Mixin` initialising `global`.
      // (http://stackoverflow.com/questions/12647326/avoiding-npe-in-trait-initialization-without-using-lazy-vals)
      val durable = stm.Durable( storeFactory )
      new System( storeFactory, durable )
   }

   private final class System( protected val storeFactory: DataStoreFactory[ DataStore ], val durable: stm.Durable )
   extends confluent.impl.ConfluentImpl.Mixin[ S ]
   with evt.impl.ReactionMapImpl.Mixin[ S ]
   with Sys {
      def inMemory               = durable.inMemory
      def durableTx(  tx: S#Tx ) = tx.durable
      def inMemoryTx( tx: S#Tx ) = tx.inMemory

      private val eventStore  = storeFactory.open( "event", overwrite = true )
      private val eventVarMap = DurablePersistentMap.newConfluentIntMap[ S ]( eventStore, this, isOblivious = true )
      val eventCache : CacheMap.Durable[ S, Int, DurablePersistentMap[ S, Int ]] = DurableCacheMapImpl.newIntCache( eventVarMap )

      protected def wrapRegular( dtx: stm.Durable#Tx, inputAccess: S#Acc, cursorCache: Cache[ S#Tx ]) = ???
//         new RegularTxn( this, dtx, inputAccess, cursorCache )

      protected def wrapRoot( peer: InTxn ) = ??? // new RootTxn( this, peer )
   }
}