//package de.sciss.synth.proc
//package impl
//
//import de.sciss.lucre.{confluent, stm, event => evt}
//import confluent.reactive.impl.ConfluentReactiveImpl
//import stm.{DataStoreFactory, DataStore}
//import concurrent.stm.InTxn
//import confluent.{CacheMap, DurablePersistentMap, Cache}
//import confluent.impl.{ConfluentImpl, DurableCacheMapImpl}
//import de.sciss.osc.Message
//import de.sciss.synth.osc.Send
//
//object SysImpl {
//   private type S = Sys
//
//   def apply( storeFactory: DataStoreFactory[ DataStore ]) : S = {
//      // tricky: before `durable` was a `val` in `System`, this caused
//      // a NPE with `Mixin` initialising `global`.
//      // (http://stackoverflow.com/questions/12647326/avoiding-npe-in-trait-initialization-without-using-lazy-vals)
//      val durable = stm.Durable( storeFactory )
//      new System( storeFactory, durable )
//   }
//
//   trait TxnMixin[ S <: SysLike[ S ]] extends ConfluentReactiveImpl.TxnMixin[ S ] with SysLike.Txn[ S ] {
//      _: S#Tx =>
//
//      final def addMessage( server: RichServer, msg: Message with Send, change: State.Change, audible: Boolean,
//                            dependencies: Map[ State, Boolean ], noErrors: Boolean ) {
//         sys.error("TODO")
//      }
//   }
//
//   private sealed trait TxnImpl extends TxnMixin[ Sys ] with Sys.Txn {
//      final lazy val inMemory: stm.InMemory#Tx = system.inMemory.wrap( peer )
//   }
//
//   private final class RegularTxn( val system: Sys, val durable: stm.Durable#Tx,
//                                   val inputAccess: Sys#Acc, val cursorCache: Cache[ Sys#Tx ])
//   extends ConfluentImpl.RegularTxnMixin[ Sys, stm.Durable ] with TxnImpl {
//      lazy val peer = durable.peer
//   }
//
//   private final class RootTxn( val system: Sys, val peer: InTxn )
//   extends ConfluentImpl.RootTxnMixin[ Sys, stm.Durable ] with TxnImpl {
//      lazy val durable: stm.Durable#Tx = {
//         log( "txn durable" )
//         system.durable.wrap( peer )
//      }
//   }
//
//   private final class System( protected val storeFactory: DataStoreFactory[ DataStore ], val durable: stm.Durable )
//   extends ConfluentImpl.Mixin[ S ]
//   with evt.impl.ReactionMapImpl.Mixin[ S ]
//   with Sys {
//      def inMemory               = durable.inMemory
//      def durableTx(  tx: S#Tx ) = tx.durable
//      def inMemoryTx( tx: S#Tx ) = tx.inMemory
//
//      private val eventStore  = storeFactory.open( "event", overwrite = true )
//      private val eventVarMap = DurablePersistentMap.newConfluentIntMap[ S ]( eventStore, this, isOblivious = true )
//      val eventCache : CacheMap.Durable[ S, Int, DurablePersistentMap[ S, Int ]] = DurableCacheMapImpl.newIntCache( eventVarMap )
//
//      protected def wrapRegular( dtx: stm.Durable#Tx, inputAccess: S#Acc, cursorCache: Cache[ S#Tx ]) = ???
////         new RegularTxn( this, dtx, inputAccess, cursorCache )
//
//      protected def wrapRoot( peer: InTxn ) = ??? // new RootTxn( this, peer )
//   }
//}