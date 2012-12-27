package de.sciss.synth.proc
package impl

import concurrent.stm.InTxn
import de.sciss.lucre.{stm, event => evt}
import evt.impl.ReactionMapImpl

object InMemoryImpl {
   def apply() : InMemory = new System

   private final class TxnImpl( val system: InMemory, val peer: InTxn )
   extends stm.impl.InMemoryImpl.TxnMixin[ InMemory ] with evt.impl.InMemoryImpl.TxnMixin[ InMemory ]
   with ProcTxnImpl[ InMemory ] {
      override def toString = "proc.InMemory#Tx@" + hashCode.toHexString

      def inMemory : InMemory#Tx = this
   }

   private final class System extends stm.impl.InMemoryImpl.Mixin[ InMemory ]
   with InMemory with ReactionMapImpl.Mixin[ InMemory ] {
      private type S = InMemory
      def wrap( peer: InTxn ) : S#Tx = new TxnImpl( this, peer )
      override def toString = "proc.InMemory@" + hashCode.toHexString

      def resources( server: Server ): ResourceManagement = ???   // XXX TODO: put this in common mixin
   }
}