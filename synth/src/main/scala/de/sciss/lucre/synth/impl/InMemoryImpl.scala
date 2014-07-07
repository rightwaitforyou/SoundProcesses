/*
 *  InMemoryImpl.scala
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

package de.sciss.lucre.synth
package impl

import concurrent.stm.InTxn
import de.sciss.lucre.{stm, event => evt}
import evt.impl.ReactionMapImpl

object InMemoryImpl {
  def apply(): InMemory = new System

  private final class TxnImpl(val system: InMemory, val peer: InTxn)
    extends stm.impl.InMemoryImpl.TxnMixin[InMemory] with evt.impl.InMemoryImpl.TxnMixin[InMemory]
    with TxnFullImpl[InMemory] {

    override def toString = s"proc.InMemory#Tx@${hashCode.toHexString}"

    def inMemory: InMemory#Tx = this
  }

  private final class System
    extends stm.impl.InMemoryImpl.Mixin[InMemory]
    with InMemory with ReactionMapImpl.Mixin[InMemory] {

    private type S = InMemory

    def inMemory = this
    def inMemoryTx(tx: Tx): Tx = tx

    def wrap(peer: InTxn): S#Tx = new TxnImpl(this, peer)

    override def toString = s"proc.InMemory@${hashCode.toHexString}"
  }
}