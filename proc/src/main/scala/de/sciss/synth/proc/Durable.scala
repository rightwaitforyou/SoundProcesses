/*
 *  Durable.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.{stm, event => evt}
import impl.{DurableImpl => Impl}
import stm.{DataStoreFactory, DataStore}
import language.implicitConversions
import de.sciss.lucre.synth.{InMemory, Sys}

object Durable {
  private type S = Durable

  def apply(factory: DataStoreFactory[DataStore], mainName: String = "data", eventName: String = "event"): S =
    Impl(factory, mainName, eventName)

  def apply(mainStore: DataStore, eventStore: DataStore): S =
    Impl(mainStore, eventStore)

  implicit def inMemory(tx: Durable#Tx): InMemory#Tx = tx.inMemory

  trait Txn extends Sys.Txn[Durable] with evt.DurableLike.Txn[Durable] {
    def inMemory: InMemory#Tx
  }
}

trait Durable extends evt.DurableLike[Durable] with Sys[Durable] {
  final type Tx = Durable.Txn // Sys.Txn[Durable] with evt.DurableLike.Txn[Durable]
  final type I  = InMemory
}