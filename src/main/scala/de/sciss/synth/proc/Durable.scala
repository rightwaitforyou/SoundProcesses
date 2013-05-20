/*
 *  Durable.scala
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

import de.sciss.lucre.{stm, event => evt}
import impl.{DurableImpl => Impl}
import stm.{DataStoreFactory, DataStore}
import language.implicitConversions

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