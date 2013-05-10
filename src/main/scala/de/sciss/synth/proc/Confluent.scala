/*
 *  Confluent.scala
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

package de.sciss
package synth
package proc

import lucre.{stm, confluent, event => evt}
import confluent.reactive.ConfluentReactiveLike
import stm.{DataStore, DataStoreFactory}
import impl.{ConfluentImpl => Impl}
import language.implicitConversions

object Confluent {
  private type S = Confluent

  def apply(storeFactory: DataStoreFactory[DataStore]): S = Impl(storeFactory)

  trait Txn extends ConfluentReactiveLike.Txn[S] with Sys.Txn[S] {
    private[proc] def durable : evt.Durable#Tx
    private[proc] def inMemory: evt.InMemory#Tx
  }

  implicit def inMemory(tx: S#Tx): evt.InMemory#Tx = tx.inMemory
}

trait Confluent extends ConfluentReactiveLike[Confluent] with Sys[Confluent] {
  protected type S  = Confluent
  type D            = evt.Durable
  type I            = evt.InMemory
  type Tx           = Confluent.Txn // Sys.Txn[ S ] with ConfluentReactiveLike.Txn[ S ]
}