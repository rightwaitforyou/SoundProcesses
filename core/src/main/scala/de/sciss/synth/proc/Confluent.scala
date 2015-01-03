/*
 *  Confluent.scala
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

package de.sciss
package synth
package proc

import lucre.{stm, confluent}
import confluent.reactive.ConfluentReactiveLike
import stm.{DataStore, DataStoreFactory}
import impl.{ConfluentImpl => Impl}
import language.implicitConversions
import de.sciss.lucre.synth.{InMemory, Sys}

object Confluent {
  private type S = Confluent

  def apply(storeFactory: DataStoreFactory[DataStore]): S = Impl(storeFactory)

  trait Txn extends ConfluentReactiveLike.Txn[S] with Sys.Txn[S] {
//    private[proc] def durable : Durable#Tx
//    private[proc] def inMemory: InMemory#Tx
  }

//  implicit def inMemory(tx: S#Tx): InMemory#Tx = tx.inMemory
//  implicit def durable (tx: S#Tx): Durable #Tx = tx.durable
}

trait Confluent extends ConfluentReactiveLike[Confluent] with Sys[Confluent] {
  protected type S  = Confluent
  type D            = Durable
  type I            = InMemory
  type Tx           = Confluent.Txn // Sys.Txn[ S ] with ConfluentReactiveLike.Txn[ S ]
}