/*
 *  InMemory.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package synth

import de.sciss.lucre.stm.InMemoryLike
import impl.{InMemoryImpl => Impl}

object InMemory {
  def apply(): InMemory = Impl()

  trait Txn extends Sys.Txn[InMemory] with InMemoryLike.Txn[InMemory]
}

trait InMemory extends InMemoryLike[InMemory] with Sys[InMemory] {
  type Tx = InMemory.Txn
  type I  = InMemory
}