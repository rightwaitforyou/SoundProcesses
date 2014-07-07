/*
 *  InMemory.scala
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

package de.sciss.lucre
package synth

import impl.{InMemoryImpl => Impl}

object InMemory {
  def apply(): InMemory = Impl()
}

trait InMemory extends event.InMemoryLike[InMemory] with Sys[InMemory] {
  type Tx = Sys.Txn[InMemory]
  type I  = InMemory
}