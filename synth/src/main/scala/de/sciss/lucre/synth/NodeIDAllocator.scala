/*
 *  NodeIDAllocator.scala
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

import impl.{NodeIDAllocatorImpl => Impl}
import scala.concurrent.stm.InTxn

object NodeIDAllocator {
  def apply(user: Int, initTemp: Int): NodeIDAllocator = new Impl(user = user, initTemp = initTemp)
}
trait NodeIDAllocator{
  def alloc()(implicit tx: InTxn): Int
}