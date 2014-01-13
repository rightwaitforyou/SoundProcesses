/*
 *  BlockAllocator.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.lucre.synth

import impl.{BlockAllocatorImpl => Impl}
import scala.concurrent.stm.InTxn

object BlockAllocator {
  /**
   * Creates a new block allocator with the given address range.
   *
   * @param start   the start address (inclusive)
   * @param stop    the stop address (exclusive)
   */
  def apply(name: String, stop: Int, start: Int = 0): BlockAllocator = Impl(name, start = start, stop = stop)
}

trait BlockAllocator {
  /** Allocates a new block with a given size.
    *
    * @param size the size (or number of channels) to allocate.
    * @return     the allocated address, or `-1` if no free blocks are available
    */
  def alloc(size: Int = 1)(implicit tx: InTxn): Int

  /** Frees a block with given address and size.
    *
    * @param address the address of the allocated block
    * @param size    the size (or number of channels) of the allocated block
    */
  def free(address: Int, size: Int)(implicit tx: InTxn): Unit

  def consistencyCheck()(implicit tx: InTxn): Unit
}