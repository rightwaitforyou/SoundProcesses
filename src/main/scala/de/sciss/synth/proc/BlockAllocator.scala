package de.sciss.synth
package proc

import concurrent.stm.InTxn
import impl.{ContiguousBlockAllocator => Impl}

object BlockAllocator {
   /**
    * Creates a new block allocator with the given address range.
    *
    * @param start   the start address (inclusive)
    * @param stop    the stop address (exclusive)
    */
   def apply( stop: Int, start: Int = 0 ) : BlockAllocator = Impl( start = start, stop = stop )
}
trait BlockAllocator {
   /**
    * Allocates a new block with a given size.
    *
    * @param size the size (or number of channels) to allocate.
    * @return     the allocated address, or `-1` if no free blocks are available
    */
   def alloc( size: Int = 1 )( implicit tx: InTxn ) : Int

   /**
    * Frees a block with given address and size.
    *
    * @param address the address of the allocated block
    * @param size    the size (or number of channels) of the allocated block
    */
   def free( address: Int, size: Int )( implicit tx: InTxn ) : Unit

   def consistencyCheck()( implicit tx: InTxn ) : Unit
}