package de.sciss.synth.proc

import scala.concurrent.stm.InTxn
import impl.{NodeIDAllocatorImpl => Impl}

object NodeIDAllocator {
  def apply(user: Int, initTemp: Int): NodeIDAllocator = new Impl(user = user, initTemp = initTemp)
}
trait NodeIDAllocator{
  def alloc()(implicit tx: InTxn): Int
}