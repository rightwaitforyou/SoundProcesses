package de.sciss.lucre.synth

import impl.{NodeIDAllocatorImpl => Impl}
import scala.concurrent.stm.InTxn

object NodeIDAllocator {
  def apply(user: Int, initTemp: Int): NodeIDAllocator = new Impl(user = user, initTemp = initTemp)
}
trait NodeIDAllocator{
  def alloc()(implicit tx: InTxn): Int
}