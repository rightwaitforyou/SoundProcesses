package de.sciss.synth.proc.impl

import scala.concurrent.stm.{Ref, InTxn}
import de.sciss.synth.proc.NodeIDAllocator

final class NodeIDAllocatorImpl(user: Int, initTemp: Int) extends NodeIDAllocator {
  private val temp = Ref(initTemp)
  private val mask = user << 26

  def alloc()(implicit tx: InTxn): Int = {
    // `getAndTransform`:
    // "Transforms the value referenced by this `Ref` by applying the function
    // `f`, and returns the previous value."
    val x = temp.getAndTransform { old =>
      val next = old + 1
      if (next < 0x03FFFFFF) next else initTemp
    }
    x | mask
  }
}