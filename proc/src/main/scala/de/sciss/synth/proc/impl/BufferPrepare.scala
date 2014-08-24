/*
 *  BufferPrepare.scala
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

package de.sciss.synth.proc
package impl

import java.io.File

import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.{Buffer, Txn}
import de.sciss.osc
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.{Processor, ProcessorFactory}
import de.sciss.synth.io.AudioFileSpec

import scala.concurrent.{blocking, Await, duration, TimeoutException}
import duration.Duration
import scala.concurrent.stm.{TxnExecutor, Ref}
import TxnExecutor.{defaultAtomic => atomic}

object BufferPrepare extends ProcessorFactory {
  case class Config(f: File, spec: AudioFileSpec, offset: Long, buf: Buffer.Modifiable)

  protected def prepare(config: Config): Prepared = {
    import config._
    val numFrL = spec.numFrames
    if (numFrL > 0x3FFFFFFF) sys.error(s"File $f is too large ($numFrL frames) for an in-memory buffer")
    new Impl(path = f.getAbsolutePath, numFrames = numFrL.toInt, off0 = offset,
      numChannels = spec.numChannels, buf = buf)
  }

  type Repr     = BufferPrepare
  type Product  = Buffer.Modifiable

  private final class Impl(path: String, numFrames: Int, off0: Long, numChannels: Int, buf: Product)
    extends BufferPrepare with ProcessorImpl[Product, BufferPrepare] {

    private val blockSize = 262144 / numChannels  // XXX TODO - could be configurable
    private val offsetRef = Ref(0)

    override def toString = s"BufferPrepare($path, $buf)@${hashCode().toHexString}"

    protected def body(): Buffer.Modifiable = {
      while (progress < 1.0) {
        val pr = atomic { implicit tx =>
          val offset = offsetRef()
          val chunk = math.min(numFrames - offset, blockSize)
          val stop = offset + chunk
          if (chunk > 0) {
            offsetRef() = stop
            implicit val ptx = Txn.wrap(tx)
            // buf might be offline if dispose was called
            if (buf.isOnline) {
              buf.read(path, fileStartFrame = offset + off0, numFrames = chunk, bufStartFrame = offset)
              stop.toDouble / numFrames
            } else -1.0
          } else -1.0
        }

        if (pr < 0) abort()
        checkAborted()

        val fut = buf.server.!!(osc.Bundle.now()) // aka 'sync', so we let other stuff be processed first
        while (!fut.isCompleted) {
          blocking {
            try {
              // we wait max. 1 second, so we can cross-check on the aborted status
              Await.result(fut, Duration(1L, duration.SECONDS))
            } catch {
              case _: TimeoutException =>
            }
          }
          checkAborted()
        }
        progress = pr
      }
      buf
    }

    def dispose()(implicit tx: Txn): Unit = {
      tx.afterCommit(abort())
      buf.dispose()
    }
  }
}
trait BufferPrepare extends Processor[Buffer.Modifiable, BufferPrepare] with Disposable[Txn]
