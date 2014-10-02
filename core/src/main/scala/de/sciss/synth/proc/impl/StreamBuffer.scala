/*
 *  StreamBuffer.scala
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

import de.sciss.{synth, osc}
import scala.annotation.switch
import de.sciss.lucre.synth.{Synth, Txn, Buffer}
import de.sciss.synth.GE

object StreamBuffer {
  def padSize(interp: Int): Int = (interp: @switch) match {
    case 1 => 0
    case 2 => 1
    case 4 => 4
    case _ => sys.error(s"Illegal interpolation value: $interp")
  }

  // via SendReply
  private def replyName(key: String): String = s"/$$str_$key"

  def makeUGen(key: String, idx: Int, buf: GE, numChannels: Int, speed: GE, interp: Int): GE = {
    import synth._
    import ugen._
    val diskPad     = StreamBuffer.padSize(interp)
    //    val bufRate     = speed
    //    val phasorRate  = bufRate / SampleRate.ir
    val phasorRate  = speed
    val bufRate     = speed * SampleRate.ir
    val numFrames   = BufFrames.ir(buf)
    val halfPeriod  = numFrames / (bufRate * 2)
    val phasor      = Phasor.ar(speed = phasorRate, lo = diskPad, hi = numFrames - diskPad)

    // ---- clock trigger ----

    // for the trigger, k-rate is sufficient
    val phasorK     = A2K.kr(phasor)
    val phasorTrig  = Trig1.kr(phasorK - numFrames/2, ControlDur.ir)
    val clockTrig   = phasorTrig + TDelay.kr(phasorTrig, halfPeriod)
    val position    = PulseCount.kr(clockTrig)

    // println(s"makeUGen($key, $idx, $buf, $numChannels, $speed, $interp)")
    // numFrames.poll(0, "numFrames")
    // position.poll(clockTrig, "count")
    // phasor.poll(2, "phasor")

    SendReply.kr(trig = clockTrig, values = position, msgName = replyName(key), id = idx)

    // ---- actual signal ----

    BufRd.ar(numChannels, buf = buf, index = phasor, loop = 0, interp = interp)
  }
}
final class StreamBuffer(key: String, idx: Int, synth: Synth, buf: Buffer.Modifiable, path: String, fileFrames: Long,
                         interp: Int) {

  private val bufSizeH  = buf.numFrames/2
  private val diskPad   = StreamBuffer.padSize(interp)
  private val bufSizeHM = bufSizeH - diskPad
  private val replyName = StreamBuffer.replyName(key)
  private val nodeID    = synth.peer.id

  private def updateBuffer(trigVal: Int)(implicit tx: Txn): Long = {
    val trigEven  = trigVal % 2 == 0
    val bufOff    = if (trigEven) 0 else bufSizeH
    val frame     = trigVal.toLong * bufSizeHM /* + startPos = 0 */ + (if (trigEven) 0 else diskPad)
    val readSz    = math.max(0, math.min(bufSizeH, fileFrames - frame)).toInt
    val fillSz    = bufSizeH - readSz

    if (fillSz > 0) {
      buf.fill(index = (bufOff + readSz) * buf.numChannels, num = fillSz * buf.numChannels, value = 0f)
    }

    if (readSz > 0) buf.read(
      path            = path,
      fileStartFrame  = frame,
      numFrames       = readSz,
      bufStartFrame   = bufOff
    )

    frame
  }

  def install()(implicit tx: Txn): Unit = {
    val trigResp = de.sciss.synth.message.Responder.add(synth.server.peer) {
      case m @ osc.Message(`replyName`, `nodeID`, `idx`, trigValF: Float) =>
        // println(s"RECEIVED TR $trigValF...")
        // logAural(m.toString())
        val trigVal = trigValF.toInt + 1
        scala.concurrent.stm.atomic { itx =>
          implicit val tx = Txn.wrap(itx)
          val frame = updateBuffer(trigVal)
          if (frame >= fileFrames + bufSizeH) {
            synth.free()
          }
        }
    }
    // Responder.add is non-transactional. Thus, if the transaction fails, we need to remove it.
    scala.concurrent.stm.Txn.afterRollback { _ =>
      trigResp.remove()
    } (tx.peer)

    synth.onEnd(trigResp.remove())

    // initial buffer fills. XXX TODO: fuse both reads into one
    updateBuffer(0)
    updateBuffer(1)
  }
}
