/*
 *  SoundProcesses.scala
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

import java.util.concurrent.{Executors, ScheduledExecutorService}

object SoundProcesses {
  var poolSize: Option[Int] = None

  private[proc] def isPowerOfTwo(value: Int) = (value & (value - 1)) == 0

  private[proc] def validateCueBufferSize(value: Int): Unit =
    require(isPowerOfTwo(value) && value >= 8192 && value <= 131072,
      "Must be a power of two and in (8192, 131072) : " + value)

  private var cueBufSz = 32768
  def cueBufferSize: Int = cueBufSz
  def cueBufferSize_=(value: Int) {
    validateCueBufferSize(value)
    cueBufSz = value
  }

  lazy val pool: ScheduledExecutorService = {
    // system wide scheduler
    val res = poolSize match {
      case Some(sz) => Executors.newScheduledThreadPool(sz)
      case _        => Executors.newSingleThreadScheduledExecutor()
    }
    sys.addShutdownHook(shutdownScheduler())
    res
  }

  private def shutdownScheduler(): Unit = {
    log("Shutting down scheduler thread pool")
    pool.shutdown()
  }
}