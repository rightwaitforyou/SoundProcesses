/*
 *  SoundProcesses.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import java.util.concurrent.{Executors, ScheduledExecutorService}

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys

import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.stm.Txn

object SoundProcesses {
  var poolSize: Option[Int] = None

  private[proc] def isPowerOfTwo(value: Int) = (value & (value - 1)) == 0

  private[proc] def validateCueBufferSize(value: Int): Unit =
    require(isPowerOfTwo(value) && value >= 8192 && value <= 131072,
      "Must be a power of two and in (8192, 131072) : " + value)

  private var cueBufSz = 32768                // XXX TODO: unused?
  def cueBufferSize: Int = cueBufSz           // XXX TODO: unused?
  def cueBufferSize_=(value: Int): Unit = {   // XXX TODO: unused?
    validateCueBufferSize(value)
    cueBufSz = value
  }

  lazy val scheduledExecutorService: ScheduledExecutorService = {
    // system wide scheduler
    val res = poolSize match {
      case Some(sz) => Executors.newScheduledThreadPool(sz)
      case _        => Executors.newSingleThreadScheduledExecutor()
    }
    sys.addShutdownHook(shutdownScheduler())
    res
  }

  lazy implicit val executionContext: ExecutionContext =
    ExecutionContext.fromExecutorService(scheduledExecutorService)

  def atomic[S <: Sys[S], A](fun: S#Tx => A)(implicit cursor: stm.Cursor[S]): Future[A] = {
    if (Txn.findCurrent.isDefined) throw new IllegalStateException(s"Cannot nest transactions")
    Future {
      cursor.step(fun)
    } (executionContext)
  }

  private def shutdownScheduler(): Unit = {
    log("Shutting down scheduler thread pool")
    scheduledExecutorService.shutdown()
  }

  private[this] lazy val _init: Unit = {
    de.sciss.lucre.expr.init()
    Action  .init()
    Code    .init()
    CurveObj.init()
    Ensemble.init()
    FadeSpec.init()
    // Folder  .init()
    Grapheme.init()
    Proc    .init()
    Scan    .init()
    Timeline.init()
  }

  /** Registers all known types. */
  def init(): Unit = _init
}