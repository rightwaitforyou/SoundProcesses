/*
 *  SensorSystem.scala
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

import de.sciss.osc
import de.sciss.lucre.stm.TxnLike
import impl.{SensorSystemImpl => Impl}
import scala.collection.immutable.{IndexedSeq => Vec}

object SensorSystem {
  final val defaultPort = 0x5350  // "SP"

  def apply(): SensorSystem = Impl()

  def start(config: Config = defaultConfig)(implicit tx: TxnLike): SensorSystem = {
    val res = apply()
    res.start(config)
    res
  }

  type Config = osc.Channel.Net.Config

  def defaultConfig: Config = {
    val builder = osc.UDP.Config()
    builder.localPort = defaultPort
    // builder.localIsLoopback = true
    builder.build
  }

  trait Client {
    def sensorsStarted(c: Server)(implicit tx: TxnLike): Unit
    def sensorsStopped()         (implicit tx: TxnLike): Unit
    def sensorsUpdate(values: Vec[Float])(implicit tx: TxnLike): Unit
  }

  type Server = osc.Receiver.Undirected.Net
}
trait SensorSystem {
  import SensorSystem.{Config, Client, Server}

  def start(config: Config = SensorSystem.defaultConfig)(implicit tx: TxnLike): Unit

  def stop()(implicit tx: TxnLike): Unit

  def addClient   (c: Client)(implicit tx: TxnLike): Unit
  def removeClient(c: Client)(implicit tx: TxnLike): Unit

  def whenStarted(fun: Server => Unit)(implicit tx: TxnLike): Unit

  def serverOption(implicit tx: TxnLike): Option[Server]
}
