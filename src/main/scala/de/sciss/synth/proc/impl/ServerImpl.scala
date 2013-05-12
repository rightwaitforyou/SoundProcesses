/*
 *  ServerImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2013 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.synth.proc
package impl

import de.sciss.synth.{Server => SServer, message, AllocatorExhausted}
import de.sciss.osc
import scala.concurrent.{ExecutionContext, Future}
import de.sciss.osc.Packet
import collection.immutable.{IndexedSeq => IIdxSeq}

object ServerImpl {
  def apply  (peer: SServer): Server          = new OnlineImpl (peer)
  def offline(peer: SServer): Server.Offline  = new OfflineImpl(peer)

  private final case class OnlineImpl(peer: SServer) extends Impl {
    override def toString = peer.toString()

    // ---- side effects ----

    def !(p: Packet) { peer ! p }

    def !!(bndl: osc.Bundle): Future[Unit] = {
      val syncMsg = peer.syncMsg()
      val syncID  = syncMsg.id
      val bndlS   = osc.Bundle(bndl.timetag, (bndl :+ syncMsg): _*)
      peer.!!(bndlS) {
        case message.Synced(`syncID`) =>
      }
    }

    def commit(future: Future[Unit]) {}   // we don't use these
  }

  private final case class OfflineImpl(peer: SServer) extends Impl with Server.Offline {
    override def toString = s"$peer @offline"

    private val sync = new AnyRef

    var position  = 0L

    private var _bundles      = Vector.empty[osc.Bundle]
    private var _commits      = Vector.empty[Future[Unit]]

    private val sampleRate    = peer.sampleRate
    private def time: Double  = position / sampleRate

    def consume(): Future[IIdxSeq[osc.Bundle]] = sync.synchronized {
      val futs  = filteredCommits
      _commits  = Vector.empty
      implicit val exec = peer.clientConfig.executionContext
      val red   = ProcWorld.reduceFutures(futs)
      red.map(_ => sync.synchronized {
        val res   = _bundles
        _bundles  = Vector.empty
        res
      })
    }

    private def addBundle(b: osc.Bundle) {
      sync.synchronized(_bundles :+= b)
    }

    def !(p: Packet) {
      val b = osc.Bundle.secs(time)
      addBundle(b)
    }

    def !!(bndl: osc.Bundle): Future[Unit] = {
      addBundle(bndl)
      Future.successful()
    }

    // caller must have `sync`
    private def filteredCommits = _commits.filterNot(_.isCompleted)

    def commit(future: Future[Unit]) {
      sync.synchronized {
        _commits = filteredCommits :+ future
      }
    }
  }

  private abstract class Impl extends Server {

    def executionContext: ExecutionContext = peer.clientConfig.executionContext

    private val controlBusAllocator = BlockAllocator("control", peer.config.controlBusChannels)
    private val audioBusAllocator   = BlockAllocator("audio"  , peer.config.audioBusChannels, peer.config.internalBusIndex)
    private val bufferAllocator     = BlockAllocator("buffer" , peer.config.audioBuffers)
    private val nodeAllocator       = NodeIDAllocator(peer.clientConfig.clientID, peer.clientConfig.nodeIDOffset)

    val defaultGroup: Group = Group.wrap(this, peer.defaultGroup) // .default( this )

    def allocControlBus(numChannels: Int)(implicit tx: Txn): Int = {
      val res = controlBusAllocator.alloc(numChannels)(tx.peer)
      if (res < 0) throw AllocatorExhausted("Control buses exhausted for " + this)
      res
    }

    def allocAudioBus(numChannels: Int)(implicit tx: Txn): Int = {
      val res = audioBusAllocator.alloc(numChannels)(tx.peer)
      if (res < 0) throw AllocatorExhausted("Audio buses exhausted for " + this)
      res
    }

    def freeControlBus(index: Int, numChannels: Int)(implicit tx: Txn) {
      controlBusAllocator.free(index, numChannels)(tx.peer)
    }

    def freeAudioBus(index: Int, numChannels: Int)(implicit tx: Txn) {
      audioBusAllocator.free(index, numChannels)(tx.peer)
    }

    def allocBuffer(numConsecutive: Int)(implicit tx: Txn): Int = {
      val res = bufferAllocator.alloc(numConsecutive)(tx.peer)
      if (res < 0) throw AllocatorExhausted("Buffers exhausted for " + this)
      res
    }

    def freeBuffer(index: Int, numConsecutive: Int)(implicit tx: Txn) {
      bufferAllocator.free(index, numConsecutive)(tx.peer)
    }

    def nextNodeID()(implicit tx: Txn): Int = nodeAllocator.alloc()(tx.peer)
  }
}