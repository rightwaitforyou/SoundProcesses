/*
 *  ServerImpl.scala
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

package de.sciss.lucre.synth
package impl

import scala.concurrent.{ExecutionContext, Future}
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.synth.{Server => SServer, Client => SClient, ControlABusMap, ControlSet, AllocatorExhausted, addToHead, message}
import de.sciss.osc

object ServerImpl {
  def apply  (peer: SServer): Server          = new OnlineImpl (peer)
  def offline(peer: SServer): Server.Offline  = new OfflineImpl(peer)

  /** If `true`, checks against bundle size overflow (64K) and prints the bundle before crashing. */
  var DEBUG_SIZE      = true
  /** If `true`, applies a few optimizations to messages within a bundle, in order to reduce its size */
  var USE_COMPRESSION = true

  private final case class OnlineImpl(peer: SServer) extends Impl {
    override def toString = peer.toString()

    private def checkPacket(p: osc.Packet): Unit = {
      val sz = p match {
        case b: osc.Bundle  => Server.codec.encodedBundleSize (b)
        case m: osc.Message => Server.codec.encodedMessageSize(m)
      }
      if (sz >= 0x10000) {
        Console.err.println(s"ERROR: Packet size $sz exceeds 64K")
        osc.Packet.printTextOn(p, Server.codec, Console.err)
      }
    }

    // ---- compression ----

    private def compressControlSet(old: Seq[ControlSet], add: Seq[ControlSet]): Seq[ControlSet] = {
      var res = old
      add.foreach {
        case c @ ControlSet.Value(key, _) =>
          val j = res.indexWhere {
            case ControlSet.Value(`key`, _) => true
            case _ => false
          }
          res = if (j < 0) res :+ c else res.updated(j, c)

        case c => res :+= c
      }
      res
    }

    private def compress(b: osc.Bundle): osc.Bundle = {
      val in  = b.packets
      val num = in.length
      if (num < 10) return b   // don't bother

      // currently, we focus on the three messages
      // most frequently found in sound processes:
      // `/n_set`, `/n_mapan`, `/n_after`.
      // `/n_set` and `/n_mapan` can be collapsed
      // per node-ID; `/n_after` can be all collapsed.
      // To ensure correctness, we must not collapse
      // across `/s_new` and `/g_new` boundaries.
      // For simplicity, we also restrict collapse
      // of `/n_after` to adjacent messages

      // XXX TODO - actually, we don't yet optimize `/n_after`

      val out     = new Array[osc.Packet](num)
      var inOff   = 0
      var outOff  = 0

      var setMap    = Map.empty[Int, Int] // node-id to out-offset containing either n_set or s_new
      var mapanMap  = Map.empty[Int, Int] // node-id to out-offset containing n_mapan
      var nAfterIdx = -2

      while (inOff < num) {
        val p       = in(inOff)
        val append  = p match {
          case m: message.NodeSet =>
            val id = m.id
            val i = setMap.getOrElse(id, -1)
            val res = i < 0
            if (res) {
              setMap += id -> outOff
            } else {
              out(i) = (out(i): @unchecked) match {  // unfortunately those case classes do not have `copy` methods...
                case n: message.NodeSet =>
                  message.NodeSet(id, compressControlSet(n.pairs, m.pairs): _*)
                case n: message.SynthNew =>
                  message.SynthNew(n.defName, id, n.addAction, n.targetID,
                    compressControlSet(n.controls, m.pairs): _*)
              }
            }
            res

          case m: message.NodeMapan =>
            val id = m.id
            val i = mapanMap.getOrElse(id, -1)
            val res = i < 0
            if (res) {
              mapanMap += id -> outOff
            } else {
              var message.NodeMapan(_, mappings @ _*) = out(i)
              m.mappings.foreach {
                case c @ ControlABusMap.Single(key, _) =>
                  val j = mappings.indexWhere {
                    case ControlABusMap.Single(`key`, _) => true
                    case _ => false
                  }
                  mappings = if (j < 0) mappings :+ c else mappings.updated(j, c)

                case c @ ControlABusMap.Multi(key, _, numChannels) =>
                  val j = mappings.indexWhere {
                    case ControlABusMap.Multi(`key`, _, `numChannels`) => true
                    case _ => false
                  }
                  mappings = if (j < 0) mappings :+ c else mappings.updated(j, c)
              }
              out(i) = message.NodeMapan(id, mappings: _*)
            }
            res

          case m: message.NodeAfter =>
            val res = nAfterIdx != outOff - 1
            if (res) {  // predecessor was not n_after
              nAfterIdx = outOff
            } else {
              val message.NodeAfter(groups @ _*) = out(nAfterIdx)
              out(nAfterIdx) = message.NodeAfter(groups ++ m.groups: _*)
            }
            res

          case m: message.SynthNew =>
            val id = m.id
            // setMap   -= id
            setMap   += id -> outOff
            mapanMap -= id
            true

          case m: message.GroupNew =>
            m.groups.foreach { g =>
              val id = g.groupID
              setMap   -= id
              mapanMap -= id
            }
            true

          case _ => true
        }
        if (append) {
          out(outOff) = p
          outOff += 1
        }
        inOff += 1
      }

      if (outOff == num) b else {
        val outT = new Array[osc.Packet](outOff)
        System.arraycopy(out, 0, outT, 0, outOff)
        val res = osc.Bundle(b.timetag, outT: _*)
        //        Console.err.println("----------- BEFORE COMPRESSION -----------")
        //        osc.Packet.printTextOn(b  , Server.codec, Console.err)
        //        Console.err.println("----------- AFTER  COMPRESSION -----------")
        //        osc.Packet.printTextOn(res, Server.codec, Console.err)
        res
      }
    }

    // ---- side effects ----

    def !(p0: osc.Packet): Unit = {
      val p = if (USE_COMPRESSION) p0 match {
        case b: osc.Bundle => compress(b)
        case _ => p0
      } else  p0

      if (DEBUG_SIZE) checkPacket(p)
      peer ! p
    }

    def !!(b0: osc.Bundle): Future[Unit] = {
      val b       = if (USE_COMPRESSION) compress(b0) else b0
      val syncMsg = peer.syncMsg()
      val syncID  = syncMsg.id
      val bndlS   = osc.Bundle(b.timetag, b :+ syncMsg: _*)
      if (DEBUG_SIZE) checkPacket(bndlS)
      peer.!!(bndlS) {
        case message.Synced(`syncID`) =>
      }
    }

    def commit(future: Future[Unit]) = ()  // we don't use these
  }

  private final case class OfflineImpl(peer: SServer) extends Impl with Server.Offline {
    override def toString = s"$peer @offline"


    private val sync = new AnyRef

    var position  = 0L

    private var _bundles      = Vector.empty[osc.Bundle]
    private var _commits      = Vector.empty[Future[Unit]]

    private def time: Double  = position / sampleRate

    def committed(): Future[Unit] = sync.synchronized {
      val futures       = filteredCommits
      _commits          = Vector.empty
      implicit val exec = peer.clientConfig.executionContext
      NodeGraphImpl.reduceFutures(futures)
    }

    def bundles(addDefaultGroup: Boolean): Vec[osc.Bundle] = sync.synchronized {
      val res   = _bundles
      _bundles  = Vector.empty
      val res1  = if (res.isEmpty || !addDefaultGroup) res else {
        val b   = osc.Bundle(res.head.timetag,
          message.GroupNew(message.GroupNew.Data(groupID = 1, addAction = addToHead.id, targetID = 0)))
        b +: res
      }
      res1
    }

    private def addBundle(b: osc.Bundle): Unit = sync.synchronized {
      val b1 = if (b.timetag == osc.Timetag.now) osc.Bundle.secs(time, b: _*) else b
      val sz = Server.codec.encodedBundleSize(b1)
      // SuperCollider versions until 2014 have a hard-coded limit of 8K bundles in NRT!
      // cf. https://github.com/supercollider/supercollider/commit/f3f0f81de4259aa44983f1041589f895c91798a1
      val szOk = sz <= 8192
      if (szOk || b1.length == 1) {
        log(s"addBundle $b1")
        if (!szOk) log("addBundle - bundle exceeds 8k!")
        _bundles :+= b1
      } else {
        val tt = b1.timetag
        b.foreach(p => addBundle(osc.Bundle(tt, p)))
      }
    }

    def !(p: osc.Packet): Unit = {
      val b = p match {
        case m : osc.Message  => osc.Bundle.secs(time, m)
        case b0: osc.Bundle   => b0
      }
      addBundle(b)
    }

    def !!(bndl: osc.Bundle): Future[Unit] = {
      addBundle(bndl)
      Future.successful(())
    }

    // caller must have `sync`
    private def filteredCommits = _commits.filterNot(_.isCompleted)

    def commit(future: Future[Unit]): Unit =
      sync.synchronized {
        _commits = filteredCommits :+ future
      }
  }

  private abstract class Impl extends Server {

    def executionContext: ExecutionContext = peer.clientConfig.executionContext

    private val controlBusAllocator = BlockAllocator("control", peer.config.controlBusChannels)
    private val audioBusAllocator   = BlockAllocator("audio"  , peer.config.audioBusChannels, peer.config.internalBusIndex)
    private val bufferAllocator     = BlockAllocator("buffer" , peer.config.audioBuffers)
    private val nodeAllocator       = NodeIDAllocator(peer.clientConfig.clientID, peer.clientConfig.nodeIDOffset)

    val defaultGroup: Group = Group.wrap(this, peer.defaultGroup) // .default( this )

    def config      : Server .Config = peer.config
    def clientConfig: SClient.Config = peer.clientConfig

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

    def freeControlBus(index: Int, numChannels: Int)(implicit tx: Txn): Unit =
      controlBusAllocator.free(index, numChannels)(tx.peer)

    def freeAudioBus(index: Int, numChannels: Int)(implicit tx: Txn): Unit =
      audioBusAllocator.free(index, numChannels)(tx.peer)

    def allocBuffer(numConsecutive: Int)(implicit tx: Txn): Int = {
      val res = bufferAllocator.alloc(numConsecutive)(tx.peer)
      if (res < 0) throw AllocatorExhausted("Buffers exhausted for " + this)
      res
    }

    def freeBuffer(index: Int, numConsecutive: Int)(implicit tx: Txn): Unit =
      bufferAllocator.free(index, numConsecutive)(tx.peer)

    def nextNodeID()(implicit tx: Txn): Int = nodeAllocator.alloc()(tx.peer)
  }
}