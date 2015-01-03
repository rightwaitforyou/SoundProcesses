/*
 *  ServerImpl.scala
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

package de.sciss.lucre.synth
package impl

import de.sciss.osc.Timetag

import scala.annotation.tailrec
import scala.collection.mutable
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

  private final val MaxPacketSize = 0x8000 // 0x10000 // 64K

  private final case class OnlineImpl(peer: SServer) extends Impl {
    override def toString = peer.toString()

    //    private def printExcessPacket(p: osc.Packet, sz: Int): Unit = {
    //      Console.err.println(s"ERROR: Packet size $sz exceeds $MaxPacketSize")
    //      osc.Packet.printTextOn(p, Server.codec, Console.err)
    //    }
    //
    //    // returns `true` if packet size ok, `false` if too large
    //    private def checkPacket(p: osc.Packet): Boolean = {
    //      val sz = p match {
    //        case b: osc.Bundle  => Server.codec.encodedBundleSize (b)
    //        case m: osc.Message => Server.codec.encodedMessageSize(m)
    //      }
    //      val ok = sz < MaxPacketSize
    //      if (!ok) {
    //        printExcessPacket(p, sz)
    //      }
    //      ok
    //    }

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
      // of `/n_after` to adjacent messages.
      // Another optimization is collapsing adjacent
      // `/n_free` messages

      // XXX TODO - actually, we don't yet optimize `/n_after`

      val out     = new Array[osc.Packet](num)
      var inOff   = 0
      var outOff  = 0

      var setMap    = Map.empty[Int, Int] // node-id to out-offset containing either n_set or s_new
      var mapanMap  = Map.empty[Int, Int] // node-id to out-offset containing n_mapan
      var nAfterIdx = -2
      var nFreeIdx  = -2

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

              //              // more ambitious:
              //              // collapse a single `/n_after` with an immediate
              //              // preceding `/s_new`.
              //              val g = m.groups
              //              if (g.size == 1) {
              //                val (id, after) = g.head
              //                val newIdx = setMap.getOrElse(id, -1)
              //                if (newIdx >= 0) {
              //                  ...
              //                }
              //              }

            } else {
              val message.NodeAfter(groups @ _*) = out(nAfterIdx)
              out(nAfterIdx) = message.NodeAfter(groups ++ m.groups: _*)
            }
            res

          case m: message.NodeFree =>
            val res = nFreeIdx != outOff - 1
            if (res) {  // predecessor was not n_after
              nFreeIdx = outOff
            } else {
              val message.NodeFree(ids @ _*) = out(nFreeIdx)
              out(nFreeIdx) = message.NodeFree(ids ++ m.ids: _*)
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

    private def splitAndSend[A, B](init: A, iter: Iterator[osc.Packet], addSize: Int)(fun: Vec[osc.Packet] => B)
                                  (combine: (A, B) => A): A = {
      @tailrec def loop(a: A, sz: Int, builder: mutable.Builder[osc.Packet, Vec[osc.Packet]]): A =
        if (iter.isEmpty) {
          val res = builder.result()
          if (res.nonEmpty) {
            val a1 = fun(res)
            val a2  = combine(a, a1)
            a2
          } else a

        } else {
          val next  = iter.next()
          val sz1   = next.encodedSize(Server.codec) + 4
          val sz2   = sz + sz1
          if (sz2 >= MaxPacketSize) {
            val res = builder.result()
            if (res.isEmpty) sys.error(s"Cannot encode packet -- too large ($sz1)")
            val a1  = fun(res)
            val a2  = combine(a, a1)
            val newBuilder = Vec.newBuilder[osc.Packet]
            newBuilder += next
            loop(a2, 16 + addSize + sz1, newBuilder)
          } else {
            builder += next
            loop(a, sz2, builder)
          }
        }

      loop(init, 16 + addSize, Vec.newBuilder[osc.Packet])
    }

    def ! (p0: osc.Packet): Unit = {
      val p = p0 match {
        case b0: osc.Bundle if USE_COMPRESSION => compress(b0)
        case _ => p0
      }

      p match {
        case b: osc.Bundle if DEBUG_SIZE =>
          val sz0 = Server.codec.encodedBundleSize(b)
          if (sz0 < MaxPacketSize) {
            peer ! b
          } else {
            // Since the bundle is synchronous, it is not trivial to split it
            // into several bundles. And if we split at arbitrary points some
            // very bad things could happen, for example sound or feedback
            // going to wrong temporary buses, so this is absolutely forbidden.
            //
            // We take this as an "emergency" branch that one should avoid to
            // be taken at all costs. The solution for this branch is that we
            // temporarily pause the server's default group. That way no
            // damage can be done, but it may result in a short noticable bit
            // of silence. That's as good as it gets, I suppose...
            Console.err.println(s"WARNING: Bundle size $sz0 exceeds $MaxPacketSize. Splitting into multiple bundles")
            val gid   = peer.defaultGroup.id
            val iter  =
              Iterator.single(message.NodeRun(gid -> false)) ++ b.packets.iterator ++
              Iterator.single(message.NodeRun(gid -> true ))

            splitAndSend[Unit, Unit](init = (), iter = iter, addSize = 0) { packets =>
              peer ! osc.Bundle(b.timetag, packets: _*)
            } ((_, _) => ())
          }

        case _ =>
          peer ! p
      }
    }

    def !! (b0: osc.Bundle): Future[Unit] = {
      val b   = if (USE_COMPRESSION) compress(b0) else b0
      val tt  = b.timetag
      if (DEBUG_SIZE) {
        val sz0     = Server.codec.encodedBundleSize(b)
        if (sz0 + 20 < MaxPacketSize) {
          perform_!!(tt, b.packets)
        } else {
          val iter = b.packets.iterator
          val futs = splitAndSend[Vec[Future[Unit]], Future[Unit]](init = Vector.empty,
                                                                   iter = iter, addSize = 20 /* /sync */) { packets =>
            perform_!!(tt, packets)
          } (_ :+ _)
          import ExecutionContext.Implicits.global
          Future.reduce[Unit, Unit](futs)((_, _) => ())
        }
      } else {
        perform_!!(tt, b.packets)
      }
    }

    private def perform_!!(tt: Timetag, packets: Seq[osc.Packet]): Future[Unit] = {
      val syncMsg = peer.syncMsg()
      val syncID  = syncMsg.id
      val bndlS   = osc.Bundle(tt, packets :+ syncMsg: _*)
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