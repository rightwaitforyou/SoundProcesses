/*
 *  NodeImpl.scala
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

import concurrent.stm.{TxnExecutor, InTxn, Ref}
import de.sciss.synth.{ControlABusMap, ControlKBusMap, ControlSetMap}
import collection.immutable.{IndexedSeq => Vec}

object NodeImpl {
  private val EmptyOnEnd = new OnEnd(Vec.empty, Vec.empty)

  private final case class OnEnd(direct: Vec[() => Unit], inTxn: Vec[Txn => Unit]) {
    def nonEmpty = direct.nonEmpty || inTxn.nonEmpty
  }
}

trait NodeImpl extends ResourceImpl with Node {
  import NodeImpl._

  private val onEndFuns = Ref(EmptyOnEnd)

  peer.onEnd {
    val funs = onEndFuns.single.get
    if (funs.nonEmpty) {
      spawn {
        implicit itx =>
          implicit val ptx: Txn = Txn.applyPlain(itx)
          funs.direct.foreach(_.apply())
          funs.inTxn.foreach(_.apply(ptx))
      }
    }
  }

  // there is still a ScalaCollider actor problem with
  // sending out new messages from an onEnd because that
  // is executed within the osc receiver actor.
  // decouple it instead.
  private def spawn(fun: InTxn => Unit): Unit =
    SoundProcesses.pool.submit(new Runnable {
      def run(): Unit = TxnExecutor.defaultAtomic(fun)
    })

  final def onEndTxn(fun: Txn => Unit)(implicit tx: Txn): Unit =
    onEndFuns.transform(e => e.copy(inTxn = e.inTxn :+ fun))(tx.peer)

  final def onEnd(code: => Unit)(implicit tx: Txn): Unit =
    onEndFuns.transform(e => e.copy(direct = e.direct :+ (() => code)))(tx.peer)

  final def read(assoc: (RichAudioBus, String))(implicit tx: Txn): AudioBusNodeSetter = {
    val (rb, name) = assoc
    val reader = BusNodeSetter.reader(name, rb, this)
    registerSetter(reader)
    reader
  }

  final def read(assoc: (RichControlBus, String))(implicit tx: Txn): ControlBusNodeSetter = {
    val (rb, name) = assoc
    val reader = BusNodeSetter.reader(name, rb, this)
    registerSetter(reader)
    reader
  }

  /** Associates an audio bus with this node such that the node writes to this bus.
    * This creates a `DynamicAudioBusUser` which will be freed automatically when
    * this node ends.
    */
  final def write(assoc: (RichAudioBus, String))(implicit tx: Txn): AudioBusNodeSetter = {
    val (rb, name) = assoc
    val writer = BusNodeSetter.writer(name, rb, this)
    registerSetter(writer)
    writer
  }

  final def write(assoc: (RichControlBus, String))(implicit tx: Txn): ControlBusNodeSetter = {
    val (rb, name) = assoc
    val writer = BusNodeSetter.writer(name, rb, this)
    registerSetter(writer)
    writer
  }

  final def readWrite(assoc: (RichAudioBus, String))(implicit tx: Txn): AudioBusNodeSetter = {
    val (rb, name) = assoc
    val rw = BusNodeSetter.readerWriter(name, rb, this)
    registerSetter(rw)
    rw
  }

  final def readWrite(assoc: (RichControlBus, String))(implicit tx: Txn): ControlBusNodeSetter = {
    val (rb, name) = assoc
    val rw = BusNodeSetter.readerWriter(name, rb, this)
    registerSetter(rw)
    rw
  }

  final def map(assoc: (RichAudioBus, String))(implicit tx: Txn): AudioBusNodeSetter = {
    val (rb, name) = assoc
    val mapper = BusNodeSetter.mapper(name, rb, this)
    registerSetter(mapper)
    mapper
  }

  final def map(assoc: (RichControlBus, String))(implicit tx: Txn): ControlBusNodeSetter = {
    val (rb, name) = assoc
    val mapper = BusNodeSetter.mapper(name, rb, this)
    registerSetter(mapper)
    mapper
  }

  private def registerSetter(bns: BusNodeSetter)(implicit tx: Txn): Unit = {
    requireOnline()
    bns.add()
    onEndTxn {
      implicit tx => bns.remove()
    }
  }

  final def dispose()(implicit tx: Txn): Unit = free(audible = true)

  /** Note: this is graceful in not throwing up if the node was already freed. */
  final def free(audible: Boolean = true)(implicit tx: Txn): Unit = {
    // requireOnline()
    if (isOnline) {
      tx.addMessage(this, peer.freeMsg, audible = audible)
      setOnline(value = false)
    }
  }

  final def set(audible: Boolean, pairs: ControlSetMap*)(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.setMsg(pairs: _*), audible = audible)
  }

  final def setn(audible: Boolean, pairs: ControlSetMap*)(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.setnMsg(pairs: _*), audible = audible)
  }

  //   final def setIfOnline( pairs: ControlSetMap* )( implicit tx: Txn ) {
  //      // XXX eventually this should be like set with different failure resolution
  //      if( isOnline.get ) {
  //         tx.addMessage( peer.setMsg( pairs: _* ), change = None, audible = true, noErrors = true )
  //      }
  ////      if( isOnline.get ) tx.add( OSCBundle(
  ////         OSCMessage( "/error", -1 ), node.setMsg( pairs: _* ), OSCMessage( "/error", -2 )), true )
  //   }

  final def mapn(audible: Boolean, pairs: ControlKBusMap*)(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.mapnMsg(pairs: _*), audible = audible)
  }

  final def mapan(audible: Boolean, pairs: ControlABusMap*)(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.mapanMsg(pairs: _*), audible = audible) // , dependencies = this :: Nil /* ?! */)
  }

  final def moveToHead(audible: Boolean, group: Group)(implicit tx: Txn): Unit = {
    require(isOnline && group.isOnline, s"Both source $this and target $group must be online")
    tx.addMessage(this, peer.moveToHeadMsg(group.peer), audible = audible, dependencies = group :: Nil)
  }

  //   final def moveToHeadIfOnline( group: Group )( implicit tx: Txn ) {
  //      if( isOnline.get ) {
  //         tx.addMessage( peer.moveToHeadMsg( group.peer ), change = None, audible = true,
  //                        dependencies = Map( group.isOnline -> true ), noErrors = true )
  //      }
  //   }

  final def moveToTail(audible: Boolean, group: Group)(implicit tx: Txn): Unit = {
    require(isOnline && group.isOnline, s"Both source $this and target $group must be online")
    tx.addMessage(this, peer.moveToTailMsg(group.peer), audible = audible, dependencies = group :: Nil)
  }

  final def moveBefore(audible: Boolean, target: Node)(implicit tx: Txn): Unit = {
    require(isOnline && target.isOnline, s"Both source $this and target $target must be online")
    tx.addMessage(this, peer.moveBeforeMsg(target.peer), audible = audible, dependencies = target :: Nil)
  }

  final def moveAfter(audible: Boolean, target: Node)(implicit tx: Txn): Unit = {
    require(isOnline && target.isOnline, s"Both source $this and target $target must be online")
    tx.addMessage(this, peer.moveAfterMsg(target.peer), audible = audible, dependencies = target :: Nil)
  }
}