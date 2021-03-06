/*
 *  Node.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth

import de.sciss.synth.{Node => SNode, _}

trait Node extends Resource with NodeRef {
  // ---- abstract ----
  def peer: SNode

  /** Refers to itself */
  def node(implicit tx: Txn): Node = this

  //   final val isOnline: State = State( this, "isOnline", init = initOnline )

  def onEndTxn(fun: Txn => Unit)(implicit tx: Txn): Unit
  def onEnd   (code:    => Unit)(implicit tx: Txn): Unit

  //   final def server = peer.server

  def read(assoc: (AudioBus  , String))(implicit tx: Txn): AudioBusNodeSetter
  def read(assoc: (ControlBus, String))(implicit tx: Txn): ControlBusNodeSetter

  /** Associates an audio bus with this node such that the node writes to this bus.
    * This creates a `DynamicAudioBusUser` which will be freed automatically when
    * this node ends.
    */
  def write    (assoc: (AudioBus  , String))(implicit tx: Txn): AudioBusNodeSetter
  def write    (assoc: (ControlBus, String))(implicit tx: Txn): ControlBusNodeSetter
  def readWrite(assoc: (AudioBus  , String))(implicit tx: Txn): AudioBusNodeSetter
  def readWrite(assoc: (ControlBus, String))(implicit tx: Txn): ControlBusNodeSetter

  def map(assoc: (AudioBus  , String))(implicit tx: Txn): AudioBusNodeSetter
  def map(assoc: (ControlBus, String))(implicit tx: Txn): ControlBusNodeSetter

  def free()(implicit tx: Txn): Unit

  def set (pairs: ControlSet*)(implicit tx: Txn): Unit
  def setn(pairs: ControlSet*)(implicit tx: Txn): Unit

  def fill(data: ControlFillRange*)(implicit tx: Txn): Unit

  //   def setIfOnline( pairs: ControlSet* )( implicit tx: Txn ) : Unit

  def mapn (pairs: ControlKBusMap*)(implicit tx: Txn): Unit
  def mapan(pairs: ControlABusMap*)(implicit tx: Txn): Unit

  //   def moveToHeadIfOnline( group: Group )( implicit tx: Txn ) : Unit

  def moveToHead(group: Group)(implicit tx: Txn): Unit
  def moveToTail(group: Group)(implicit tx: Txn): Unit
  def moveBefore(target: Node)(implicit tx: Txn): Unit
  def moveAfter (target: Node)(implicit tx: Txn): Unit

  def run(state: Boolean)(implicit tx: Txn): Unit

  def release(releaseTime: Optional[Double] = None)(implicit tx: Txn): Unit
}