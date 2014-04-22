/*
 *  Node.scala
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

import de.sciss.synth.{Node => SNode, ControlABusMap, ControlKBusMap, ControlSet}

trait Node extends Resource {
  // ---- abstract ----
  def peer: SNode

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

  def free(audible: Boolean = true)(implicit tx: Txn): Unit

  def set (audible: Boolean, pairs: ControlSet*)(implicit tx: Txn): Unit
  def setn(audible: Boolean, pairs: ControlSet*)(implicit tx: Txn): Unit

  //   def setIfOnline( pairs: ControlSet* )( implicit tx: Txn ) : Unit

  def mapn (audible: Boolean, pairs: ControlKBusMap*)(implicit tx: Txn): Unit
  def mapan(audible: Boolean, pairs: ControlABusMap*)(implicit tx: Txn): Unit

  //   def moveToHeadIfOnline( group: Group )( implicit tx: Txn ) : Unit

  def moveToHead(audible: Boolean, group: Group)(implicit tx: Txn): Unit
  def moveToTail(audible: Boolean, group: Group)(implicit tx: Txn): Unit
  def moveBefore(audible: Boolean, target: Node)(implicit tx: Txn): Unit
  def moveAfter (audible: Boolean, target: Node)(implicit tx: Txn): Unit
}