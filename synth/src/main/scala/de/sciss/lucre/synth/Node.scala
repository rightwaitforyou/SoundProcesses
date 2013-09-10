/*
 *  Node.scala
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

package de.sciss.lucre.synth

import de.sciss.synth.{Node => SNode, ControlABusMap, ControlKBusMap, ControlSetMap}

trait Node extends Resource {
  // ---- abstract ----
  def peer: SNode

  //   final val isOnline: State = State( this, "isOnline", init = initOnline )

  def onEndTxn(fun: Txn => Unit)(implicit tx: Txn): Unit
  def onEnd   (code:    => Unit)(implicit tx: Txn): Unit

  //   final def server = peer.server

  def read(assoc: (RichAudioBus  , String))(implicit tx: Txn): AudioBusNodeSetter
  def read(assoc: (RichControlBus, String))(implicit tx: Txn): ControlBusNodeSetter

  /**
   * Associates an audio bus with this node such that the node writes to this bus.
   * This creates a `DynamicAudioBusUser` which will be freed automatically when
   * this node ends.
   */
  def write    (assoc: (RichAudioBus  , String))(implicit tx: Txn): AudioBusNodeSetter
  def write    (assoc: (RichControlBus, String))(implicit tx: Txn): ControlBusNodeSetter
  def readWrite(assoc: (RichAudioBus  , String))(implicit tx: Txn): AudioBusNodeSetter
  def readWrite(assoc: (RichControlBus, String))(implicit tx: Txn): ControlBusNodeSetter

  def map(assoc: (RichAudioBus  , String))(implicit tx: Txn): AudioBusNodeSetter
  def map(assoc: (RichControlBus, String))(implicit tx: Txn): ControlBusNodeSetter

  def free(audible: Boolean = true)(implicit tx: Txn): Unit

  def set (audible: Boolean, pairs: ControlSetMap*)(implicit tx: Txn): Unit
  def setn(audible: Boolean, pairs: ControlSetMap*)(implicit tx: Txn): Unit

  //   def setIfOnline( pairs: ControlSetMap* )( implicit tx: Txn ) : Unit

  def mapn (audible: Boolean, pairs: ControlKBusMap*)(implicit tx: Txn): Unit
  def mapan(audible: Boolean, pairs: ControlABusMap*)(implicit tx: Txn): Unit

  //   def moveToHeadIfOnline( group: Group )( implicit tx: Txn ) : Unit

  def moveToHead(audible: Boolean, group: Group)(implicit tx: Txn): Unit
  def moveToTail(audible: Boolean, group: Group)(implicit tx: Txn): Unit
  def moveBefore(audible: Boolean, target: Node)(implicit tx: Txn): Unit
  def moveAfter (audible: Boolean, target: Node)(implicit tx: Txn): Unit
}