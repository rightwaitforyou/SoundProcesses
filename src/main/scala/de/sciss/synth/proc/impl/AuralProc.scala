/*
 *  AuralProc.scala
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

import concurrent.stm.Ref
import de.sciss.synth.addBefore

object AuralProc {
  def apply(synth: Synth, outputBuses: Map[String, RichAudioBus])
           (implicit tx: Txn): AuralProc = {
    val res = new Impl(synth, outputBuses)
    ProcDemiurg.addVertex(res)
    res
  }

   /*
    * The possible differentiation of groups for an aural process. The minimum configuration is one main
    * group. If synths need to play before the main process, a pre group will be established, if they need
    * to play after the main process, a post group will be established. If multiple synths participate in
    * the main process, a core group may be established. A back group will hold 'forgotten' synths, once
    * they have been marked to fade out and be removed by themselves.
    */
  private final case class AllGroups(main: Group, pre: Option[Group] = None,
                                     core: Option[Group] = None,
                                     post: Option[Group] = None, back: Option[Group] = None)

  private final class Impl(synth: Synth, outBuses: Map[String, RichAudioBus])
    extends AuralProc {

    private val groupsRef = Ref[Option[AllGroups]](None)
    private var busUsers  = List.empty[DynamicBusUser]
    private var inBuses   = Map.empty[String, RichAudioBus]

    override def toString = s"AuralProc($synth, outs = ${outBuses.mkString("(", ", ", ")")}, " +
      s"ins = ${inBuses.mkString("(", ", ", ")")}"

    def server = synth.server

    def groupOption(implicit tx: Txn): Option[Group] = groupsRef.get(tx.peer).map(_.main)

    def group()(implicit tx: Txn): Group =
      groupOption.getOrElse {
        val res = Group(server)
        group_=(res)
        res
      }

    def group_=(newGroup: Group)(implicit tx: Txn): Unit = {
      implicit val itx = tx.peer
      groupsRef.transform(_ map { all =>
        moveAllTo(all, newGroup)
        all.main.free(audible = true) // que se puede...?
        all.copy(main = newGroup)
      } orElse {
        val all = AllGroups(main = newGroup)
        synth.moveToHead(audible = true, group = newGroup)
        Some(all)
      })
    }

    @inline private def preGroupOption(implicit tx: Txn): Option[Group] = groupsRef.get(tx.peer).flatMap(_.pre)

    def preGroup()(implicit tx: Txn): Group = {
      implicit val itx = tx.peer
      preGroupOption.getOrElse {
        /* val main = */ group() // creates group if necessary
        val all       = groupsRef().get
        val target    = anchorNode
        val addAction = addBefore
        val res       = Group(target = target, addAction = addAction)
        groupsRef.set(Some(all.copy(pre = Some(res))))
        res
      }
    }

    private def anchorNode(implicit tx: Txn): Node = {
      implicit val itx = tx.peer
      groupsRef().flatMap(_.core) getOrElse synth // runningRef().map( _.anchorNode )
    }

    private def moveAllTo(all: AllGroups, newGroup: Group)(implicit tx: Txn): Unit = {
      val core = anchorNode
      core.moveToTail(audible = true, group = newGroup)
      all.pre .foreach(_.moveBefore(audible = true, target = core))
      all.post.foreach(_.moveAfter (audible = true, target = core))

      all.back.foreach { g =>
        if (g.isOnline) g.moveToHead(audible = true, group = newGroup)
      }
    }

    def stop()(implicit tx: Txn): Unit = {
      synth.free()
      busUsers.foreach(_.remove())
      ProcDemiurg.removeVertex(this)
    }

    def getInputBus (key: String): Option[RichAudioBus] = inBuses .get(key)
    def getOutputBus(key: String): Option[RichAudioBus] = outBuses.get(key)

    private[impl] def setBusUsers(users: List[DynamicBusUser]): Unit = busUsers = users

    private[impl] def addInputBus(key: String, bus: RichAudioBus): Unit = inBuses += key -> bus

    // def addSink(key: String, sink: AudioBusNodeSetter)(implicit tx: Txn): Unit = {
    //   ?
    // }
  }
}

sealed trait AuralProc /* extends Writer */ {
  def server: Server

  /** Retrieves the main group of the Proc, or returns None if a group has not yet been assigned. */
  def groupOption(implicit tx: Txn): Option[Group]

  /** Retrieves the main group of the Proc. If this group has not been assigned yet,
    * this method will create a new group. */
  def group()(implicit tx: Txn): Group

  def group_=(value: Group)(implicit tx: Txn): Unit

  def preGroup()(implicit tx: Txn): Group

  def stop()(implicit tx: Txn): Unit

  def getInputBus (key: String): Option[RichAudioBus]
  def getOutputBus(key: String): Option[RichAudioBus]

  /** Warning: This is strictly for the builder to update the bus users, and it must be
    * called within the same transaction that the aural proc was created.
    */
  private[impl] def setBusUsers(users: List[DynamicBusUser]): Unit

  // dito
  private[impl] def addInputBus(key: String, bus: RichAudioBus): Unit

  // def addSink(key: String, sink: AudioBusNodeSetter)(implicit tx: Txn): Unit
}