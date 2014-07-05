/*
 *  AuralNode.scala
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

package de.sciss
package lucre.synth

import concurrent.stm.Ref
import de.sciss.synth.addBefore

object AuralNode {
  def apply(synth: Synth, outputBuses: Map[String, AudioBus])
           (implicit tx: Txn): AuralNode = {
    val res = new Impl(synth, outputBuses)
    NodeGraph.addNode(res)
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

  private final class Impl(synth: Synth, outBuses: Map[String, AudioBus])
    extends AuralNode {

    private val groupsRef = Ref[Option[AllGroups]](None)
    private var users     = List.empty[DynamicUser]
    private var resources = List.empty[Resource]
    private var inBuses   = Map.empty[String, AudioBus]

    override def toString = s"AuralProc($synth, outs = ${outBuses.mkString("(", ", ", ")")}, " +
      s"ins = ${inBuses.mkString("(", ", ", ")")}"

    def server = synth.server

    def groupOption(implicit tx: Txn): Option[Group] = groupsRef.get(tx.peer).map(_.main)

    def node(implicit tx: Txn): Node = groupOption.getOrElse(synth)

    def group()(implicit tx: Txn): Group =
      groupOption.getOrElse {
        val res = Group(synth, addBefore) // i.e. occupy the same place as before
        group_=(res)
        res
      }

    def group_=(newGroup: Group)(implicit tx: Txn): Unit = {
      implicit val itx = tx.peer
      groupsRef.transform { groupsOpt =>
        val res = groupsOpt.fold {
          val all = AllGroups(main = newGroup)
          synth.moveToHead(audible = true, group = newGroup)
          all
        } { all =>
          moveAllTo(all, newGroup)
          all.main.free(audible = true) // what can you do...?
          all.copy(main = newGroup)
        }
        Some(res)
      }
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
      val node = groupOption.getOrElse(synth)
      node.free()
      if (users    .nonEmpty) users    .foreach(_.remove ())
      if (resources.nonEmpty) resources.foreach(_.dispose())
      NodeGraph.removeNode(this)
    }

    def getInputBus (key: String): Option[AudioBus] = inBuses .get(key)
    def getOutputBus(key: String): Option[AudioBus] = outBuses.get(key)

    def init(users: List[DynamicUser], resources: List[Resource]): Unit = {
      this.users      = users
      this.resources  = resources
    }

    def addInputBus (key: String, bus: AudioBus): Unit = inBuses += key -> bus

    // def addOutputBus(key: String, bus: AudioBus): Unit = outBuses += key -> bus

    // def addSink(key: String, sink: AudioBusNodeSetter)(implicit tx: Txn): Unit = {
    //   ?
    // }
  }
}

sealed trait AuralNode extends NodeRef {
  def server: Server

  /** Retrieves the main group of the Proc, or returns None if a group has not yet been assigned. */
  def groupOption(implicit tx: Txn): Option[Group]

  /** Retrieves the main group of the Proc. If this group has not been assigned yet,
    * this method will create a new group. */
  def group()(implicit tx: Txn): Group

  def group_=(value: Group)(implicit tx: Txn): Unit

  def preGroup()(implicit tx: Txn): Group

  def stop()(implicit tx: Txn): Unit

  def getInputBus (key: String): Option[AudioBus]
  def getOutputBus(key: String): Option[AudioBus]

  /** Warning: This is strictly for the builder to update the users, and it must be
    * called within the same transaction that the aural proc was created.
    */
  private[sciss] def init(users: List[DynamicUser], resources: List[Resource]): Unit

  // ditto
  private[sciss] def addInputBus(key: String, bus: AudioBus): Unit

  // def addSink(key: String, sink: AudioBusNodeSetter)(implicit tx: Txn): Unit
}