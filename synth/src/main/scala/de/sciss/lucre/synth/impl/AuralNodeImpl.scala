/*
 *  AuralNodeImpl.scala
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

import de.sciss.lucre.stm.Disposable
import de.sciss.synth.addBefore

import scala.concurrent.stm.{Ref, TMap}

object AuralNodeImpl {
  def apply(synth: Synth, inputBuses: Map[String, AudioBus], outputBuses: Map[String, AudioBus],
            resources: List[Disposable[Txn]], attrMap: Map[String, List[Disposable[Txn]]])
           (implicit tx: Txn): AuralNode = {
    val res = new Impl(synth, inputBuses = inputBuses, outputBuses = outputBuses,
      resources = resources, attrMap0 = attrMap)
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

  private final class Impl(synth: Synth, inputBuses: Map[String, AudioBus], outputBuses: Map[String, AudioBus],
                           resources: List[Disposable[Txn]], attrMap0: Map[String, List[Disposable[Txn]]])
    extends AuralNode {

    private val groupsRef = Ref[Option[AllGroups]](None)

    private val attrMap   = TMap[String, List[Disposable[Txn]]](attrMap0.toSeq: _*)

    override def toString = s"AuralProc($synth, outs = ${outputBuses.mkString("(", ", ", ")")}, " +
      s"ins = ${inputBuses.mkString("(", ", ", ")")}"

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
          synth.moveToHead(newGroup)
          all
        } { all =>
          moveAllTo(all, newGroup)
          all.main.free() // what can you do...?
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
      core.moveToTail(newGroup)
      all.pre .foreach(_.moveBefore(core))
      all.post.foreach(_.moveAfter (core))

      all.back.foreach { g =>
        if (g.isOnline) g.moveToHead(newGroup)
      }
    }

    def dispose()(implicit tx: Txn): Unit = {
      implicit val itx = tx.peer
      node.free()
      resources.foreach(_.dispose())
      attrMap  .foreach(_._2.foreach(_.dispose()))
      attrMap.clear()
      NodeGraph.removeNode(this)
    }

    def getInputBus (key: String): Option[AudioBus] = inputBuses .get(key)
    def getOutputBus(key: String): Option[AudioBus] = outputBuses.get(key)

    def addAttrResources(key: String, values: List[Disposable[Txn]])(implicit tx: Txn): Unit = {
      val all = attrMap.get(key)(tx.peer).fold(values)(_ ::: values)
      attrMap.put(key, all)(tx.peer)
    }

    def removeAttrResources(key: String)(implicit tx: Txn): Unit =
      attrMap.remove(key)(tx.peer).foreach(_.foreach(_.dispose()))
  }
}
