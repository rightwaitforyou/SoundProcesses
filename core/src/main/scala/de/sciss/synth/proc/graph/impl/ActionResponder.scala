/*
 *  ActionResponder.scala
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

package de.sciss.synth.proc
package graph
package impl

import de.sciss.lucre.stm
import de.sciss.lucre.synth.{Txn, DynamicUser, Node, Sys}
import de.sciss.{osc, synth}
import de.sciss.synth.{proc, GE, message}

import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.Ref

object ActionResponder {
  // via SendReply
  private def replyName(key: String): String = s"/$$act_$key"

  def makeUGen(trig: GE, values: Option[GE], key: String): Unit = {
    import synth._
    import ugen._
    // we cannot make values.size zero, because then in the multi-channel expansion,
    // the SendReply collapses :)
    SendReply.kr(trig = trig, values = values.getOrElse(0) /* Vec.empty[GE] */ , msgName = replyName(key), id = 0)
  }

  var DEBUG = false
}
class ActionResponder[S <: Sys[S]](objH: stm.Source[S#Tx, Obj[S]], key: String, synth: Node)
                                  (implicit cursor: stm.Cursor[S], context: AuralContext[S])
  extends DynamicUser {

  import ActionResponder._

  private val Name    = replyName(key)
  private val NodeID  = synth.peer.id
  private val trigResp = message.Responder(synth.server.peer) {
    case m @ osc.Message(Name, NodeID, 0, raw @ _*) =>
      if (DEBUG) println(s"ActionResponder($key, $NodeID) - received trigger")
      // logAural(m.toString())
      val values: Vec[Float] = raw.collect {
        case f: Float => f
      } (breakOut)
      SoundProcesses.atomic { implicit tx: S#Tx =>
        objH().attr.get(key).foreach { valueOpaque =>
          // for some reason we cannot pattern match for Action.Obj(action);
          // scalac gives us
          // "inferred type arguments [S] do not conform to method unapply type
          // parameter bounds [S <: de.sciss.lucre.event.Sys[S]]"
          // - WHY??
          if (valueOpaque.elem.isInstanceOf[proc.Action.Elem[S]]) {
            val action = valueOpaque.asInstanceOf[proc.Action.Obj[S]]
            if (DEBUG) println("...and found action")
            val universe = proc.Action.Universe(action, context.workspaceHandle, values)
            action.elem.peer.execute(universe)
          }
        }
      }
  }

  private val added = Ref(initialValue = false)

  def add()(implicit tx: Txn): Unit = if (!added.swap(true)(tx.peer)) {
    trigResp.add()
    // Responder.add is non-transactional. Thus, if the transaction fails, we need to remove it.
    scala.concurrent.stm.Txn.afterRollback { _ =>
      trigResp.remove()
    } (tx.peer)

    // synth.onEnd(trigResp.remove())
  }

  def remove()(implicit tx: Txn): Unit = if (added.swap(false)(tx.peer)) {
    trigResp.remove()
    scala.concurrent.stm.Txn.afterRollback { _ =>
      trigResp.add()
    } (tx.peer)
  }
}
