package de.sciss.synth.proc
package graph
package impl

import de.sciss.lucre.stm
import de.sciss.lucre.synth.{Synth, Sys}
import de.sciss.{osc, synth}
import de.sciss.synth.{proc, GE, message}

object ActionResponder {
  // via SendReply
  private def replyName(key: String): String = s"/$$act_$key"

  def makeUGen(trig: GE, key: String): Unit = {
    import synth._
    import ugen._
    // we cannot make values.size zero, because then in the multi-channel expansion,
    // the SendReply collapses :)
    SendReply.kr(trig = trig, values = 0 /* Vec.empty[GE] */, msgName = replyName(key), id = 0)
  }

  var DEBUG = false

  def install[S <: Sys[S]](obj: Obj[S], key: String, synth: Synth)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Unit = {
    val Name    = replyName(key)
    val NodeID  = synth.peer.id
    val objH    = tx.newHandle(obj)
    val trigResp = message.Responder.add(synth.server.peer) {
      case m @ osc.Message(Name, NodeID, 0, _ /* dummy single value */) =>
        if (DEBUG) println(s"ActionResponder($key, $NodeID) - received trigger")
        // logAural(m.toString())
        SoundProcesses.atomic { implicit tx: S#Tx =>
          objH().attr.get(key).foreach { valueOpaque =>
            // for some reason we cannot pattern match for Action.Obj(action);
            // scalac gives us
            // "inferred type arguments [S] do not conform to method unapply's type
            // parameter bounds [S <: de.sciss.lucre.event.Sys[S]]"
            // - WHY??
            if (valueOpaque.elem.isInstanceOf[proc.Action.Elem[S]]) {
              val action = valueOpaque.asInstanceOf[proc.Action.Obj[S]]
              if (DEBUG) println("...and found action")
              val universe = proc.Action.Universe(action)
              action.elem.peer.execute(universe)
            }
          }
        }
    }
    // Responder.add is non-transactional. Thus, if the transaction fails, we need to remove it.
    scala.concurrent.stm.Txn.afterRollback { _ =>
      trigResp.remove()
    } (tx.peer)

    synth.onEnd(trigResp.remove())
  }
}
