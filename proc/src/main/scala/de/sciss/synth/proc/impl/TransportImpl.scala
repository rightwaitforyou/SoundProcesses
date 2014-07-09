/*
 *  TransportImpl.scala
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

package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm
import de.sciss.lucre.stm.IdentifierMap
import de.sciss.lucre.synth.{Txn, Server, Sys}

import scala.concurrent.stm.Ref

object TransportImpl {
  def apply[S <: Sys[S]](aural: AuralSystem, scheduler: Scheduler[S])(implicit tx: S#Tx): Transport[S] = {
    val objMap  = tx.newInMemoryIDMap[stm.Source[S#Tx, Obj[S]]]
    val viewMap = tx.newInMemoryIDMap[AuralObj[S]]
    val res     = new Impl(aural, scheduler, objMap, viewMap)
    aural.addClient(res)
    aural.serverOption.foreach(res.auralStarted)
    res
  }

  private final class Impl[S <: Sys[S]](aural: AuralSystem, val scheduler: Scheduler[S],
                                        objMap: IdentifierMap[S#ID, S#Tx, stm.Source[S#Tx, Obj[S]]],
                                        viewMap: IdentifierMap[S#ID, S#Tx, AuralObj[S]])
    extends Transport[S] with ObservableImpl[S, Transport.Update[S]] with AuralSystem.Client {

    def play()(implicit tx: S#Tx): Unit = ???

    def stop()(implicit tx: S#Tx): Unit = ???

    def position(implicit tx: S#Tx): Long = ???

    def seek(position: Long)(implicit tx: S#Tx): Unit = ???

    def isPlaying(implicit tx: S#Tx): Boolean = ???

    // we stupidly need these because identifier-map doesn't have an iterator
    private val objSeq  = Ref(Vector.empty[stm.Source[S#Tx, Obj[S]]])
    private val viewSeq = Ref(Vector.empty[AuralObj[S]])

    def addObject(obj: Obj[S])(implicit tx: S#Tx): Unit = {
      val id = obj.id
      if (objMap.contains(id)) throw new IllegalArgumentException(s"Object $obj was already added to transport")
      val objH = tx.newHandle(obj)
      objMap.put(id, objH)
      objSeq.transform(_ :+ objH)(tx.peer)

      contextOption.foreach { implicit context =>
        val view = mkView(obj)

      }
    }

    private def mkView(obj: Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = {
      val view = AuralObj(obj)
      viewMap.put(obj.id, view)
      viewSeq.transform(_ :+ view)(tx.peer)
      if (isPlaying) {
        ???
      }
      view
    }

    def removeObject(obj: Obj[S])(implicit tx: S#Tx): Unit = ???

    def dispose()(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      aural.removeClient(this)
      objMap .dispose()
      viewMap.dispose()
      objSeq() = Vector.empty
      viewSeq.swap(Vector.empty).foreach { view =>
        view.dispose()
      }
    }

    // ---- aural system ----

    private val contextRef = Ref(Option.empty[AuralContext[S]])

    def contextOption(implicit tx: S#Tx): Option[AuralContext[S]] = contextRef.get(tx.peer)

    def auralStarted(s: Server)(implicit tx: Txn): Unit = {
      tx.afterCommit {
        scheduler.cursor.step { implicit tx =>
          started(s)
        }
      }
    }

    private def started(server: Server)(implicit tx: S#Tx): Unit = {
      implicit val aural = AuralContext(server, scheduler)
      contextRef.set(Some(aural))(tx.peer)
      objSeq.get(tx.peer).foreach { objH =>
        val obj   = objH()
        val view  = mkView(obj)
      }
    }

    def auralStopped()(implicit tx: Txn): Unit = {
      contextRef.set(None)(tx.peer)

      ??? // dispose views
    }
  }
}
