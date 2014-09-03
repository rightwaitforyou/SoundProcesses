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
import de.sciss.span.Span
import de.sciss.synth.proc
import proc.{logTransport => logT}

import scala.concurrent.stm.{TSet, Ref}

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
                                        objMap : IdentifierMap[S#ID, S#Tx, stm.Source[S#Tx, Obj[S]]],
                                        viewMap: IdentifierMap[S#ID, S#Tx, AuralObj[S]])
    extends Transport[S] with ObservableImpl[S, Transport.Update[S]] with AuralSystem.Client {

    private final class PlayTime(val wallClock0: Long, val pos0: Long) {
      override def toString = s"[pos0 = $pos0 / ${TimeRef.framesToSecs(pos0)}, time0 = $wallClock0]"

      def isPlaying: Boolean = wallClock0 != Long.MinValue

      def play()(implicit tx: S#Tx): PlayTime =
        new PlayTime(wallClock0 = scheduler.time, pos0 = pos0)

      def currentPos(implicit tx: S#Tx): Long = if (!isPlaying) pos0 else {
        val wc1   = scheduler.time
        val delta = wc1 - wallClock0
        pos0 + delta
      }

      def stop()(implicit tx: S#Tx): PlayTime =
        new PlayTime(wallClock0 = Long.MinValue, pos0 = currentPos)
    }

    // we stupidly need these because identifier-map doesn't have an iterator
    private val objSet  = TSet.empty[stm.Source[S#Tx, Obj[S]]]
    private val viewSet = TSet.empty[AuralObj[S]]

    private val timeBaseRef = Ref(new PlayTime(wallClock0 = Long.MinValue, pos0 = 0L))

    def views(implicit tx: S#Tx): Set[AuralObj[S]] = viewSet.single.toSet

    def play()(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      val timeBase0 = timeBaseRef()
      if (timeBase0.isPlaying) return

      val timeBase1 = timeBase0.play()
      timeBaseRef() = timeBase1
      logT(s"transport - play - $timeBase1")

      playViews()
      fire(Transport.Play(this, timeBase1.pos0))
    }

    private def playViews()(implicit tx: S#Tx): Unit = {
      val tr = mkTimeRef()
      logT(s"transport - playViews - $tr")
      viewSet.foreach(_.play(tr))(tx.peer)
    }

    def stop()(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      val timeBase0 = timeBaseRef()
      if (!timeBase0.isPlaying) return

      val timeBase1 = timeBase0.stop()
      timeBaseRef() = timeBase1
      logT(s"transport - stop - $timeBase1")

      stopViews()
      fire(Transport.Stop(this, timeBase1.pos0))
    }

    private def stopViews()(implicit tx: S#Tx): Unit =
      viewSet.foreach(_.stop())(tx.peer)

    def position(implicit tx: S#Tx): Long = timeBaseRef.get(tx.peer).currentPos

    def seek(position: Long)(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      val p = isPlaying
      if (p) stopViews()

      val timeBase1 = new PlayTime(wallClock0 = if (p) scheduler.time else Long.MinValue, pos0 = position)
      timeBaseRef() = timeBase1
      logT(s"transport - seek - $timeBase1")

      if (p) playViews()
      fire(Transport.Seek(this, timeBase1.pos0, isPlaying = p))
    }

    def isPlaying(implicit tx: S#Tx): Boolean = timeBaseRef.get(tx.peer).isPlaying

    def addObject(obj: Obj[S])(implicit tx: S#Tx): Unit = {
      val id = obj.id
      if (objMap.contains(id)) throw new IllegalArgumentException(s"Object $obj was already added to transport")
      val objH = tx.newHandle(obj)
      objMap.put(id, objH)
      objSet.add(objH)(tx.peer)

      contextOption.foreach { implicit context =>
        val view = mkView(obj)
        if (isPlaying) view.play(mkTimeRef())
      }
    }

    def removeObject(obj: Obj[S])(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      val id    = obj.id
      // we need objH to find the index in objSeq
      val objH  = objMap.get(id).getOrElse {
        Console.err.println(s"Warning: transport - removeObject - not found: $obj")
        return
      }
      objMap.remove(id)
      objSet.remove(objH)
      // note - if server not running, there are no views
      viewMap.get(id).foreach { view =>
        viewMap.remove(id)
        viewSet.remove(view)
        if (isPlaying) view.stop()
      }
    }

    private def mkTimeRef()(implicit tx: S#Tx) = TimeRef(Span.from(0L), position)

    private def mkView(obj: Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = {
      val view = AuralObj(obj)
      viewMap.put(obj.id, view)
      viewSet.add(view)(tx.peer)
      view
    }

    private def clearSet[A](s: TSet[A])(implicit tx: S#Tx): Unit =
      s.retain(_ => false)(tx.peer) // no `clear` method

    def dispose()(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      aural.removeClient(this)
      objMap.dispose()
      clearSet(objSet)
      disposeViews()
    }

    private def disposeViews()(implicit tx: S#Tx): Unit = {
      viewMap.dispose()
      viewSet.foreach(_.dispose())(tx.peer)
      clearSet(viewSet)
    }

    // ---- aural system ----

    private val contextRef = Ref(Option.empty[AuralContext[S]])

    def contextOption(implicit tx: S#Tx): Option[AuralContext[S]] = contextRef.get(tx.peer)

    def auralStarted(server: Server)(implicit tx: Txn): Unit = {
      tx.afterCommit {
        scheduler.cursor.step { implicit tx =>
          auralStartedTx(server)
        }
      }
    }

    private def auralStartedTx(server: Server)(implicit tx: S#Tx): Unit = {
      logT(s"transport - aural-system started")
      import WorkspaceHandle.Implicits._
      implicit val aural = AuralContext(server, scheduler)
      implicit val ptx   = tx.peer
      contextRef.set(Some(aural))
      objSet.foreach { objH =>
        val obj = objH()
        mkView(obj)
      }
      if (isPlaying) playViews()
    }

    def auralStopped()(implicit tx: Txn): Unit = {
      tx.afterCommit {
        scheduler.cursor.step { implicit tx =>
          auralStoppedTx()
        }
      }
    }

    private def auralStoppedTx()(implicit tx: S#Tx): Unit = {
      logT(s"transport - aural-system stopped")
      contextRef.set(None)(tx.peer)
      disposeViews()
    }
  }
}
