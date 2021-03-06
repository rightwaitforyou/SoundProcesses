/*
 *  TransportImpl.scala
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

package de.sciss.synth.proc
package impl

import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{TxnLike, Obj, IdentifierMap}
import de.sciss.lucre.synth.{Txn, Server, Sys}
import de.sciss.span.Span
import de.sciss.synth.proc
import de.sciss.synth.proc.Transport.AuralStarted
import proc.{logTransport => logT}

import scala.concurrent.stm.{TSet, Ref}

object TransportImpl {
  def apply[S <: Sys[S]](auralSystem: AuralSystem, scheduler: Scheduler[S])
                        (implicit tx: S#Tx, workspace: WorkspaceHandle[S]): Transport[S] = {
    val res = mkTransport(Some(auralSystem), scheduler)
    auralSystem.addClient(res)
    auralSystem.serverOption.foreach { server =>
      implicit val auralContext = AuralContext(server, scheduler)
      res.auralStartedTx(server)
    }
    res
  }

  def apply[S <: Sys[S]](implicit tx: S#Tx, context: AuralContext[S]): Transport[S] = {
    import context.workspaceHandle
    val res = mkTransport(None, context.scheduler)
    res.auralStartedTx(context.server)
    res
  }

  private def mkTransport[S <: Sys[S]](auralSystem: Option[AuralSystem], scheduler: Scheduler[S])
                                      (implicit tx: S#Tx, workspace: WorkspaceHandle[S]): Impl[S] = {
    val objMap  = tx.newInMemoryIDMap[stm.Source[S#Tx, Obj[S]]]
    val viewMap = tx.newInMemoryIDMap[AuralObj[S]]
    // (new Throwable).printStackTrace()
    new Impl(auralSystem, scheduler, objMap, viewMap)
  }

  private final class Impl[S <: Sys[S]](auralSystem: Option[AuralSystem], val scheduler: Scheduler[S],
                                        objMap : IdentifierMap[S#ID, S#Tx, stm.Source[S#Tx, Obj[S]]],
                                        viewMap: IdentifierMap[S#ID, S#Tx, AuralObj[S]])
                                       (implicit workspace: WorkspaceHandle[S])
    extends Transport[S] with ObservableImpl[S, Transport.Update[S]] with AuralSystem.Client {

    import scheduler.cursor
    import TxnLike.peer

    private final class PlayTime(val wallClock0: Long, val pos0: Long) {
      override def toString = s"[pos0 = ${TimeRef.framesAndSecs(pos0)}, time0 = $wallClock0]"

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
    private[this] val objSet  = TSet.empty[stm.Source[S#Tx, Obj[S]]]
    private[this] val viewSet = TSet.empty[AuralObj[S]]

    private[this] val timeBaseRef = Ref(new PlayTime(wallClock0 = Long.MinValue, pos0 = 0L))

    def views(implicit tx: S#Tx): Set[AuralObj[S]] = viewSet.single.toSet

    def getView    (obj: Obj[S])(implicit tx: S#Tx): Option[AuralObj[S]] = getViewById(obj.id)
    def getViewById(id : S#ID  )(implicit tx: S#Tx): Option[AuralObj[S]] = viewMap.get(id)

    def play()(implicit tx: S#Tx): Unit = {
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
      viewSet.foreach(_.play(tr, ()))
    }

    def stop()(implicit tx: S#Tx): Unit = {
      val timeBase0 = timeBaseRef()
      if (!timeBase0.isPlaying) return

      val timeBase1 = timeBase0.stop()
      timeBaseRef() = timeBase1
      logT(s"transport - stop - $timeBase1")

      stopViews()
      fire(Transport.Stop(this, timeBase1.pos0))
    }

    private def stopViews()(implicit tx: S#Tx): Unit =
      viewSet.foreach(_.stop())

    def position(implicit tx: S#Tx): Long = timeBaseRef().currentPos

    def seek(position: Long)(implicit tx: S#Tx): Unit = if (this.position != position) {
      val p = isPlaying
      if (p) stopViews()

      val timeBase1 = new PlayTime(wallClock0 = if (p) scheduler.time else Long.MinValue, pos0 = position)
      timeBaseRef() = timeBase1
      logT(s"transport - seek - $timeBase1")

      if (p) playViews()
      fire(Transport.Seek(this, timeBase1.pos0, isPlaying = p))
    }

    def isPlaying(implicit tx: S#Tx): Boolean = timeBaseRef().isPlaying

    def addObject(obj: Obj[S])(implicit tx: S#Tx): Unit = {
      val id = obj.id
      if (objMap.contains(id)) throw new IllegalArgumentException(s"Object $obj was already added to transport")
      val objH = tx.newHandle(obj)
      objMap.put(id, objH)
      objSet.add(objH)
      fire(Transport.ObjectAdded(this, obj))

      contextOption.foreach { implicit context =>
        val view = mkView(obj)
        if (isPlaying) view.play(mkTimeRef(), ())
      }
    }

    def removeObject(obj: Obj[S])(implicit tx: S#Tx): Unit = {
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
        fire(Transport.ViewRemoved(this, view))
      }

      fire(Transport.ObjectRemoved(this, obj))
    }

    private def mkTimeRef()(implicit tx: S#Tx) = TimeRef(Span.from(0L), offset = position)

    private def mkView(obj: Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = {
      val view = AuralObj(obj)
      viewMap.put(obj.id, view)
      viewSet.add(view)
      fire(Transport.ViewAdded(this, view))
      view
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      auralSystem.foreach(_.removeClient(this))
      objMap.dispose()
      objSet.foreach { obj =>
        fire(Transport.ObjectRemoved(this, obj()))
      }
      objSet.clear()
      disposeViews()
    }

    private def disposeViews()(implicit tx: S#Tx): Unit = {
      viewMap.dispose()
      viewSet.foreach { view =>
        fire(Transport.ViewRemoved(this, view))
        view.dispose()
      }
      viewSet.clear()
    }

    // ---- aural system ----

    private[this] val contextRef = Ref(Option.empty[AuralContext[S]])

    def contextOption(implicit tx: S#Tx): Option[AuralContext[S]] = contextRef()

    def auralStarted(server: Server)(implicit tx: Txn): Unit = {
      // XXX TODO -- what was the reasoning for the txn decoupling?
      // (perhaps the discrepancy between Txn and S#Tx ?)
      tx.afterCommit {
        SoundProcesses.atomic { implicit tx: S#Tx =>
          implicit val auralContext = AuralContext(server, scheduler)
          auralStartedTx(server)
        }
      }
    }

    def auralStartedTx(server: Server)(implicit tx: S#Tx, auralContext: AuralContext[S]): Unit = {
      logT(s"transport - aural-system started")
      contextRef.set(Some(auralContext))
      fire(AuralStarted(this, auralContext))
      objSet.foreach { objH =>
        val obj = objH()
        mkView(obj)
      }
      if (isPlaying) playViews()
    }

    def auralStopped()(implicit tx: Txn): Unit =
      tx.afterCommit {
        SoundProcesses.atomic { implicit tx: S#Tx =>
          auralStoppedTx()
        }
      }

    private def auralStoppedTx()(implicit tx: S#Tx): Unit = {
      logT(s"transport - aural-system stopped")
      contextRef() = None
      disposeViews()
    }
  }
}
