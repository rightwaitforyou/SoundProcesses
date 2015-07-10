/*
 *  AuralTimelineImpl.scala
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
package impl

import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.bitemp.impl.BiGroupImpl
import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.geom.LongSpace
import de.sciss.lucre.stm.{Disposable, IdentifierMap}
import de.sciss.lucre.synth.{Sys, expr}
import de.sciss.lucre.{data, stm}
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.{logAural => logA}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{Ref, TSet}

object AuralTimelineImpl {
  private type Leaf[S <: Sys[S]] = (SpanLike, Vec[(stm.Source[S#Tx, S#ID], AuralObj[S])])

  def apply[S <: Sys[S]](tlObj: Timeline.Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline[S] = {
    val system            = tx.system
    val (tree, viewMap, res) = prepare[S, system.I](tlObj, system)

    val tl                = tlObj.elem.peer
    type I                = system.I
    implicit val iSys     = system.inMemoryTx _
    implicit val itx      = iSys(tx)
    //    val dummyEvent        = evt.Dummy[system.I, Unit]
    //    implicit val dummySer = DummySerializerFactory[system.I].dummySerializer[AuralObj[S]]
    //    val map               = BiGroup.Modifiable[system.I, AuralObj[S], Unit](_ => dummyEvent)
    implicit val pointView = (l: Leaf[S], tx: I#Tx) => BiGroupImpl.spanToPoint(l._1)
    implicit val dummyKeySer = DummySerializerFactory[system.I].dummySerializer[Leaf[S]]

    // Note: in the future, we might want to
    // restrict the view build-up to a particular
    // time window. Right now, let's just eagerly
    // create views for all objects
    tl.iterator.foreach {
      case (span, elems) =>
        val views = elems.map { timed =>
          val obj   = timed.value
          val view  = AuralObj(obj)
          viewMap.put(timed.id, view)
          import expr.IdentifierSerializer
          (tx.newHandle(timed.id), view)
        }
        // logA(s"timeline - init. add $span -> $views")
        tree.add(span -> views)
    }

    res.init(tlObj)
    res
  }

  /** An empty view that does not add the timeline's children,
    * nor does it listen for events on the timeline.
    */
  def empty[S <: Sys[S]](tlObj: Timeline.Obj[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline.Manual[S] = {
    val system = tx.system
    val (_, _, res) = prepare[S, system.I](tlObj, system)
    res
  }

  private def prepare[S <: Sys[S], I1 <: stm.Sys[I1]](tlObj: Timeline.Obj[S], system: S { type I = I1 })
                        (implicit tx: S#Tx,
                         context: AuralContext[S]): (SkipOctree[I1, LongSpace.TwoDim, Leaf[S]],
                                                     IdentifierMap[S#ID, S#Tx, AuralObj[S]],
                                                     Impl[S, I1]) = {
    implicit val iSys     = system.inMemoryTx _
    implicit val itx      = iSys(tx)
    implicit val pointView = (l: Leaf[S], tx: I1#Tx) => BiGroupImpl.spanToPoint(l._1)
    implicit val dummyKeySer = DummySerializerFactory[system.I].dummySerializer[Leaf[S]]
    val tree = SkipOctree.empty[I1, LongSpace.TwoDim, Leaf[S]](BiGroup.MaxSquare)

    val viewMap = tx.newInMemoryIDMap[AuralObj[S]]
    val res = new Impl[S, I1](tx.newHandle(tlObj), tree, viewMap)
    (tree, viewMap, res)
  }

  private final class PlayTime(val wallClock: Long, val timeRef: TimeRef.Apply) {
    def shiftTo(newWallClock: Long): TimeRef.Apply = timeRef.shift(newWallClock - wallClock)

    override def toString = s"[wallClock = $wallClock, timeRef = $timeRef]"
  }

  private final class Scheduled(val token: Int, val frame: Long) {
    override def toString = s"[token = $token, frame = $frame / ${TimeRef.framesToSecs(frame)}]"
  }

  private final class Impl[S <: Sys[S], I <: stm.Sys[I]](val obj: stm.Source[S#Tx, Timeline.Obj[S]],
                                                         tree: SkipOctree[I, LongSpace.TwoDim, Leaf[S]],
                                                         viewMap: IdentifierMap[S#ID, S#Tx, AuralObj[S]])
                                                        (implicit context: AuralContext[S], iSys: S#Tx => I#Tx)
    extends AuralObj.Timeline.Manual[S] with ObservableImpl[S, AuralObj.State] { impl =>

    def typeID: Int = Timeline.typeID

    private def sched = context.scheduler

    private val currentStateRef = Ref[AuralObj.State](AuralObj.Stopped)
    private val activeViews     = TSet.empty[AuralObj[S]]
    private var tlObserver: Disposable[S#Tx] = _
    private val schedToken      = Ref(new Scheduled(-1, Long.MaxValue))   // (-1, MaxValue) indicate no scheduled function
    private val playTimeRef     = Ref(new PlayTime(0L, TimeRef(Span.from(0L), 0L)))

    def state(implicit tx: S#Tx): AuralObj.State = currentStateRef.get(tx.peer)

    def views(implicit tx: S#Tx): Set[AuralObj[S]] = activeViews.single.toSet

    object contents extends ObservableImpl[S, AuralObj.Timeline.Update[S]] {
      def viewAdded(timed: S#ID, view: AuralObj[S])(implicit tx: S#Tx): Unit =
        fire(AuralObj.Timeline.ViewAdded(impl, timed, view))

      def viewRemoved(view: AuralObj[S])(implicit tx: S#Tx): Unit =
        fire(AuralObj.Timeline.ViewRemoved(impl, view))
    }

    def getView(timed: Timeline.Timed[S])(implicit tx: S#Tx): Option[AuralObj[S]] = viewMap.get(timed.id)

    private def state_=(value: AuralObj.State)(implicit tx: S#Tx): Unit = {
      val old = currentStateRef.swap(value)(tx.peer)
      if (value != old) {
        // println(s"------TIMELINE STATE $old > $value")
        fire(value)
      }
    }

    def init(tl: Timeline.Obj[S])(implicit tx: S#Tx): Unit = {
      tlObserver = tl.elem.peer.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case Timeline.Added  (span, timed)    => elemAdded  (span, timed)
          case Timeline.Removed(span, timed)    => elemRemoved(span, timed)
          case Timeline.Moved  (timed, spanCh)  =>
            // for simplicity just remove and re-add
            // ; in the future this could be optimized
            // (e.g., not deleting and re-creating the AuralObj)
            elemRemoved(spanCh.before, timed)
            elemAdded  (spanCh.now   , timed)
          case Timeline.Element(_, _) =>  // we don't care
        }
      }
    }

    def addObject   (timed: Timeline.Timed[S])(implicit tx: S#Tx): Unit = elemAdded  (timed.span.value, timed)
    def removeObject(timed: Timeline.Timed[S])(implicit tx: S#Tx): Unit = elemRemoved(timed.span.value, timed)

    private def elemAdded(span: SpanLike, timed: Timeline.Timed[S])(implicit tx: S#Tx): Unit = {
      logA(s"timeline - elemAdded($span, ${timed.value})")
      implicit val itx: I#Tx = iSys(tx)

      // create a view for the element and add it to the tree and map
      val view = AuralObj(timed.value)
      viewMap.put(timed.id, view)
      tree.transformAt(BiGroupImpl.spanToPoint(span)) { opt =>
        import expr.IdentifierSerializer
        val tup       = (tx.newHandle(timed.id), view)
        val newViews  = opt.fold(span -> Vec(tup)) { case (span1, views) => (span1, views :+ tup) }
        Some(newViews)
      }

      // contents.viewAdded(view)

      if (state != AuralObj.Playing) return

      // calculate current frame
      implicit val ptx  = tx.peer
      val pt            = playTimeRef()
      val tr0           = pt.shiftTo(sched.time)
      val currentFrame  = tr0.frame

      // if we're playing and the element span intersects contains
      // the current frame, play that new element
      val elemPlays     = span.contains(currentFrame)

      if (elemPlays) {
        val tr1 = tr0.intersect(span)
        logA(s"...playView: $tr1")
        playView(timed.id, view, tr1)
      }

      // re-validate the next scheduling position
      val oldSched    = schedToken()
      val oldTarget   = oldSched.frame
      val reschedule  = if (elemPlays) {
        // reschedule if the span has a stop and elem.stop < oldTarget
        span match {
          case hs: Span.HasStop => hs.stop < oldTarget
          case _ => false
        }
      } else {
        // reschedule if the span has a start and that start is greater than the current frame,
        // and elem.start < oldTarget
        span match {
          case hs: Span.HasStart => hs.start > currentFrame && hs.start < oldTarget
          case _ => false
        }
      }

      if (reschedule) {
        logA("...reschedule")
        sched.cancel(oldSched.token)
        scheduleNext(currentFrame)
      }
    }

    private def elemRemoved(span: SpanLike, timed: Timeline.Timed[S])(implicit tx: S#Tx): Unit = {
      logA(s"timeline - elemRemoved($span, ${timed.value})")
      implicit val itx: I#Tx = iSys(tx)

      val view = viewMap.get(timed.id).getOrElse {
        Console.err.println(s"Warning: timeline - elemRemoved - no view for $timed found")
        return
      }

      // remove view for the element from tree and map
      viewMap.remove(timed.id)
      tree.transformAt(BiGroupImpl.spanToPoint(span)) { opt =>
        opt.flatMap { case (span1, views) =>
          val i = views.indexWhere(_._2 == view)
          val views1 = if (i >= 0) {
            views.patch(i, Nil, 1)
          } else {
            Console.err.println(s"Warning: timeline - elemRemoved - view for $timed not in tree")
            views
          }
          if (views1.isEmpty) None else Some(span1 -> views1)
        }
      }

      if (state != AuralObj.Playing) return

      // TODO - a bit of DRY re elemAdded
      // calculate current frame
      implicit val ptx  = tx.peer
      val pt            = playTimeRef()
      val tr0           = pt.shiftTo(sched.time)
      val currentFrame  = tr0.frame

      // if we're playing and the element span intersects contains
      // the current frame, play that new element
      val elemPlays     = span.contains(currentFrame)

      if (elemPlays) {
        logA(s"...stopView")
        stopView(view)
      }

      // re-validate the next scheduling position
      val oldSched    = schedToken()
      val oldTarget   = oldSched.frame
      val reschedule  = if (elemPlays) {
        // reschedule if the span has a stop and elem.stop == oldTarget
        span match {
          case hs: Span.HasStop => hs.stop == oldTarget
          case _ => false
        }
      } else {
        // reschedule if the span has a start and that start is greater than the current frame,
        // and elem.start == oldTarget
        span match {
          case hs: Span.HasStart => hs.start > currentFrame && hs.start == oldTarget
          case _ => false
        }
      }

      if (reschedule) {
        logA("...reschedule")
        sched.cancel(oldSched.token)
        scheduleNext(currentFrame)
      }

      // contents.viewRemoved(view)
    }

    def prepare()(implicit tx: S#Tx): Unit = {
      if (state != AuralObj.Stopped) return
      Console.err.println("TODO: AuralTimeline.prepare") // XXX TODO
    }

    def play(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      if (state != AuralObj.Stopped) return

      implicit val ptx = tx.peer
      implicit val itx: I#Tx = iSys(tx)
      playTimeRef() = new PlayTime(sched.time, timeRef.force)
      val frame     = timeRef.offsetOrZero
      val toStart   = intersect(frame)
      playViews(toStart, timeRef)
      scheduleNext(frame)
      state         = AuralObj.Playing
    }

    private def playViews(it: data.Iterator[I#Tx, Leaf[S]], timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      logA("timeline - playViews")
      implicit val itx: I#Tx = iSys(tx)
      if (it.hasNext) it.foreach { case (span, views) =>
        val tr = timeRef.intersect(span)
        views.foreach(view => playView(view._1(), view._2, tr))
      }
    }

    // note: `timeRef` should already have been updated
    private def playView(timed: S#ID, view: AuralObj[S], timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      view.play(timeRef)
      activeViews.add(view)(tx.peer)
      contents.viewAdded(timed, view)
    }

    private def stopViews(it: data.Iterator[I#Tx, Leaf[S]])(implicit tx: S#Tx): Unit = {
      logA("timeline - stopViews")
      implicit val itx: I#Tx = iSys(tx)
      if (it.hasNext) it.foreach { case (span, views) =>
        views.foreach { case (_, view) => stopView(view) }
      }
    }

    private def stopView(view: AuralObj[S])(implicit tx: S#Tx): Unit = {
      view.stop()
      activeViews.remove(view)(tx.peer)
      contents.viewRemoved(view)
    }

    private def scheduleNext(currentFrame: Long)(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      val targetFrame = nearestEventAfter(currentFrame + 1)
      val token = if (targetFrame == Long.MaxValue) -1 else {
        logA(s"timeline - scheduleNext($currentFrame) -> $targetFrame")
        val targetTime = sched.time + (targetFrame - currentFrame)
        sched.schedule(targetTime) { implicit tx =>
          eventReached(frame = targetFrame)
        }
      }
      schedToken() = new Scheduled(token, targetFrame)
    }

    private def eventReached(frame: Long)(implicit tx: S#Tx): Unit = {
      logA(s"timeline - eventReached($frame)")
      implicit val ptx = tx.peer
      val (toStart, toStop) = eventsAt(frame)
      val tr = playTimeRef().timeRef.updateFrame(frame)
      playViews(toStart, tr)
      stopViews(toStop)
      scheduleNext(frame)
    }

    def stop()(implicit tx: S#Tx): Unit = {
      freeNodes()
      state = AuralObj.Stopped
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      if (tlObserver != null) tlObserver.dispose()
      freeNodes()
      // XXX TODO - we really need an iterator for id-map
      // viewMap.foreach { view => contents.fire(AuralObj.Timeline.ViewRemoved(this, view)) }
      viewMap.dispose()
    }

    private def freeNodes()(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      activeViews.foreach { view =>
        view.stop()
        contents.viewRemoved(view)
      }
      sched.cancel(schedToken().token)
      activeViews.clear()
    }

    // ---- bi-group functionality TODO - DRY ----

    private def intersect(frame: Long)(implicit tx: S#Tx): data.Iterator[I#Tx, Leaf[S]] =
      BiGroupImpl.intersectTime(tree)(frame)(iSys(tx))

    // this can be easily implemented with two rectangular range searches
    // return: (things-that-start, things-that-stop)
    private def eventsAt(frame: Long)(implicit tx: S#Tx): (data.Iterator[I#Tx, Leaf[S]], data.Iterator[I#Tx, Leaf[S]]) =
      BiGroupImpl.eventsAt(tree)(frame)(iSys(tx))

    // Long.MaxValue indicates _no event_; frame is inclusive!
    private def nearestEventAfter(frame: Long)(implicit tx: S#Tx): Long =
      BiGroupImpl.nearestEventAfter(tree)(frame)(iSys(tx)).getOrElse(Long.MaxValue)
  }
}