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
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.geom.{LongPoint2D, LongSpace}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, IdentifierMap, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.AuralObj.{Playing, Prepared, Preparing, Stopped}
import de.sciss.synth.proc.{logAural => logA}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{Ref, TMap, TSet}

object AuralTimelineImpl {
  private type Leaf[S <: Sys[S]] = (SpanLike, Vec[(stm.Source[S#Tx, S#ID], AuralObj[S])])

  private final val LOOK_AHEAD  = (1.0 * TimeRef.SampleRate).toLong  // one second. XXX TODO -- make configurable
  private final val STEP_GRID   = (0.5 * TimeRef.SampleRate).toLong  // XXX TODO -- make configurable

  def apply[S <: Sys[S]](tlObj: Timeline[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline[S] = {
    val system  = tx.system
    val res     = prepare[S, system.I](tlObj, system)
    res.init(tlObj)
    res
  }

  /** An empty view that does not listen for events on the timeline. */
  def empty[S <: Sys[S]](tlObj: Timeline[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline.Manual[S] = {
    val system = tx.system
    val res = prepare[S, system.I](tlObj, system)
    res
  }

  private def prepare[S <: Sys[S], I1 <: stm.Sys[I1]](tlObj: Timeline[S], system: S { type I = I1 })
                        (implicit tx: S#Tx, context: AuralContext[S]): Impl[S, I1] = {
    implicit val iSys     = system.inMemoryTx _
    implicit val itx      = iSys(tx)
    implicit val pointView = (l: Leaf[S], tx: I1#Tx) => spanToPoint(l._1)
    implicit val dummyKeySer = DummySerializerFactory[system.I].dummySerializer[Leaf[S]]
    val tree = SkipOctree.empty[I1, LongSpace.TwoDim, Leaf[S]](BiGroup.MaxSquare)

    val viewMap = tx.newInMemoryIDMap[AuralObj[S]]
    val res = new Impl[S, I1](tx.newHandle(tlObj), tree, viewMap)
    res
  }

  private final class PlayTime(val wallClock: Long, val timeRef: TimeRef.Apply) {
    def shiftTo(newWallClock: Long): TimeRef.Apply = timeRef.shift(newWallClock - wallClock)

    override def toString = s"[wallClock = $wallClock, timeRef = $timeRef]"
  }

  private final class Scheduled(val token: Int, val frame: Long) {
    override def toString = s"[token = $token, frame = $frame / ${TimeRef.framesToSecs(frame)}]"
    def isEmpty: Boolean = token == -1
  }

  private final class Impl[S <: Sys[S], I <: stm.Sys[I]](val obj: stm.Source[S#Tx, Timeline[S]],
                                                         tree: SkipOctree[I, LongSpace.TwoDim, Leaf[S]],
                                                         viewMap: IdentifierMap[S#ID, S#Tx, AuralObj[S]])
                                                        (implicit context: AuralContext[S], iSys: S#Tx => I#Tx)
    extends AuralObj.Timeline.Manual[S] with ObservableImpl[S, AuralObj.State] { impl =>

    def typeID: Int = Timeline.typeID

    import context.{scheduler => sched}

    private[this] val currentStateRef   = Ref[AuralObj.State](Stopped)
    private[this] val playingViews      = TSet.empty[AuralObj[S]]
    private[this] val preparingViews    = TMap.empty[AuralObj[S], Disposable[S#Tx]]
    private[this] var tlObserver        = null: Disposable[S#Tx]
    private[this] val schedEvtToken     = Ref(new Scheduled(-1, Long.MaxValue))   // (-1, MaxValue) indicate no scheduled function
    private[this] val schedGridToken    = Ref(new Scheduled(-1, Long.MaxValue))   // (-1, MaxValue) indicate no scheduled function
    private[this] val playTimeRef       = Ref(new PlayTime(0L, TimeRef(Span.from(0L), 0L)))
    private[this] val prepareSpanRef    = Ref(Span(0L, 0L))

//    private[this] val targetStateRef  = Ref[TargetState](TargetStop)

    def state(implicit tx: S#Tx): AuralObj.State = currentStateRef.get(tx.peer)

    def views(implicit tx: S#Tx): Set[AuralObj[S]] = playingViews.single.toSet

    object contents extends ObservableImpl[S, AuralObj.Timeline.Update[S]] {
      def viewAdded(timed: S#ID, view: AuralObj[S])(implicit tx: S#Tx): Unit =
        fire(AuralObj.Timeline.ViewAdded(impl, timed, view))

      def viewRemoved(view: AuralObj[S])(implicit tx: S#Tx): Unit =
        fire(AuralObj.Timeline.ViewRemoved(impl, view))
    }

//    private[this] def targetState(implicit tx: S#Tx): TargetState = targetStateRef.get(tx.peer)
//    private[this] def targetState_=(value: TargetState)(implicit tx: S#Tx): Unit = targetStateRef.set(value)(tx.peer)

    def getView(timed: Timeline.Timed[S])(implicit tx: S#Tx): Option[AuralObj[S]] = viewMap.get(timed.id)

    private[this] def state_=(value: AuralObj.State)(implicit tx: S#Tx): Unit = {
      val old = currentStateRef.swap(value)(tx.peer)
      if (value != old) {
        logA(s"timeline - state = $value")
        // println(s"------TIMELINE STATE $old > $value")
        fire(value)
      }
    }

    def init(tl: Timeline[S])(implicit tx: S#Tx): Unit = {
      tlObserver = tl.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case Timeline.Added  (span, timed)    => elemAdded  (timed.id, span, timed.value)
          case Timeline.Removed(span, timed)    => elemRemoved(timed.id, span, timed.value)
          case Timeline.Moved  (spanCh, timed)  =>
            // for simplicity just remove and re-add
            // ; in the future this could be optimized
            // (e.g., not deleting and re-creating the AuralObj)
            elemRemoved(timed.id, spanCh.before, timed.value)
            elemAdded  (timed.id, spanCh.now   , timed.value)
//          case Timeline.Element(_, _) =>  // we don't care
        }
      }
    }

    def addObject   (id: S#ID, span: Expr[S, SpanLike], obj: Obj[S])(implicit tx: S#Tx): Unit = elemAdded  (id, span.value, obj)
    def removeObject(id: S#ID, span: Expr[S, SpanLike], obj: Obj[S])(implicit tx: S#Tx): Unit = elemRemoved(id, span.value, obj)

    private[this] def elemAdded(tid: S#ID, span: SpanLike, obj: Obj[S])(implicit tx: S#Tx): Unit = {
      val st = state
      if (st == Stopped || !span.overlaps(prepareSpanRef.get(tx.peer))) return

      logA(s"timeline - elemAdded($span, $obj)")

      implicit val ptx  = tx.peer
      implicit val itx  = iSys(tx)

      // create a view for the element and add it to the tree and map
      val view = AuralObj(obj)
      viewMap.put(tid, view)
      tree.transformAt(spanToPoint(span)) { opt =>
        // import expr.IdentifierSerializer
        val tup       = (tx.newHandle(tid), view)
        val newViews  = opt.fold(span -> Vec(tup)) { case (span1, views) => (span1, views :+ tup) }
        Some(newViews)
      }

      // calculate current frame
      val pt            = playTimeRef()
      val tr0           = pt.shiftTo(sched.time)

      if (st != Playing) {
        // i.e. Preparing or Prepared
        val childTime   = tr0.intersect(span)
        val isPrepared  = prepareView(view, childTime)
        if (st == Prepared && !isPrepared) state = Preparing  // go back to that state
        return
      }

      // if we're playing and the element span intersects contains
      // the current frame, play that new element
      val currentFrame  = tr0.frame
      val elemPlays     = span.contains(currentFrame)

      if (elemPlays) {
        val tr1 = tr0.intersect(span)
        playView(tid, view, tr1)
      }

      // re-validate the next scheduling position
      val oldSched    = schedEvtToken()
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
        scheduleNextEvent(currentFrame)
      }
    }

    private[this] def elemRemoved(tid: S#ID, span: SpanLike, obj: Obj[S])(implicit tx: S#Tx): Unit =
      viewMap.get(tid).foreach { view =>
        // finding the object in the view-map implies that it
        // is currently preparing or playing
        logA(s"timeline - elemRemoved($span, $obj)")
        elemRemoved1(tid, span, obj, view)
      }

    private[this] def elemRemoved1(tid: S#ID, span: SpanLike, obj: Obj[S], view: AuralObj[S])
                                  (implicit tx: S#Tx): Unit = {
      implicit val ptx  = tx.peer
      implicit val itx  = iSys(tx)

      // remove view for the element from tree and map
      viewMap.remove(tid)
      stopAndDisposeView(span, view)

      val st = state
      if (st != Playing) {
        if (st == Preparing && preparingViews.isEmpty) state = Prepared // go back to that
        return
      }

      // TODO - a bit of DRY re elemAdded
      // calculate current frame
      val pt            = playTimeRef()
      val tr0           = pt.shiftTo(sched.time)
      val currentFrame  = tr0.frame

      // if we're playing and the element span intersects contains
      // the current frame, play that new element
      val elemPlays     = span.contains(currentFrame)

//      if (elemPlays) {
//        logA("...stopView")
//        stopAndDisposeView(span, view)
//      }

      // re-validate the next scheduling position
      val oldSched    = schedEvtToken()
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
        scheduleNextEvent(currentFrame)
      }

      // contents.viewRemoved(view)
    }

    def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      if (state != Stopped) return
      implicit val ptx = tx.peer
        // targetState   = TargetPrepared
      val tForce    = timeRef.force
      playTimeRef() = new PlayTime(sched.time, tForce)
      prepare1(tForce)
    }

    private[this] def prepare1(timeRef: TimeRef.Apply)(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      implicit val itx = iSys(tx)
      state           = Preparing
      val tl          = obj()
      val startFrame  = timeRef.frame
      val stopFrame   = startFrame + LOOK_AHEAD + STEP_GRID
      val prepareSpan = Span(startFrame, stopFrame)
      prepareSpanRef() = prepareSpan

      // this can happen if `play` is called with a
      // different `timeRef` from what was previously prepared
      if (!tree.isEmpty) freeNodesAndCancelSchedule()

      val it          = tl.intersect(prepareSpan)
      prepare2(timeRef, it)
      if (preparingViews.isEmpty) state = Prepared
    }

    // consumes the iterator
    private[this] def prepare2(timeRef: TimeRef.Apply, it: Iterator[Timeline.Leaf[S]])(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      if (it.nonEmpty) it.foreach { case (span, elems) =>
        val childTime = timeRef.intersect(span)
        if (childTime.span.nonEmpty) {
          val views = elems.map { timed =>
            val child = timed.value
            val view  = AuralObj(child)
            viewMap.put(timed.id, view)
            prepareView(view, childTime)
            (tx.newHandle(timed.id), view)
          }
          tree.add(span -> views)(iSys(tx))
        }
      }
    }

    private[this] def prepareView(view: AuralObj[S], childTime: TimeRef)(implicit tx: S#Tx): Boolean = {
      implicit val ptx = tx.peer
      logA(s"timeline - prepare $view - $childTime")
      view.prepare(childTime)
      val isPrepared = view.state == Prepared
      if (!isPrepared) {
        val childObs = view.react { implicit tx => {
          case Prepared => viewPrepared(view)
          case _ =>
        }}
        preparingViews.put(view, childObs)
      }
      isPrepared
    }

    // called by prepare1 for each child view when it becomes ready
    private[this] def viewPrepared(view: AuralObj[S])(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      preparingViews.remove(view).foreach { obs =>
        obs.dispose()
        val isReady = state == Preparing && preparingViews.isEmpty
        if (isReady) state = Prepared
//          targetState match {
//          case TargetPrepared => state = Prepared
//          case TargetPlaying(wallClock, timeRef) =>
//            val dt = sched.time - wallClock
//            val timeRefNew = if (dt == 0) timeRef else
//              timeRef.intersect(Span.from(timeRef.offsetOrZero + dt))
//            play1(timeRefNew)
//          case _ => // nada
//        }
      }
    }

    def play(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      val st = state
      if (st == Playing) return
      // targetState = TargetPlaying(sched.time, timeRef)

      implicit val ptx = tx.peer
      implicit val itx: I#Tx = iSys(tx)

      val tForce    = timeRef.force
      playTimeRef() = new PlayTime(sched.time, tForce)
      val frame     = timeRef.offsetOrZero
      if (st == Stopped || prepareSpanRef().start != frame) prepare1(tForce)

      val toStart   = intersect(frame)
      playViews(toStart, tForce)
      scheduleNextEvent(frame)
      scheduleNextGrid (frame)
      state         = Playing
    }

    private[this] def playViews(it: Iterator[Leaf[S]], timeRef: TimeRef.Apply)(implicit tx: S#Tx): Unit = {
      // logA("timeline - playViews")
      implicit val itx: I#Tx = iSys(tx)
      if (it.hasNext) it.foreach { case (span, views) =>
        val tr = timeRef.intersect(span)
        views.foreach(view => playView(view._1(), view._2, tr))
      }
    }

    // note: `timeRef` should already have been updated
    private[this] def playView(timed: S#ID, view: AuralObj[S], timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      logA(s"timeline - playView: $timed - $timeRef")
      view.play(timeRef)
      playingViews.add(view)(tx.peer)
      contents.viewAdded(timed, view)
    }

    private[this] def stopAndDisposeViews(it: Iterator[Leaf[S]])(implicit tx: S#Tx): Unit = {
      // logA("timeline - stopViews")
      implicit val itx: I#Tx = iSys(tx)
      // Note: `toList` makes sure the iterator is not
      // invalidated when `stopAndDisposeView` removes element from `tree`!
      if (it.hasNext) it.toList.foreach { case (span, views) =>
        views.foreach { case (_, view) => stopAndDisposeView(span, view) }
      }
    }

    private[this] def stopAndDisposeView(span: SpanLike, view: AuralObj[S])(implicit tx: S#Tx): Unit = {
      logA(s"timeline - stopAndDispose - $span - $view")

      implicit val ptx = tx.peer
      implicit val itx = iSys(tx)
      view.stop()
      view.dispose()
      playingViews  .remove(view)
      preparingViews.remove(view).foreach(_.dispose())
      tree.transformAt(spanToPoint(span)) { opt =>
        opt.flatMap { case (span1, views) =>
          val i = views.indexWhere(_._2 == view)
          val views1 = if (i >= 0) {
            views.patch(i, Nil, 1)
          } else {
            Console.err.println(s"Warning: timeline - elemRemoved - view for $obj not in tree")
            views
          }
          if (views1.isEmpty) None else Some(span1 -> views1)
        }
      }
      contents.viewRemoved(view)
    }

    private[this] def scheduleNextEvent(currentFrame: Long)(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      val targetFrame = eventAfter(currentFrame)
      val token = if (targetFrame == Long.MaxValue) -1 else {
        logA(s"timeline - scheduleNextEvent($currentFrame) -> $targetFrame")
        val targetTime = sched.time + (targetFrame - currentFrame)
        sched.schedule(targetTime) { implicit tx =>
          eventReached(frame = targetFrame)
        }
      }
      val oldSched = schedEvtToken.swap(new Scheduled(token, targetFrame))
      if (oldSched.token != -1) sched.cancel(oldSched.token)
    }

    private[this] def eventReached(frame: Long)(implicit tx: S#Tx): Unit = {
      logA(s"timeline - eventReached($frame)")
      implicit val ptx = tx.peer
      val (toStart, toStop) = eventsAt(frame)
      val tr = playTimeRef().timeRef.updateFrame(frame)
      playViews(toStart, tr)
      stopAndDisposeViews(toStop)
      scheduleNextEvent(frame)
    }

    private[this] def scheduleNextGrid(currentFrame: Long)(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      val targetTime  = sched.time   + STEP_GRID
      val targetFrame = currentFrame + STEP_GRID
      logA(s"timeline - scheduleNextGrid($currentFrame) -> $targetFrame")
      val token       = sched.schedule(targetTime) { implicit tx =>
        gridReached(frame = targetFrame)
      }
      schedGridToken() = new Scheduled(token, targetFrame)
    }

    private[this] def gridReached(frame: Long)(implicit tx: S#Tx): Unit = {
      logA(s"timeline - gridReached($frame)")
      implicit val ptx = tx.peer
      val startFrame  = frame       + LOOK_AHEAD
      val stopFrame   = startFrame  + STEP_GRID
      val prepareSpan = Span(frame, stopFrame)
      prepareSpanRef() = prepareSpan
      val tl          = obj()
      // search for new regions starting within the look-ahead period
      val it          = tl.rangeSearch(start = Span(startFrame, stopFrame), stop = Span.All)
      val pt          = playTimeRef()
      val tr0         = pt.shiftTo(sched.time)
      val reschedule  = it.nonEmpty
      prepare2(tr0, it)
//      println(s"tree.size = ${tree.size(iSys(tx))}")
      // XXX TODO -- a refinement could look for eventAfter,
      // however then we need additional fiddling around in
      // `elemAdded` and `elemRemoved`...
      scheduleNextGrid(frame)
      if (reschedule) {
        logA("...reschedule")
        scheduleNextEvent(frame)
      }
    }

    def stop()(implicit tx: S#Tx): Unit = {
      freeNodesAndCancelSchedule()
      state       = Stopped
      // targetState = TargetStop
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      if (tlObserver != null) tlObserver.dispose()
      freeNodesAndCancelSchedule()
      // XXX TODO - we really need an iterator for id-map
      // viewMap.foreach { view => contents.fire(AuralObj.Timeline.ViewRemoved(this, view)) }
      viewMap.dispose()
    }

    private[this] def freeNodesAndCancelSchedule()(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      implicit val itx = iSys(tx)

      playingViews.foreach { view =>
        view.stop()
        contents.viewRemoved(view)
      }
      sched.cancel(schedEvtToken ().token)
      sched.cancel(schedGridToken().token)
      playingViews   .clear()
      tree           .clear()
      preparingViews .clear()
    }

    // ---- bi-group functionality TODO - DRY ----

    @inline
    private[this] def intersect(frame: Long)(implicit tx: S#Tx): Iterator[Leaf[S]] =
      BiGroupImpl.intersectTime(tree)(frame)(iSys(tx))

    // this can be easily implemented with two rectangular range searches
    // return: (things-that-start, things-that-stop)
    @inline
    private[this] def eventsAt(frame: Long)(implicit tx: S#Tx): (Iterator[Leaf[S]], Iterator[Leaf[S]]) =
      BiGroupImpl.eventsAt(tree)(frame)(iSys(tx))

    // Long.MaxValue indicates _no event_; frame is exclusive!
    @inline
    private[this] def eventAfter(frame: Long)(implicit tx: S#Tx): Long =
      BiGroupImpl.eventAfter(tree)(frame)(iSys(tx)).getOrElse(Long.MaxValue)
  }

  @inline
  private[this] def spanToPoint(span: SpanLike): LongPoint2D = BiGroupImpl.spanToPoint(span)
}