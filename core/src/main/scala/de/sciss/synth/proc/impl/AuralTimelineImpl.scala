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
import de.sciss.lucre.geom.LongSpace
import de.sciss.lucre.stm.{Obj, Disposable, IdentifierMap}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.stm
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.AuralObj.{TargetPlaying, TargetPrepared, TargetStop, TargetState, Playing, Preparing, Prepared, Stopped}
import de.sciss.synth.proc.{logAural => logA}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{Ref, TSet}

object AuralTimelineImpl {
  private type Leaf[S <: Sys[S]] = (SpanLike, Vec[(stm.Source[S#Tx, S#ID], AuralObj[S])])

  private final val LOOK_AHEAD  = (1.0 * Timeline.SampleRate).toLong  // one second. XXX TODO -- make configurable
  private final val STEP_GRID   = (0.5 * Timeline.SampleRate).toLong  // XXX TODO -- make configurable

  def apply[S <: Sys[S]](tlObj: Timeline[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline[S] = {
    val system            = tx.system
    val (tree, viewMap, res) = prepare[S, system.I](tlObj, system)

    val tl                = tlObj
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
          // import expr.IdentifierSerializer
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
  def empty[S <: Sys[S]](tlObj: Timeline[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline.Manual[S] = {
    val system = tx.system
    val (_, _, res) = prepare[S, system.I](tlObj, system)
    res
  }

  private def prepare[S <: Sys[S], I1 <: stm.Sys[I1]](tlObj: Timeline[S], system: S { type I = I1 })
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

  private final class Impl[S <: Sys[S], I <: stm.Sys[I]](val obj: stm.Source[S#Tx, Timeline[S]],
                                                         tree: SkipOctree[I, LongSpace.TwoDim, Leaf[S]],
                                                         viewMap: IdentifierMap[S#ID, S#Tx, AuralObj[S]])
                                                        (implicit context: AuralContext[S], iSys: S#Tx => I#Tx)
    extends AuralObj.Timeline.Manual[S] with ObservableImpl[S, AuralObj.State] { impl =>

    def typeID: Int = Timeline.typeID

    private[this] def sched = context.scheduler

    private[this] val currentStateRef = Ref[AuralObj.State](Stopped)
    private[this] val activeViews     = TSet.empty[AuralObj[S]]
    private[this] var tlObserver: Disposable[S#Tx] = _
    private[this] val schedToken      = Ref(new Scheduled(-1, Long.MaxValue))   // (-1, MaxValue) indicate no scheduled function
    private[this] val playTimeRef     = Ref(new PlayTime(0L, TimeRef(Span.from(0L), 0L)))
    private[this] val targetStateRef  = Ref[TargetState](TargetStop)

    def state(implicit tx: S#Tx): AuralObj.State = currentStateRef.get(tx.peer)

    def views(implicit tx: S#Tx): Set[AuralObj[S]] = activeViews.single.toSet

    object contents extends ObservableImpl[S, AuralObj.Timeline.Update[S]] {
      def viewAdded(timed: S#ID, view: AuralObj[S])(implicit tx: S#Tx): Unit =
        fire(AuralObj.Timeline.ViewAdded(impl, timed, view))

      def viewRemoved(view: AuralObj[S])(implicit tx: S#Tx): Unit =
        fire(AuralObj.Timeline.ViewRemoved(impl, view))
    }

    private[this] def targetState(implicit tx: S#Tx): TargetState = targetStateRef.get(tx.peer)
    private[this] def targetState_=(value: TargetState)(implicit tx: S#Tx): Unit = targetStateRef.set(value)(tx.peer)

    def getView(timed: Timeline.Timed[S])(implicit tx: S#Tx): Option[AuralObj[S]] = viewMap.get(timed.id)

    private[this] def state_=(value: AuralObj.State)(implicit tx: S#Tx): Unit = {
      val old = currentStateRef.swap(value)(tx.peer)
      if (value != old) {
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
      logA(s"timeline - elemAdded($span, $obj)")
      implicit val itx: I#Tx = iSys(tx)

      // create a view for the element and add it to the tree and map
      val view = AuralObj(obj)
      viewMap.put(tid, view)
      tree.transformAt(BiGroupImpl.spanToPoint(span)) { opt =>
        // import expr.IdentifierSerializer
        val tup       = (tx.newHandle(tid), view)
        val newViews  = opt.fold(span -> Vec(tup)) { case (span1, views) => (span1, views :+ tup) }
        Some(newViews)
      }

      // contents.viewAdded(view)

      if (state != Playing) return

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
        playView(tid, view, tr1)
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

    private[this] def elemRemoved(tid: S#ID, span: SpanLike, obj: Obj[S])(implicit tx: S#Tx): Unit = {
      logA(s"timeline - elemRemoved($span, $obj)")
      implicit val itx: I#Tx = iSys(tx)

      val view = viewMap.get(tid).getOrElse {
        Console.err.println(s"Warning: timeline - elemRemoved - no view for $obj found")
        return
      }

      // remove view for the element from tree and map
      viewMap.remove(tid)
      tree.transformAt(BiGroupImpl.spanToPoint(span)) { opt =>
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

      if (state != Playing) return

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

    private[this] val childrenPreparing = TSet.empty[Disposable[S#Tx]]

    def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      if (state != Stopped) return
      targetState = TargetPrepared
      prepare1(timeRef)
    }

    private[this] def prepare1(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      state           = Preparing
      val tl          = obj()
      val startFrame  = timeRef.frame
      val stopFrame   = timeRef.frame + LOOK_AHEAD + STEP_GRID
      val it          = tl.intersect(Span(startFrame, stopFrame))
      assert(tree.isEmpty(iSys(tx)))
      it.foreach { case (span, elems) =>
        val childTime = timeRef.intersect(span)
        val views     = elems.map { timed =>
          val child = timed.value
          val view  = AuralObj(child)
          viewMap.put(timed.id, view)
          view.prepare(childTime)
          if (view.state != Prepared) {
            lazy val childObs: Disposable[S#Tx] = view.react { implicit tx => {
              case Prepared => viewPrepared(view, childObs)
              case _ =>
            }}
            childrenPreparing.add(childObs)
          }
          (tx.newHandle(timed.id), view)
        }
        tree.add(span -> views)(iSys(tx))
      }
      if (childrenPreparing.isEmpty) state = Prepared
    }

    // called by prepare1 for each child view when it becomes ready
    private[this] def viewPrepared(view: AuralObj[S], obs: Disposable[S#Tx])(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      obs.dispose()
      val ok = childrenPreparing.remove(obs)
      if (ok) {
        val isReady = childrenPreparing.isEmpty
        targetState match {
          case TargetPrepared if isReady => state = Prepared
          case tp @ TargetPlaying(wallClock, timeRef) if isReady =>
            val dt = sched.time - wallClock
            val timeRefNew = if (dt == 0) timeRef else
              timeRef.intersect(Span.from(timeRef.offsetOrZero + dt))
            play1(timeRefNew)
          case _ => // nada
        }
      }
    }

    def play(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      targetState = TargetPlaying(sched.time, timeRef)
      state match {
        case Stopped =>
          prepare1(timeRef)
          if (state == Prepared) play1(timeRef)
        case Prepared =>
          play1(timeRef)
        case _ =>
      }
    }

    private[this] def play1(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      implicit val itx: I#Tx = iSys(tx)
      playTimeRef() = new PlayTime(sched.time, timeRef.force)
      val frame     = timeRef.offsetOrZero
      val toStart   = intersect(frame)
      playViews(toStart, timeRef)
      scheduleNext(frame)
      state         = Playing
    }

    private[this] def playViews(it: Iterator[Leaf[S]], timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      logA("timeline - playViews")
      implicit val itx: I#Tx = iSys(tx)
      if (it.hasNext) it.foreach { case (span, views) =>
        val tr = timeRef.intersect(span)
        views.foreach(view => playView(view._1(), view._2, tr))
      }
    }

    // note: `timeRef` should already have been updated
    private[this] def playView(timed: S#ID, view: AuralObj[S], timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      view.play(timeRef)
      activeViews.add(view)(tx.peer)
      contents.viewAdded(timed, view)
    }

    private[this] def stopViews(it: Iterator[Leaf[S]])(implicit tx: S#Tx): Unit = {
      logA("timeline - stopViews")
      implicit val itx: I#Tx = iSys(tx)
      if (it.hasNext) it.foreach { case (span, views) =>
        views.foreach { case (_, view) => stopView(view) }
      }
    }

    private[this] def stopView(view: AuralObj[S])(implicit tx: S#Tx): Unit = {
      view.stop()
      activeViews.remove(view)(tx.peer)
      contents.viewRemoved(view)
    }

    private[this] def scheduleNext(currentFrame: Long)(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      val targetFrame = eventAfter(currentFrame)
      val token = if (targetFrame == Long.MaxValue) -1 else {
        logA(s"timeline - scheduleNext($currentFrame) -> $targetFrame")
        val targetTime = sched.time + (targetFrame - currentFrame)
        sched.schedule(targetTime) { implicit tx =>
          eventReached(frame = targetFrame)
        }
      }
      schedToken() = new Scheduled(token, targetFrame)
    }

    private[this] def eventReached(frame: Long)(implicit tx: S#Tx): Unit = {
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
      state       = Stopped
      targetState = TargetStop
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      if (tlObserver != null) tlObserver.dispose()
      freeNodes()
      // XXX TODO - we really need an iterator for id-map
      // viewMap.foreach { view => contents.fire(AuralObj.Timeline.ViewRemoved(this, view)) }
      viewMap.dispose()
    }

    private[this] def freeNodes()(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      activeViews.foreach { view =>
        view.stop()
        contents.viewRemoved(view)
      }
      sched.cancel(schedToken().token)
      activeViews.clear()
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
}