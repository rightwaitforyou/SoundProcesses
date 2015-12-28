/*
 *  AuralScheduledImpl.scala
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

import de.sciss.lucre.bitemp.impl.BiGroupImpl
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.geom.LongPoint2D
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, IdentifierMap, Obj, TxnLike}
import de.sciss.lucre.synth.Sys
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.AuralView.{Playing, Prepared, Preparing, Stopped}
import de.sciss.synth.proc.{logAural => logA}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{Ref, TSet}

object AuralScheduledBase {
  type Leaf[S <: Sys[S], Elem] = (SpanLike, Vec[(stm.Source[S#Tx, S#ID], Elem)])

  private final val LOOK_AHEAD  = (1.0 * TimeRef.SampleRate).toLong  // one second. XXX TODO -- make configurable
  private final val STEP_GRID   = (0.5 * TimeRef.SampleRate).toLong  // XXX TODO -- make configurable

  private final class Scheduled(val token: Int, val frame: Long) {
    override def toString = s"[token = $token, frame = $frame / ${TimeRef.framesToSecs(frame)}]"
    def isEmpty: Boolean = token == -1
  }

  @inline
  def spanToPoint(span: SpanLike): LongPoint2D = BiGroupImpl.spanToPoint(span)
}
trait AuralScheduledBase[S <: Sys[S], I <: stm.Sys[I], Target, Elem <: AuralView[S, Target]]
  extends /* AuralObj.Scheduled.Manual[S] with */ ObservableImpl[S, AuralView.State] { impl =>

  import TxnLike.peer

  //  private[this] implicit def itx(implicit tx: S#Tx): I#Tx = iSys(tx)

  private type Leaf = (SpanLike, Vec[(stm.Source[S#Tx, S#ID], Elem)])

  // ---- abstract ----

// TTT
//  def obj: stm.Source[S#Tx, Scheduled[S]]
//
//  protected def tree: SkipOctree[I, LongSpace.TwoDim, (SpanLike, Vec[(stm.Source[S#Tx, S#ID], Elem)])]

  protected def viewMap: IdentifierMap[S#ID, S#Tx, Elem]

  implicit protected val context: AuralContext[S]

  protected def iSys: S#Tx => I#Tx

  protected def makeView   (obj: Obj[S]                 )(implicit tx: S#Tx): Elem

  //  protected def prepareView(view: Elem, timeRef: TimeRef)(implicit tx: S#Tx): Unit
  //  protected def playView   (view: Elem, timeRef: TimeRef)(implicit tx: S#Tx): Unit
  //  protected def stopView   (view: Elem                  )(implicit tx: S#Tx): Unit

  protected def viewAdded  (timed: S#ID, view: Elem     )(implicit tx: S#Tx): Unit
  protected def viewRemoved(             view: Elem     )(implicit tx: S#Tx): Unit

  protected def processEvent  (timeRef: TimeRef.Apply                )(implicit tx: S#Tx): Unit
  protected def processPlay   (timeRef: TimeRef.Apply, target: Target)(implicit tx: S#Tx): Unit
  protected def processPrepare(timeRef: TimeRef.Apply                )(implicit tx: S#Tx): Map[Elem, Disposable[S#Tx]]
  protected def processGrid   (searchSpan: Span)(implicit tx: S#Tx): Boolean

  protected def clearViewsTree()(implicit tx: S#Tx): Unit

  protected def eventAfter(frame: Long)(implicit tx: S#Tx): Long

  // ---- impl ----

  import AuralScheduledBase.{LOOK_AHEAD, STEP_GRID, Scheduled}

  protected sealed trait InternalState extends Disposable[S#Tx] {
    def external: AuralView.State
  }

  protected object IStopped extends InternalState {
    def dispose()(implicit tx: S#Tx): Unit = ()
    def external = Stopped
  }

  protected final class IPreparing(val map: Map[Elem, Disposable[S#Tx]], val timeRef: TimeRef)
    extends InternalState {

    def copy(map: Map[Elem, Disposable[S#Tx]]): IPreparing = new IPreparing(map, timeRef)

    override def toString = s"IPreparing($map, $timeRef)"

    def dispose()(implicit tx: S#Tx): Unit = map.foreach(_._2.dispose())

    def external = if (map.isEmpty) Prepared else Preparing
  }

  protected final class IPlaying(val wallClock: Long, val timeRef: TimeRef.Apply, val target: Target)
    extends InternalState {

    override def toString = s"IPlaying($wallClock, $timeRef, $target)"

    def shiftTo(newWallClock: Long): TimeRef.Apply = timeRef.shift(newWallClock - wallClock)

    def dispose()(implicit tx: S#Tx): Unit = ()

    def external = Playing
  }

  // TTT final def typeID: Int = Scheduled.typeID

  import context.{scheduler => sched}

  private[this] val internalRef       = Ref[InternalState](IStopped)
  private[this] val prepareSpanRef    = Ref(Span(0L, 0L))

  private[this] val playingViews      = TSet.empty[Elem]
// TTT
//  private[this] var tlObserver        = null: Disposable[S#Tx]
  private[this] val schedEvtToken     = Ref(new Scheduled(-1, Long.MaxValue))   // (-1, MaxValue) indicate no scheduled function
  private[this] val schedGridToken    = Ref(new Scheduled(-1, Long.MaxValue))   // (-1, MaxValue) indicate no scheduled function

  final def state(implicit tx: S#Tx): AuralView.State = internalRef().external

  protected final def internalState(implicit tx: S#Tx): InternalState = internalRef()

  final def views(implicit tx: S#Tx): Set[Elem] = playingViews.single.toSet

// TTT
//  final def getView(timed: Scheduled.Timed[S])(implicit tx: S#Tx): Option[Elem] = viewMap.get(timed.id)

// TTT
//  def init(tl: Scheduled[S])(implicit tx: S#Tx): this.type = {
//    tlObserver = tl.changed.react { implicit tx => upd =>
//      upd.changes.foreach {
//        case Scheduled.Added  (span, timed)    => elemAdded  (timed.id, span, timed.value)
//        case Scheduled.Removed(span, timed)    => elemRemoved(timed.id, span, timed.value)
//        case Scheduled.Moved  (spanCh, timed)  =>
//          // for simplicity just remove and re-add
//          // ; in the future this could be optimized
//          // (e.g., not deleting and re-creating the AuralObj)
//          elemRemoved(timed.id, spanCh.before, timed.value)
//          elemAdded  (timed.id, spanCh.now   , timed.value)
//      }
//    }
//    this
//  }

// TTT
//  final def addObject   (id: S#ID, span: Expr[S, SpanLike], obj: Obj[S])(implicit tx: S#Tx): Unit =
//    elemAdded  (id, span.value, obj)
//
//  final def removeObject(id: S#ID, span: Expr[S, SpanLike], obj: Obj[S])(implicit tx: S#Tx): Unit =
//    elemRemoved(id, span.value, obj)

// TTT
//  private[this] def elemAdded(tid: S#ID, span: SpanLike, obj: Obj[S])(implicit tx: S#Tx): Unit = {
//    val st = internalState
//    if (st.external == Stopped || !span.overlaps(prepareSpanRef())) return
//
//    logA(s"timeline - elemAdded($span, $obj)")
//
//    // create a view for the element and add it to the tree and map
//    val childView = makeView(obj) // AuralObj(obj)
//    viewMap.put(tid, childView)
//    tree.transformAt(spanToPoint(span)) { opt =>
//      // import expr.IdentifierSerializer
//      val tup       = (tx.newHandle(tid), childView)
//      val newViews  = opt.fold(span -> Vec(tup)) { case (span1, views) => (span1, views :+ tup) }
//      Some(newViews)
//    } (iSys(tx))
//
//    st match {
//      case prep: IPreparing =>
//        val childTime = prep.timeRef.intersect(span)
//        val prepOpt   = prepareChild(childView, childTime)
//        prepOpt.foreach { case (_, childObs) =>
//          val map1      = prep.map + (childView -> childObs)
//          val prep1     = prep.copy(map = map1)
//          internalRef() = prep1
//          val st0       = prep.external
//          if (st0 == Prepared) fire(Preparing)
//        }
//
//      case play: IPlaying =>
//        // calculate current frame
//        val tr0           = play.shiftTo(sched.time)
//
//        // if we're playing and the element span intersects contains
//        // the current frame, play that new element
//        val currentFrame  = tr0.frame
//        val elemPlays     = span.contains(currentFrame)
//
//        if (elemPlays) {
//          val tr1 = tr0.intersect(span)
//          playView(tid, childView, tr1, play.target)
//        }
//
//        // re-validate the next scheduling position
//        val oldSched    = schedEvtToken()
//        val oldTarget   = oldSched.frame
//        val reschedule  = if (elemPlays) {
//          // reschedule if the span has a stop and elem.stop < oldTarget
//          span match {
//            case hs: Span.HasStop => hs.stop < oldTarget
//            case _ => false
//          }
//        } else {
//          // reschedule if the span has a start and that start is greater than the current frame,
//          // and elem.start < oldTarget
//          span match {
//            case hs: Span.HasStart => hs.start > currentFrame && hs.start < oldTarget
//            case _ => false
//          }
//        }
//
//        if (reschedule) {
//          logA("...reschedule")
//          scheduleNextEvent(currentFrame)
//        }
//
//      case _ => assert(false, st)
//    }
//  }

// TTT
//  private[this] def elemRemoved(tid: S#ID, span: SpanLike, obj: Obj[S])(implicit tx: S#Tx): Unit =
//    viewMap.get(tid).foreach { view =>
//      // finding the object in the view-map implies that it
//      // is currently preparing or playing
//      logA(s"timeline - elemRemoved($span, $obj)")
//      elemRemoved1(tid, span, obj, view)
//    }

// TTT
//  private[this] def elemRemoved1(tid: S#ID, span: SpanLike, obj: Obj[S], childView: Elem)
//                                (implicit tx: S#Tx): Unit = {
//    // remove view for the element from tree and map
//    viewMap.remove(tid)
//    stopAndDisposeView(span, childView)
//    internalState match {
//      case _: IPreparing =>
//        childPreparedOrRemoved(childView) // might change state to `Prepared`
//
//      case play: IPlaying =>
//        // TODO - a bit of DRY re elemAdded
//        // calculate current frame
//        val tr0           = play.shiftTo(sched.time)
//        val currentFrame  = tr0.frame
//
//        // if we're playing and the element span intersects contains
//        // the current frame, play that new element
//        val elemPlays     = span.contains(currentFrame)
//
//        // re-validate the next scheduling position
//        val oldSched    = schedEvtToken()
//        val oldTarget   = oldSched.frame
//        val reschedule  = if (elemPlays) {
//          // reschedule if the span has a stop and elem.stop == oldTarget
//          span match {
//            case hs: Span.HasStop => hs.stop == oldTarget
//            case _ => false
//          }
//        } else {
//          // reschedule if the span has a start and that start is greater than the current frame,
//          // and elem.start == oldTarget
//          span match {
//            case hs: Span.HasStart => hs.start > currentFrame && hs.start == oldTarget
//            case _ => false
//          }
//        }
//
//        if (reschedule) {
//          logA("...reschedule")
//          scheduleNextEvent(currentFrame)
//        }
//
//      case _ =>
//    }
//  }

  final def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
    if (state != Stopped) return
    val tForce    = timeRef.force
    val newState  = prepareNoFire(tForce)
    fire(newState.external)
  }

  private[this] def prepareNoFire(timeRef: TimeRef.Apply)(implicit tx: S#Tx): InternalState = {
// TTT
//    val tl            = obj()
    val startFrame    = timeRef.frame
    val stopFrame     = startFrame + LOOK_AHEAD + STEP_GRID
    val prepareSpan   = Span(startFrame, stopFrame)

    // this can happen if `play` is called with a
    // different `timeRef` from what was previously prepared
    /* TTT if (!tree.isEmpty(iSys(tx))) */ freeNodesAndCancelSchedule()

// TTT
//    val it            = tl.intersect(prepareSpan)
    val prepObs: Map[Elem, Disposable[S#Tx]] = processPrepare(timeRef) // prepareFromIterator(timeRef, it)

    val st            = new IPreparing(prepObs, timeRef)
    internalRef()     = st
    prepareSpanRef()  = prepareSpan
    st
  }

// TTT
//  // consumes the iterator
//  private[this] def prepareFromIterator(timeRef: TimeRef.Apply, it: Iterator[Scheduled.Leaf[S]])
//                                       (implicit tx: S#Tx): Map[Elem, Disposable[S#Tx]] =
//    it.flatMap { case (span, elems) =>
//      val childTime = timeRef.intersect(span)
//      val sub: Vec[(Elem, Disposable[S#Tx])] = if (childTime.span.isEmpty) Vector.empty else {
//        val childViews = elems.map { timed =>
//          val child     = timed.value
//          val childView = makeView(child)
//          viewMap.put(timed.id, childView)  // XXX TODO -- yeah, not nice inside a `map`
//          (tx.newHandle(timed.id), childView)
//        }
//        tree.add(span -> childViews)(iSys(tx))
//        childViews.flatMap { case (_, childView) =>
//          prepareChild(childView, childTime)
//        } // (breakOut)
//      }
//      sub
//    } .toMap // (breakOut)

  protected final def prepareChild(childView: Elem, childTime: TimeRef)
                                  (implicit tx: S#Tx): Option[(Elem, Disposable[S#Tx])] = {
    logA(s"timeline - prepare $childView - $childTime")
    childView.prepare(childTime)
    val isPrepared = childView.state == Prepared
    if (isPrepared) None else {
      val childObs = childView.react { implicit tx => {
        case Prepared => childPreparedOrRemoved(childView)
        case _        =>
      }}
      Some(childView -> childObs)
    }
  }

  // called by `prepareChild` for each child view when it becomes ready
  private[this] def childPreparedOrRemoved(childView: Elem)(implicit tx: S#Tx): Unit =
    internalState match {
      case prep: IPreparing =>
        prep.map.get(childView).foreach { obs =>
          obs.dispose()
          val map1      = prep.map - childView
          val prep1     = prep.copy(map = map1)
          internalRef() = prep1
          val st = prep1.external
          if (st == Prepared) fire(Prepared)
        }
      case _ =>
    }

  def play(timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit = {
    val st = state
    if (st == Playing) return

    val tForce    = timeRef.force
    val frame     = timeRef.frame

    if (st == Stopped || prepareSpanRef() != Span(frame, frame + LOOK_AHEAD + STEP_GRID)) prepareNoFire(tForce)

    val st1 = new IPlaying(sched.time, tForce, target)
    internalRef.swap(st1).dispose()

    processPlay(tForce, target)
    scheduleNextEvent(frame)
    scheduleNextGrid (frame)

    fire(Playing)
  }

// TTT
//  private[this] def playViews(it: Iterator[Leaf], timeRef: TimeRef.Apply, target: Target)(implicit tx: S#Tx): Unit =
//    if (it.hasNext) it.foreach { case (span, views) =>
//      val tr = timeRef.intersect(span)
//      views.foreach(view => playView(view._1(), view._2, tr, target))
//    }

  // note: `timeRef` should already have been updated
  protected final def playView(timed: S#ID, view: Elem, timeRef: TimeRef, target: Target)
                              (implicit tx: S#Tx): Unit = {
    logA(s"timeline - playView: $timed - $timeRef")
    view.play(timeRef, target)
    playingViews.add(view)
    viewAdded(timed, view)
  }

// TTT
//  private[this] def stopAndDisposeViews(it: Iterator[Leaf])(implicit tx: S#Tx): Unit = {
//    // logA("timeline - stopViews")
//    implicit val itx: I#Tx = iSys(tx)
//    // Note: `toList` makes sure the iterator is not
//    // invalidated when `stopAndDisposeView` removes element from `tree`!
//    if (it.hasNext) it.toList.foreach { case (span, views) =>
//      views.foreach { case (_, view) => stopAndDisposeView(span, view) }
//    }
//  }

// TTT
//  private[this] def stopAndDisposeView(span: SpanLike, view: Elem)(implicit tx: S#Tx): Unit = {
//    logA(s"timeline - stopAndDispose - $span - $view")
//
//    // note: this doesn't have to check for `IPreparing`, as it is called only
//    // via `eventReached`, thus during playing. correct?
//
//    view.stop()
//    view.dispose()
//
//    playingViews  .remove(view)
//    // preparingViews.remove(view).foreach(_.dispose())
//    tree.transformAt(spanToPoint(span)) { opt =>
//      opt.flatMap { case (span1, views) =>
//        val i = views.indexWhere(_._2 == view)
//        val views1 = if (i >= 0) {
//          views.patch(i, Nil, 1)
//        } else {
//          Console.err.println(s"Warning: timeline - elemRemoved - view for $obj not in tree")
//          views
//        }
//        if (views1.isEmpty) None else Some(span1 -> views1)
//      }
//    } (iSys(tx))
//    viewRemoved(view)
//  }

  private[this] def scheduleNextEvent(currentFrame: Long)(implicit tx: S#Tx): Unit = {
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
    internalState match {
      case play: IPlaying =>
        val tr = play.timeRef.updateFrame(frame)
        processEvent(tr)
        scheduleNextEvent(frame)
      case _ =>
    }
  }

  private[this] def scheduleNextGrid(currentFrame: Long)(implicit tx: S#Tx): Unit = {
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
    internalState match {
      case play: IPlaying =>
        val startFrame  = frame       + LOOK_AHEAD
        val stopFrame   = startFrame  + STEP_GRID
        val prepareSpan = Span(frame, stopFrame)
        prepareSpanRef() = prepareSpan
        val searchSpan  = Span(startFrame, stopFrame)
        val reschedule  = processGrid(searchSpan)
// TTT
//        val tl          = obj()
//        // search for new regions starting within the look-ahead period
//        val it          = tl.rangeSearch(start = searchSpan, stop = Span.All)
//        val tr0         = play.shiftTo(sched.time)
//        val reschedule  = it.nonEmpty
//        prepareFromIterator(tr0, it)
        //      println(s"tree.size = ${tree.size(iSys(tx))}")
        // XXX TODO -- a refinement could look for eventAfter,
        // however then we need additional fiddling around in
        // `elemAdded` and `elemRemoved`...
        scheduleNextGrid(frame)
        if (reschedule) {
          logA("...reschedule")
          scheduleNextEvent(frame)
        }

      case _ =>
    }
  }

  def stop()(implicit tx: S#Tx): Unit = if (state != Stopped) {
    freeNodesAndCancelSchedule()
    fire(Stopped)
  }

  def dispose()(implicit tx: S#Tx): Unit = {
// TTT
//    if (tlObserver != null) tlObserver.dispose()
    freeNodesAndCancelSchedule()
    // XXX TODO - we really need an iterator for id-map
    // viewMap.foreach { view => contents.fire(AuralObj.Scheduled.ViewRemoved(this, view)) }
    viewMap.dispose()
  }

  private[this] def freeNodesAndCancelSchedule()(implicit tx: S#Tx): Unit = {
    // implicit val itx = iSys(tx)

    playingViews.foreach { view =>
      view.stop() // stopView   (view) // view.stop()
      viewRemoved(view) // contents.viewRemoved(view)
    }
    sched.cancel(schedEvtToken ().token)
    sched.cancel(schedGridToken().token)
    playingViews   .clear()
// TTT
//    tree           .clear()(iSys(tx))
    clearViewsTree()

    internalRef.swap(IStopped).dispose()
  }

  // ---- bi-group functionality TODO - DRY ----

// TTT
//  @inline
//  private[this] def intersect(frame: Long)(implicit tx: S#Tx): Iterator[Leaf] =
//    BiGroupImpl.intersectTime(tree)(frame)(iSys(tx))

// TTT
//  // this can be easily implemented with two rectangular range searches
//  // return: (things-that-start, things-that-stop)
//  @inline
//  private[this] def eventsAt(frame: Long)(implicit tx: S#Tx): (Iterator[Leaf], Iterator[Leaf]) =
//    BiGroupImpl.eventsAt(tree)(frame)(iSys(tx))
}