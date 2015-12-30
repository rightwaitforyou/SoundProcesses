/*
 *  AuralScheduledBase.scala
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
import de.sciss.lucre.stm.{Disposable, TxnLike}
import de.sciss.lucre.synth.Sys
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.AuralView.{Playing, Prepared, Preparing, Stopped}
import de.sciss.synth.proc.{logAural => logA}

import scala.concurrent.stm.{Ref, TSet}

object AuralScheduledBase {
  final         val LOOK_AHEAD      = (1.0 * TimeRef.SampleRate).toLong  // one second. XXX TODO -- make configurable
  private final val PREP_FRAMES     = (0.5 * TimeRef.SampleRate).toLong  // XXX TODO -- make configurable
  private final val LOOK_STOP       = LOOK_AHEAD + PREP_FRAMES

  private val EmptyScheduled = new Scheduled(-1, Long.MaxValue)

  final class Scheduled(val token: Int, val frame: Long) {
    override def toString = s"[token = $token, frame = $frame / ${TimeRef.framesToSecs(frame)}]"
    def isEmpty: Boolean = token == -1
  }

  @inline
  def spanToPoint(span: SpanLike): LongPoint2D = BiGroupImpl.spanToPoint(span)
}
trait AuralScheduledBase[S <: Sys[S], Target, Elem <: AuralView[S, Target]]
  extends ObservableImpl[S, AuralView.State] { impl =>

  import TxnLike.peer

  // ---- abstract ----

  //  /** A map from the model object's identifier to its view.
  //    * This should be created upon initialization of the sub-class.
  //    * It will be cleared in `dispose`.
  //    */
  //  protected def viewMap: IdentifierMap[S#ID, S#Tx, Elem]

  implicit protected val context: AuralContext[S]

  /** Called during preparation of armed elements. This
    * happens either during initial `prepare` or during grid-events.
    * Given the `prepareSpan`, the sub-class should
    *
    * - find the elements using an `intersect`
    * - for each build a view and store it somewhere
    * - for each view call `prepareChild`
    * - accumulate the results of `prepareChild` into a `Map` that is returned.
    *
    * The map will become part of `IPreparing`. The returned `Boolean` indicates
    * if elements were found (`true`) or not (`false`).
    *
    * @param initial  if `true` this is an initial preparation which means the method
    *                 must include views that start before `prepareSpan` if their span
    *                 overlaps with `prepareSpan`. If `false` this is a follow up from
    *                 `gridReached` and the search must be restricted to views that
    *                 start no earlier than `prepareSpan`.
    */
  protected def processPrepare(prepareSpan: Span, timeRef: TimeRef.Apply, initial: Boolean)
                              (implicit tx: S#Tx): PrepareResult

  /** The result of `processPrepare`.
    *
    * @param async      the views whose preparation is ongoing (view and observer pairs)
    * @param nonEmpty   `true` if at least one view was prepared, `false` otherwise
    */
  protected final class PrepareResult(val async: Map[Elem, Disposable[S#Tx]],
                                      val nonEmpty: Boolean)

  /** Called during `play`. Sub-classes should intersect
    * the current elements and for each of them call `playView`.
    */
  protected def processPlay(timeRef: TimeRef.Apply, target: Target)(implicit tx: S#Tx): Unit

  /** Called when a next interesting frame has been reached.
    * The method should look for and invoke the events such as
    * starting or stopping a view.
    */
  protected def processEvent(play: IPlaying, timeRef: TimeRef.Apply)(implicit tx: S#Tx): Unit

  /** If the sub-type maintains a tree structure for the views, this method
    * should simply clear that structure without performing any additional clean-up on the views.
    */
  protected def clearViewsTree()(implicit tx: S#Tx): Unit

  /** Report the next interesting frame greater than the given frame for which
    * `eventReached` (internal) and `processEvent` will be called.
    * If no such event exists, the method must return `Long.MaxValue`.
    */
  protected def viewEventAfter(frame: Long)(implicit tx: S#Tx): Long

  /** Report the next interesting frame greater than the given frame for which
    * `gridReached` (internal) and `processPrepare` will be called.
    * If no such event exists, the method must return `Long.MaxValue`.
    */
  protected def modelEventAfter(frame: Long)(implicit tx: S#Tx): Long

  /** An opaque type passed into `playView` that may be used by an overriding implementation.
    * Otherwise it may simply be set to `Unit`.
    */
  protected type ViewID

  // ---- impl ----

  import AuralScheduledBase.{LOOK_AHEAD, PREP_FRAMES, LOOK_STOP, Scheduled, EmptyScheduled}

  protected sealed trait InternalState extends Disposable[S#Tx] {
    def external: AuralView.State
  }

  protected case object IStopped extends InternalState {
    def dispose()(implicit tx: S#Tx): Unit = ()
    def external = Stopped
  }

  protected sealed trait ITimedState extends InternalState {
    def timeRef: TimeRef
  }

  protected final class IPreparing(val map: Map[Elem, Disposable[S#Tx]], val timeRef: TimeRef)
    extends ITimedState {

    def copy(map: Map[Elem, Disposable[S#Tx]]): IPreparing = new IPreparing(map, timeRef)

    override def toString = s"IPreparing($map, $timeRef)"

    def dispose()(implicit tx: S#Tx): Unit = map.foreach(_._2.dispose())

    def external = if (map.isEmpty) Prepared else Preparing
  }

  protected final class IPlaying(val wallClock: Long, val timeRef: TimeRef.Apply, val target: Target)
    extends ITimedState {

    override def toString = s"IPlaying($wallClock, $timeRef, $target)"

    def shiftTo(newWallClock: Long): TimeRef.Apply = timeRef.shift(newWallClock - wallClock)

    def dispose()(implicit tx: S#Tx): Unit = ()

    def external = Playing
  }

  import context.{scheduler => sched}

  private[this] val internalRef       = Ref[InternalState](IStopped)
  private[this] val prepareSpanRef    = Ref[Span.HasStart](Span(0L, 0L))

  private[this] val playingViews      = TSet.empty[Elem]
  private[this] val schedEvtToken     = Ref(EmptyScheduled)
  private[this] val schedGridToken    = Ref(EmptyScheduled)

  final def state(implicit tx: S#Tx): AuralView.State = internalRef().external

  protected final def internalState(implicit tx: S#Tx): InternalState = internalRef()
  protected final def internalState_=(value: InternalState)(implicit tx: S#Tx): Unit = internalRef() = value

  final def views(implicit tx: S#Tx): Set[Elem] = playingViews.toSet

  // ---- utility methods for sub-types ----

  /** Should be called from `processPlay`. It calls `play` on the view
    * and adds it to the list of playing views.
    * Note: `timeRef` must already have been updated through
    * appropriate intersection.
    *
    * Sub-classes may override this if they call `super.playView`
    */
  protected def playView(id: ViewID, view: Elem, timeRef: TimeRef, target: Target)
                        (implicit tx: S#Tx): Unit = {
    logA(s"timeline - playView: $view - $timeRef")
    view.play(timeRef, target)
    playingViews.add(view)
    // viewAdded(timed, view)
  }

  /** Should be called from `processEvent` for views that should be
    * stopped and disposed. The caller is responsible for removing
    * the view also from a view-tree if such structure is maintained.
    * NOT: This method ends by calling `viewRemoved`.
    */
  protected def stopView(view: Elem)(implicit tx: S#Tx): Unit = {
    logA(s"timeline - stopView: $view")
    view.stop()
    view.dispose()
    playingViews.remove(view)
    // viewRemoved(view)
  }

  /** Should be called from `processPrepare`. If the child can be instantly
    * prepared, `None` is returned. Otherwise the child is observed until it
    * is prepared and the method returns `Some(elem, observer)` that must
    * be passed back from `processPrepare` (they'll show up in `IPreparing`).
    */
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
  protected final def childPreparedOrRemoved(childView: Elem)(implicit tx: S#Tx): Unit =
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

  // ----

  final def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
    if (state != Stopped) return
    val tForce    = timeRef.force
    val newState  = prepareNoFire(tForce)
    fire(newState.external)
  }

  /* Makes sure no more nodes are playing, then calls `processPrepare`.
   * Finally sets internal state to `IPreparing` and updates `prepareSpanRef`.
   */
  private[this] def prepareNoFire(timeRef: TimeRef.Apply)(implicit tx: S#Tx): InternalState = {
    val startFrame    = timeRef.frame
    val stopFrame     = startFrame + LOOK_STOP
    val prepareSpan   = Span(startFrame, stopFrame)

    // this can happen if `play` is called with a
    // different `timeRef` from what was previously prepared
    /* TTT if (!tree.isEmpty(iSys(tx))) */ freeNodesAndCancelSchedule()

    val prepRes       = processPrepare(prepareSpan, timeRef, initial = true)

    val st            = new IPreparing(prepRes.async, timeRef)
    internalRef()     = st
    prepareSpanRef()  = prepareSpan // if (prepRes.nextStart != Long.MaxValue) Span(startFrame, prepRes.nextStart) else Span.from(startFrame)
    st
  }

  protected final def scheduledEvent()(implicit tx: S#Tx): Scheduled      = schedEvtToken()
  protected final def scheduledGrid ()(implicit tx: S#Tx): Scheduled      = schedGridToken()

  /** Note: the prepare span will always start from current-frame and have
    * a duration of at least `LOOK_STOP`. I.e. during playback it contains the current play position.
    */
  protected final def prepareSpan   ()(implicit tx: S#Tx): Span.HasStart  = prepareSpanRef()

  /** Ensures state is consistent, then checks preparation of children.
    * If all is good, sets internal state to `IPlaying` and calls `processPlay`.
    * Next instructs scheduler and sets external state to `Playing`.
    */
  final def play(timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit = {
    val st = state
    if (st == Playing) return

    val tForce    = timeRef.force
    val frame     = timeRef.frame

    if (st == Stopped || prepareSpanRef() != Span(frame, frame + LOOK_STOP)) prepareNoFire(tForce)

    val st1 = new IPlaying(sched.time, tForce, target)
    internalRef.swap(st1).dispose()

    processPlay(tForce, target)
    scheduleNextEvent(frame)
    scheduleNextGrid (frame)

    fire(Playing)
  }

  /** Calls `eventAfter` to determine the next interesting frame. If that
    * frame exists, schedules the execution of `eventReached`.
    */
  protected final def scheduleNextEvent(currentFrame: Long)(implicit tx: S#Tx): Unit = {
    val targetFrame = viewEventAfter(currentFrame)
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

  /* Processes next interesting event that has been reached. If the internal
   * state is playing, calls `processEvent` followed by `scheduleNextEvent`.
   */
  private[this] def eventReached(frame: Long)(implicit tx: S#Tx): Unit = {
    logA(s"timeline - eventReached($frame)")
    internalState match {
      case play: IPlaying =>
        val tr = play.timeRef.updateFrame(frame)
        processEvent(play, tr)
        scheduleNextEvent(frame)
      case _ =>
    }
  }

  /* Schedules ahead `STEP_GRID` frames to execute `gridReached`. */
  protected final def scheduleNextGrid(currentFrame: Long)(implicit tx: S#Tx): Unit = {
    val modelFrame  = modelEventAfter(currentFrame + LOOK_STOP - 1)
    val targetFrame = if (modelFrame  == Long.MaxValue) Long.MaxValue else modelFrame - LOOK_AHEAD
    val token       = if (targetFrame == Long.MaxValue) -1 else {
      val targetTime  = sched.time + (targetFrame - currentFrame)
      // val targetFrame = currentFrame + STEP_GRID
      logA(s"timeline - scheduleNextGrid($currentFrame) -> $targetFrame")
      sched.schedule(targetTime) { implicit tx =>
        gridReached(frame = targetFrame)
      }
    }
    val oldSched = schedGridToken.swap(new Scheduled(token, targetFrame))
    if (oldSched.token != -1) sched.cancel(oldSched.token)
  }

  /* If internal state is playing, calls `processPrepare` with the new prepare-span.
   * That is `LOOK_AHEAD` ahead of the `frame` we stopped at.
   */
  private[this] def gridReached(frame: Long)(implicit tx: S#Tx): Unit = {
    logA(s"timeline - gridReached($frame)")
    internalState match {
      case play: IPlaying =>
        val startFrame      = frame       + LOOK_AHEAD
        val stopFrame       = startFrame  + PREP_FRAMES
        val prepareSpan     = Span(frame, stopFrame)
        prepareSpanRef()    = prepareSpan
        val searchSpan      = Span(startFrame, stopFrame)
        val tr0             = play.shiftTo(sched.time)
        val prepRes         = processPrepare(searchSpan, tr0, initial = false)
        // XXX TODO -- a refinement could look for eventAfter,
        // however then we need additional fiddling around in
        // `elemAdded` and `elemRemoved`...
        scheduleNextGrid(frame)
        if (prepRes.nonEmpty) {
          logA("...reschedule")
          scheduleNextEvent(frame)
        }

      case _ =>
    }
  }

  final def stop()(implicit tx: S#Tx): Unit = if (state != Stopped) {
    freeNodesAndCancelSchedule()
    fire(Stopped)
  }

  /** Sub-classes may override this and call `super.dispose()`
    * if they wish to free additional observers, e.g. the
    * timeline or grapheme observer.
    */
  def dispose()(implicit tx: S#Tx): Unit =
    freeNodesAndCancelSchedule()

  /* Stops playing views and disposes them. Cancels scheduled actions.
   * Then calls `clearViewsTree`. Puts internal state to `IStopped`.
   */
  private[this] def freeNodesAndCancelSchedule()(implicit tx: S#Tx): Unit = {
    playingViews.foreach { view =>
      stopView(view)
    }
    sched.cancel(schedEvtToken ().token)
    sched.cancel(schedGridToken().token)
    assert(playingViews.isEmpty)
    // playingViews.clear()
    clearViewsTree()

    internalRef.swap(IStopped).dispose()
  }

  // ---- helper methods for elemAdded ----

  protected final def elemAdded(st: ITimedState, vid: ViewID, span: SpanLike)(mkView: => Elem)
                               (implicit tx: S#Tx): Unit =
    st match {
      case prep: IPreparing => elemAddedPrepare(prep,      span)(mkView)
      case play: IPlaying   => elemAddedPlay   (play, vid, span)(mkView)
    }

//  /** This is usually called from the implementation's `elemAdded` to check if a view
//    * should be created now. This method takes care of re-scheduling grid if necessary.
//    * If the method returns `true`, the caller should build the view and then proceed
//    * to `elemAddedPreparePlay`.
//    */
//  protected final def elemAddedHasView(st: ITimedState, span: SpanLike)(implicit tx: S#Tx): Boolean = {
//    val prepS = prepareSpan()
//    span.overlaps(prepS) && {            // we don't need to prepare or play it.
//      if (span.compareStart(prepS.start) == 1) st match {   // but we have to check if we need to reschedule grid.
//        case play: IPlaying =>
//          val oldGrid = scheduledGrid()
//          if (oldGrid.isEmpty || span.compareStart(oldGrid.frame + LOOK_AHEAD) == -1) {
//            val tr0           = play.shiftTo(sched.time)
//            val currentFrame  = tr0.frame
//            scheduleNextGrid(currentFrame)
//          }
//        case _ =>
//      }
//      false
//    }
//  }

//  protected final def elemAddedPreparePlay(st: ITimedState, vid: ViewID, span: SpanLike, childView: Elem)
//                                          (implicit tx: S#Tx): Unit = st match {
//    case prep: IPreparing => elemAddedPrepare(prep,      span, childView)
//    case play: IPlaying   => elemAddedPlay   (play, vid, span, childView)
//  }

  private[this] def elemAddedPrepare(prep: IPreparing, span: SpanLike)(mkView: => Elem)(implicit tx: S#Tx): Unit = {
    val prepS     = prepareSpan()
    if (!span.overlaps(prepS)) return

    val childTime = prep.timeRef.intersect(span)
    val childView = mkView
    val prepOpt   = prepareChild(childView, childTime)
    prepOpt.foreach { case (_, childObs) =>
      val map1      = prep.map + (childView -> childObs)
      val prep1     = prep.copy(map = map1)
      internalState = prep1
      val st0       = prep.external
      if (st0 == Prepared) fire(Preparing)
    }
  }

  private[this] final def elemAddedPlay(play: IPlaying, vid: ViewID, span: SpanLike)(mkView: => Elem)
                                       (implicit tx: S#Tx): Unit = {
    // calculate current frame
    val tr0           = play.shiftTo(sched.time)

    // if we're playing and the element span intersects contains
    // the current frame, play that new element
    val currentFrame  = tr0.frame
    val elemPlays     = span.contains(currentFrame)

    if (elemPlays) {
      val tr1       = tr0.intersect(span)
      val childView = mkView
      playView(vid, childView, tr1, play.target)
    }

    // re-validate the next scheduling position
    val oldEvt      = scheduledEvent() // schedEvtToken()
    val oldEvtFrame = oldEvt.frame
    val schedEvt    = if (elemPlays) {
      // reschedule if the span has a stop and elem.stop < oldTarget
      span match {
        case hs: Span.HasStop => hs.stop < oldEvtFrame
        case _ => false
      }
    } else {
      // reschedule if the span has a start and that start is greater than the current frame,
      // and elem.start < oldTarget
      span match {
        case hs: Span.HasStart => hs.start > currentFrame && hs.start < oldEvtFrame
        case _ => false
      }
    }

    if (schedEvt) {
      logA("...reschedule event")
      scheduleNextEvent(currentFrame)
    }

    val schedGrid = !elemPlays && {
      val prepS = prepareSpan()
      span.compareStart(prepS.start) == 1 && {
        val oldGrid = scheduledGrid()
        oldGrid.isEmpty || span.compareStart(oldGrid.frame + LOOK_AHEAD) == -1
      }
    }

    if (schedGrid) {
      logA("...reschedule grid")
      scheduleNextGrid(currentFrame)
    }
  }
}