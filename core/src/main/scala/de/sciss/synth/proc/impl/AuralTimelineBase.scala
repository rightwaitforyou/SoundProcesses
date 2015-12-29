/*
 *  AuralTimelineBase.scala
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
import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.expr.SpanLikeObj
import de.sciss.lucre.geom.{LongPoint2D, LongSpace}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{IdentifierMap, Disposable, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.AuralView.{Preparing, Prepared, Stopped}
import de.sciss.synth.proc.TimeRef.Apply
import de.sciss.synth.proc.{logAural => logA}

import scala.collection.immutable.{IndexedSeq => Vec}

object AuralTimelineBase {
  type Leaf[S <: Sys[S], Elem] = (SpanLike, Vec[(stm.Source[S#Tx, S#ID], Elem)])

  @inline
  def spanToPoint(span: SpanLike): LongPoint2D = BiGroupImpl.spanToPoint(span)
}
trait AuralTimelineBase[S <: Sys[S], I <: stm.Sys[I], Target, Elem <: AuralView[S, Target]]
  extends AuralScheduledBase[S, Target, Elem] with ObservableImpl[S, AuralView.State] { impl =>

  import AuralTimelineBase.spanToPoint
  import context.{scheduler => sched}

  // ---- abstract ----

  def obj: stm.Source[S#Tx, Timeline[S]]

  protected def tree: SkipOctree[I, LongSpace.TwoDim, (SpanLike, Vec[(stm.Source[S#Tx, S#ID], Elem)])]

  protected def iSys: S#Tx => I#Tx

  protected def makeView(obj: Obj[S])(implicit tx: S#Tx): Elem

  protected def viewMap: IdentifierMap[S#ID, S#Tx, Elem]

  /** A notification method that may be used to `fire` an event
    * such as `AuralObj.Timeline.ViewAdded`.
    */
  protected def viewAdded(timed: S#ID, view: Elem)(implicit tx: S#Tx): Unit

  /** A notification method that may be used to `fire` an event
    * such as `AuralObj.Timeline.ViewRemoved`.
    */
  protected def viewRemoved(view: Elem)(implicit tx: S#Tx): Unit

  // ---- impl ----

  private[this] var tlObserver: Disposable[S#Tx] = _

  final def typeID: Int = Timeline.typeID

  protected type ViewID = S#ID

  private type Leaf = (SpanLike, Vec[(stm.Source[S#Tx, S#ID], Elem)])

  override protected def stopView(view: Elem)(implicit tx: S#Tx): Unit = {
    super.stopView(view)
    viewRemoved(view)
  }

  override protected def playView(id: S#ID, view: Elem, timeRef: TimeRef, target: Target)
                                 (implicit tx: S#Tx): Unit = {
    super.playView(id, view, timeRef, target)
    viewAdded(id, view)
  }

  protected final def viewEventAfter(frame: Long)(implicit tx: S#Tx): Long =
    BiGroupImpl.eventAfter(tree)(frame)(iSys(tx)).getOrElse(Long.MaxValue)

  protected final def processPlay(timeRef: Apply, target: Target)(implicit tx: S#Tx): Unit = {
    val toStart = intersect(timeRef.frame)
    playViews(toStart, timeRef, target)
  }

  @inline
  private[this] def intersect(frame: Long)(implicit tx: S#Tx): Iterator[Leaf] =
    BiGroupImpl.intersectTime(tree)(frame)(iSys(tx))

  protected final def processPrepare(span: Span, timeRef: Apply, initial: Boolean)
                                    (implicit tx: S#Tx): (Map[Elem, Disposable[S#Tx]], Boolean) = {
    val tl          = obj()
    // search for new regions starting within the look-ahead period
    val startSpan   = if (initial) Span.until(span.stop) else span
    val stopSpan    = Span.from(span.start)
    val it          = tl.rangeSearch(start = startSpan, stop = stopSpan)
    val reschedule  = it.nonEmpty
    val prepObs     = prepareFromIterator(timeRef, it)
    (prepObs, reschedule)
  }

  // consumes the iterator
  private[this] def prepareFromIterator(timeRef: TimeRef.Apply, it: Iterator[Timeline.Leaf[S]])
                                       (implicit tx: S#Tx): Map[Elem, Disposable[S#Tx]] =
    it.flatMap { case (span, elems) =>
      val childTime = timeRef.intersect(span)
      val sub: Vec[(Elem, Disposable[S#Tx])] = if (childTime.span.isEmpty) Vector.empty else {
        val childViews = elems.map { timed =>
          val child     = timed.value
          val childView = makeView(child)
          viewMap.put(timed.id, childView)  // XXX TODO -- yeah, not nice inside a `map`
          (tx.newHandle(timed.id), childView)
        }
        tree.add(span -> childViews)(iSys(tx))
        childViews.flatMap { case (_, childView) =>
          prepareChild(childView, childTime)
        } // (breakOut)
      }
      sub
    } .toMap // (breakOut)

  protected final def clearViewsTree()(implicit tx: S#Tx): Unit =
    tree.clear()(iSys(tx))

  protected final def processEvent(play: IPlaying, timeRef: Apply)(implicit tx: S#Tx): Unit = {
    val (toStart, toStop) = eventsAt(timeRef.frame)

    // this is a pretty tricky decision...
    // do we first free the stopped views and then launch the started ones?
    // or vice versa?
    //
    // we stick now to stop-then-start because it seems advantageous
    // for aural-attr-target as we don't build up unnecessary temporary
    // attr-set/attr-map synths. however, I'm not sure this doesn't
    // cause a problem where the stop action schedules on immediate
    // bundle and the start action requires a sync'ed bundle? or is
    // this currently prevented automatically? we might have to
    // reverse this decision.

    //        playViews(toStart, tr, play.target)
    //        stopAndDisposeViews(toStop)

    stopAndDisposeViews(toStop)
    playViews(toStart, timeRef, play.target)
  }

  private[this] def stopAndDisposeViews(it: Iterator[Leaf])(implicit tx: S#Tx): Unit = {
    // logA("timeline - stopViews")
    implicit val itx: I#Tx = iSys(tx)
    // Note: `toList` makes sure the iterator is not
    // invalidated when `stopAndDisposeView` removes element from `tree`!
    if (it.hasNext) it.toList.foreach { case (span, views) =>
      views.foreach { case (_, view) => stopAndDisposeView(span, view) }
    }
  }

  private[this] def playViews(it: Iterator[Leaf], timeRef: TimeRef.Apply, target: Target)(implicit tx: S#Tx): Unit =
    if (it.hasNext) it.foreach { case (span, views) =>
      val tr = timeRef.intersect(span)
      views.foreach { case (idH, elem) =>
        playView(idH(), elem, tr, target)
      }
    }

  // this can be easily implemented with two rectangular range searches
  // return: (things-that-start, things-that-stop)
  @inline
  private[this] def eventsAt(frame: Long)(implicit tx: S#Tx): (Iterator[Leaf], Iterator[Leaf]) =
    BiGroupImpl.eventsAt(tree)(frame)(iSys(tx))

  def init(tl: Timeline[S])(implicit tx: S#Tx): this.type = {
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
      }
    }
    this
  }

  final def getView(timed: Timeline.Timed[S])(implicit tx: S#Tx): Option[Elem] = viewMap.get(timed.id)

  final def addObject(id: S#ID, span: SpanLikeObj[S], obj: Obj[S])(implicit tx: S#Tx): Unit =
    elemAdded(id, span.value, obj)

  final def removeObject(id: S#ID, span: SpanLikeObj[S], obj: Obj[S])(implicit tx: S#Tx): Unit =
    elemRemoved(id, span.value, obj)

  private[this] def elemAdded(tid: S#ID, span: SpanLike, obj: Obj[S])(implicit tx: S#Tx): Unit = {
    val st = internalState
    if (st.external == Stopped || !span.overlaps(prepareSpan())) return

    logA(s"timeline - elemAdded($span, $obj)")

    // create a view for the element and add it to the tree and map
    val childView = makeView(obj) // AuralObj(obj)
    viewMap.put(tid, childView)
    tree.transformAt(spanToPoint(span)) { opt =>
      // import expr.IdentifierSerializer
      val tup       = (tx.newHandle(tid), childView)
      val newViews  = opt.fold(span -> Vec(tup)) { case (span1, views) => (span1, views :+ tup) }
      Some(newViews)
    } (iSys(tx))

    st match {
      case prep: IPreparing =>
        val childTime = prep.timeRef.intersect(span)
        val prepOpt   = prepareChild(childView, childTime)
        prepOpt.foreach { case (_, childObs) =>
          val map1      = prep.map + (childView -> childObs)
          val prep1     = prep.copy(map = map1)
          internalState = prep1
          val st0       = prep.external
          if (st0 == Prepared) fire(Preparing)
        }

      case play: IPlaying =>
        // calculate current frame
        val tr0           = play.shiftTo(sched.time)

        // if we're playing and the element span intersects contains
        // the current frame, play that new element
        val currentFrame  = tr0.frame
        val elemPlays     = span.contains(currentFrame)

        if (elemPlays) {
          val tr1 = tr0.intersect(span)
          playView(tid, childView, tr1, play.target)
        }

        // re-validate the next scheduling position
        val oldSched    = scheduledEvent() // schedEvtToken()
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

      case _ => assert(false, st)
    }
  }

  private[this] def elemRemoved(tid: S#ID, span: SpanLike, obj: Obj[S])(implicit tx: S#Tx): Unit =
    viewMap.get(tid).foreach { view =>
      // finding the object in the view-map implies that it
      // is currently preparing or playing
      logA(s"timeline - elemRemoved($span, $obj)")
      elemRemoved1(tid, span, obj, view)
    }

  private[this] def elemRemoved1(tid: S#ID, span: SpanLike, obj: Obj[S], childView: Elem)
                                (implicit tx: S#Tx): Unit = {
    // remove view for the element from tree and map
    viewMap.remove(tid)
    stopAndDisposeView(span, childView)
    internalState match {
      case _: IPreparing =>
        childPreparedOrRemoved(childView) // might change state to `Prepared`

      case play: IPlaying =>
        // TODO - a bit of DRY re elemAdded
        // calculate current frame
        val tr0           = play.shiftTo(sched.time)
        val currentFrame  = tr0.frame

        // if we're playing and the element span intersects contains
        // the current frame, play that new element
        val elemPlays     = span.contains(currentFrame)

        // re-validate the next scheduling position
        val oldSched    = scheduledEvent()
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

      case _ =>
    }
  }

  private[this] def stopAndDisposeView(span: SpanLike, view: Elem)(implicit tx: S#Tx): Unit = {
    logA(s"timeline - stopAndDispose - $span - $view")

    // note: this doesn't have to check for `IPreparing`, as it is called only
    // via `eventReached`, thus during playing. correct?

    // preparingViews.remove(view).foreach(_.dispose())
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
    } (iSys(tx))

    stopView(view)
  }

  override def dispose()(implicit tx: S#Tx): Unit = {
    super.dispose()
    tlObserver.dispose()
  }
}