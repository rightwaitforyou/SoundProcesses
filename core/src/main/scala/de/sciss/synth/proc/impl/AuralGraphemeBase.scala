/*
 *  AuralGraphemeBase.scala
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
import de.sciss.lucre.data.{SkipList, SkipOctree}
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.geom.{LongPoint2D, LongSpace}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.AuralView.{Prepared, Preparing, Stopped}
import de.sciss.synth.proc.TimeRef.Apply
import de.sciss.synth.proc.{logAural => logA}

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq => Vec}

object AuralGraphemeBase {
  type Leaf[S <: Sys[S], Elem] = Vec[(stm.Source[S#Tx, S#ID], Elem)]

  @inline
  def spanToPoint(span: SpanLike): LongPoint2D = BiGroupImpl.spanToPoint(span)
}
trait AuralGraphemeBase[S <: Sys[S], I <: stm.Sys[I], Target, Elem <: AuralView[S, Target]]
  extends AuralScheduledBase[S, Target, Elem] with ObservableImpl[S, AuralView.State] { impl =>

  import AuralGraphemeBase.spanToPoint
  import context.{scheduler => sched}

  // ---- abstract ----

  def obj: stm.Source[S#Tx, Grapheme[S]]

  // protected def tree: SkipOctree[I, LongSpace.TwoDim, (SpanLike, Vec[(stm.Source[S#Tx, S#ID], Elem)])]
  protected def tree: SkipList.Map[I, Long, Vec[(stm.Source[S#Tx, S#ID], Elem)]]

  protected def iSys: S#Tx => I#Tx

  protected def makeView(obj: Obj[S])(implicit tx: S#Tx): Elem

  // ---- impl ----

  private[this] var grObserver: Disposable[S#Tx] = _

  final def typeID: Int = Grapheme.typeID

  protected type ViewID = Unit

  private type Leaf = (SpanLike, Vec[(stm.Source[S#Tx, S#ID], Elem)])

  protected final def eventAfter(frame: Long)(implicit tx: S#Tx): Long =
    ??? // BiGroupImpl.eventAfter(tree)(frame)(iSys(tx)).getOrElse(Long.MaxValue)

  protected final def processPlay(timeRef: Apply, target: Target)(implicit tx: S#Tx): Unit = {
    val toStart = intersect(timeRef.frame)
    playViews(toStart, timeRef, target)
  }

  @inline
  private[this] def intersect(frame: Long)(implicit tx: S#Tx): Iterator[Leaf] = {
    // tree.floor(frame)
    ??? // BiGroupImpl.intersectTime(tree)(frame)(iSys(tx))
  }

  protected final def processPrepare(prepareSpan: Span, timeRef: Apply)
                                    (implicit tx: S#Tx): (Map[Elem, Disposable[S#Tx]], Boolean) = {
    val gr = obj()

    // search for new elements starting within the look-ahead period
    @tailrec
    def loop(frame: Long, m: Map[Elem, Disposable[S#Tx]], reschedule: Boolean): (Map[Elem, Disposable[S#Tx]], Boolean) =
      gr.ceil(frame) match {
        case Some(entry) =>
          val frame2 = entry.key.value
          if (!prepareSpan.contains(frame2)) (m, reschedule) else {
            val prepObs = prepareFromEntry(timeRef, ???, entry.value)
            loop(frame2 + 1, prepObs.fold(m)(m + _), reschedule = true)
          }
        case None => (m, reschedule)
      }

    loop(prepareSpan.start, Map.empty, reschedule = false)
  }

  private[this] def prepareFromEntry(timeRef: TimeRef.Apply, span: SpanLike, child: Obj[S])
                                    (implicit tx: S#Tx): Option[(Elem, Disposable[S#Tx])] = {
    val childTime = timeRef.intersect(span)
    val sub: Option[(Elem, Disposable[S#Tx])] = if (childTime.span.isEmpty) None else {
      val childView = makeView(child)
      val id        = child.id
      ??? // viewMap.put(id, childView)  // XXX TODO -- yeah, not nice inside a `map`
      // (tx.newHandle(id), childView)
      ??? // tree.add(span -> childViews)(iSys(tx))
      prepareChild(childView, childTime)
    }
    sub
  }

  protected final def clearViewsTree()(implicit tx: S#Tx): Unit =
    ??? // tree.clear()(iSys(tx))

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

    ??? // views.foreach(stopAndDisposeView)
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
        playView((), elem, tr, target)
      }
    }

  // this can be easily implemented with two rectangular range searches
  // return: (things-that-start, things-that-stop)
  @inline
  private[this] def eventsAt(frame: Long)(implicit tx: S#Tx): (Iterator[Leaf], Iterator[Leaf]) =
    ??? // BiGroupImpl.eventsAt(tree)(frame)(iSys(tx))

  def init(tl: Grapheme[S])(implicit tx: S#Tx): this.type = {
    grObserver = tl.changed.react { implicit tx => upd =>
      upd.changes.foreach {
        case Grapheme.Added  (time, entry)    => elemAdded(time, entry.value)
        case Grapheme.Removed(time, entry)    => ??? // elemRemoved(timed.id, span, timed.value)
        case Grapheme.Moved  (timeCh, entry)  =>
          // for simplicity just remove and re-add
          // ; in the future this could be optimized
          // (e.g., not deleting and re-creating the AuralObj)
          ???
//          elemRemoved(timed.id, spanCh.before, timed.value)
          elemAdded(timeCh.now, entry.value)
      }
    }
    this
  }

  private[this] def elemAdded(start: Long, obj: Obj[S])(implicit tx: S#Tx): Unit = {
    val st    = internalState
    if (st.external == Stopped) return
    val gr    = this.obj()
    val stop  = gr.eventAfter(start)
    val span  = stop.fold[SpanLike](Span.from(start))(Span(start, _))

    if (!span.overlaps(prepareSpan())) return

    logA(s"timeline - elemAdded($span, $obj)")

    // Create a view for the element.
    val childView = makeView(obj)

    // because the assertion is with the span overlap
    // that internal state is either preparing or playing,
    // the view will always be stored, either in
    // `IPreparing` (XXX TODO: NOT) or in `playingViews`.
    //
    //    viewMap.put(tid, childView)
    //    tree.transformAt(spanToPoint(span)) { opt =>
    //      // import expr.IdentifierSerializer
    //      val tup       = (tx.newHandle(tid), childView)
    //      val newViews  = opt.fold(span -> Vec(tup)) { case (span1, views) => (span1, views :+ tup) }
    //      Some(newViews)
    //    } (iSys(tx))

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
          views.foreach(stopView) // there ought to be either zero or one view
          playView((), childView, tr1, play.target)
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

  private[this] def elemRemoved(tid: S#ID, span: SpanLike, obj: Obj[S])(implicit tx: S#Tx): Unit = {
    ???
//    viewMap.get(tid).foreach { view =>
//      // finding the object in the view-map implies that it
//      // is currently preparing or playing
//      logA(s"timeline - elemRemoved($span, $obj)")
//      elemRemoved1(tid, span, obj, view)
//    }
  }

  private[this] def elemRemoved1(tid: S#ID, span: SpanLike, obj: Obj[S], childView: Elem)
                                (implicit tx: S#Tx): Unit = {
    // remove view for the element from tree and map
    ??? // viewMap.remove(tid)
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
    ???
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

    stopView(view)
  }

  override def dispose()(implicit tx: S#Tx): Unit = {
    super.dispose()
    grObserver.dispose()
  }
}