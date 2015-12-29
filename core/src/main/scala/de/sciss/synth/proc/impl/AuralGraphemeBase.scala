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

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.AuralView.{Prepared, Preparing, Stopped}
import de.sciss.synth.proc.TimeRef.Apply
import de.sciss.synth.proc.{logAural => logA}

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq => Vec}

trait AuralGraphemeBase[S <: Sys[S], I <: stm.Sys[I], Target, Elem <: AuralView[S, Target]]
  extends AuralScheduledBase[S, Target, Elem] with ObservableImpl[S, AuralView.State] { impl =>

  import context.{scheduler => sched}

  // ---- abstract ----

  def obj: stm.Source[S#Tx, Grapheme[S]]

  // protected def tree: SkipOctree[I, LongSpace.TwoDim, (SpanLike, Vec[(stm.Source[S#Tx, S#ID], Elem)])]
  protected def tree: SkipList.Map[I, Long, Vec[Elem]]

  protected def iSys: S#Tx => I#Tx

  protected def makeView(obj: Obj[S])(implicit tx: S#Tx): Elem

  // ---- impl ----

  private[this] var grObserver: Disposable[S#Tx] = _

  final def typeID: Int = Grapheme.typeID

  protected type ViewID = Unit

  protected final def eventAfter(frame: Long)(implicit tx: S#Tx): Long =
    tree.ceil(frame + 1)(iSys(tx)).fold(Long.MaxValue)(_._1)

  protected final def processPlay(timeRef: Apply, target: Target)(implicit tx: S#Tx): Unit = {
    implicit val itx = iSys(tx)
    tree.floor(timeRef.frame).foreach { case (startTime, entries) =>
      val toStart   = entries.head
      val span      = tree.ceil(startTime + 1).fold[SpanLike](Span.from(startTime)) { case (stopTime, _) =>
        Span(startTime, stopTime  )
      }
      val tr0       = timeRef.intersect(span)
      playView1(toStart, tr0, target)
    }
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
      val childView   = makeView(child)
      val childViews  = Vector.empty :+ childView
      tree.add(childTime.frame -> childViews)(iSys(tx))
      prepareChild(childView, childTime)
    }
    sub
  }

  protected final def clearViewsTree()(implicit tx: S#Tx): Unit =
    tree.clear()(iSys(tx))

  protected final def processEvent(play: IPlaying, timeRef: Apply)(implicit tx: S#Tx): Unit = {
    val toStart = tree.get(timeRef.frame)(iSys(tx))
      .getOrElse(throw new IllegalStateException(s"No element at event ${timeRef.frame}"))
      .head
    playView1(toStart, timeRef, play.target)
  }

//  private[this] def stopAndDisposeViews(it: Iterator[Leaf])(implicit tx: S#Tx): Unit = {
//    // logA("timeline - stopViews")
//    implicit val itx: I#Tx = iSys(tx)
//    // Note: `toList` makes sure the iterator is not
//    // invalidated when `stopAndDisposeView` removes element from `tree`!
//    if (it.hasNext) it.toList.foreach { case (span, views) =>
//      views.foreach { case (_, view) => stopAndDisposeView(span, view) }
//    }
//  }
//
//  private[this] def playViews(it: Iterator[Leaf], timeRef: TimeRef.Apply, target: Target)(implicit tx: S#Tx): Unit =
//    if (it.hasNext) it.foreach { case (span, views) =>
//      val tr = timeRef.intersect(span)
//      views.foreach { case (idH, elem) =>
//        playView((), elem, tr, target)
//      }
//    }

  def init(tl: Grapheme[S])(implicit tx: S#Tx): this.type = {
    grObserver = tl.changed.react { implicit tx => upd =>
      upd.changes.foreach {
        case Grapheme.Added  (time, entry)    => elemAdded  (time, entry.value)
        case Grapheme.Removed(time, entry)    => elemRemoved(time, entry.value)
        case Grapheme.Moved  (timeCh, entry)  =>
          // for simplicity just remove and re-add
          // ; in the future this could be optimized
          // (e.g., not deleting and re-creating the AuralObj)
          elemRemoved(timeCh.before, entry.value)
          elemAdded  (timeCh.now   , entry.value)
      }
    }
    this
  }

  private[this] def playView1(view: Elem, timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit = {
    views.foreach(stopView) // there ought to be either zero or one view
    playView((), view, timeRef, target)
  }

  private[this] def elemAdded(start: Long, child: Obj[S])(implicit tx: S#Tx): Unit = {
    val st    = internalState
    if (st.external == Stopped) return
    val gr    = this.obj()
    val stop  = gr.eventAfter(start)
    val span  = stop.fold[SpanLike](Span.from(start))(Span(start, _))

    if (!span.overlaps(prepareSpan())) return

    logA(s"timeline - elemAdded($span, $child)")

    // Create a view for the element.
    val childView = makeView(child)

    // because the assertion is with the span overlap
    // that internal state is either preparing or playing,
    // the view will always be stored, either in
    // `IPreparing` (XXX TODO: NOT) or in `playingViews`.
    //
    //    viewMap.put(tid, childView)
    val oldEntries = tree.get(start)(iSys(tx)).getOrElse(Vector.empty)
    val newEntries = oldEntries :+ childView
    tree.add(start -> newEntries)(iSys(tx))

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
          playView1(childView, tr1, play.target)
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

  private[this] def elemRemoved(start: Long, child: Obj[S])(implicit tx: S#Tx): Unit = {
    views.find(_.obj() == child).foreach { view =>
      // finding the object in the view-map implies that it
      // is currently preparing or playing
      logA(s"timeline - elemRemoved($start, $child)")
      elemRemoved1(start, child, view)
    }
  }

  private[this] def elemRemoved1(start: Long, child: Obj[S], childView: Elem)
                                (implicit tx: S#Tx): Unit = {
    // remove view for the element from tree and map
    ??? // viewMap.remove(tid)
    val span: SpanLike = ???
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