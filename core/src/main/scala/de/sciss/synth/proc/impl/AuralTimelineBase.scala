/*
 *  AuralTimelineBase.scala
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

import de.sciss.lucre.bitemp.impl.BiGroupImpl
import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.expr.SpanLikeObj
import de.sciss.lucre.geom.{LongPoint2D, LongSpace}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, IdentifierMap, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.TimeRef.Apply
import de.sciss.synth.proc.{logAural => logA}

import scala.collection.immutable.{IndexedSeq => Vec}

object AuralTimelineBase {
  type Leaf[S <: Sys[S], Elem] = (SpanLike, Vec[(stm.Source[S#Tx, S#ID], Elem)])

  @inline
  def spanToPoint(span: SpanLike): LongPoint2D = BiGroupImpl.spanToPoint(span)

  protected final case class ElemHandle[S <: Sys[S], Elem](idH: stm.Source[S#Tx, S#ID], span: SpanLike, view: Elem)
}
trait AuralTimelineBase[S <: Sys[S], I <: stm.Sys[I], Target, Elem <: AuralView[S, Target]]
  extends AuralScheduledBase[S, Target, Elem] with ObservableImpl[S, AuralView.State] { impl =>

  import AuralTimelineBase.spanToPoint

  // ---- abstract ----

  def obj: stm.Source[S#Tx, Timeline[S]]

  protected def tree: SkipOctree[I, LongSpace.TwoDim, (SpanLike, Vec[(stm.Source[S#Tx, S#ID], Elem)])]

  protected def iSys: S#Tx => I#Tx

  protected def makeViewElem(obj: Obj[S])(implicit tx: S#Tx): Elem

  // ---- impl ----

  protected type ElemHandle = AuralTimelineBase.ElemHandle[S, Elem]

  private[this] def ElemHandle(idH: stm.Source[S#Tx, S#ID], span: SpanLike, view: Elem): ElemHandle =
    new AuralTimelineBase.ElemHandle(idH, span, view)

  protected def elemFromHandle(h: ElemHandle): Elem = h.view

  private[this] var tlObserver: Disposable[S#Tx] = _

  final def typeID: Int = Timeline.typeID

  protected type ViewID = S#ID

  private type Leaf = (SpanLike, Vec[(stm.Source[S#Tx, S#ID], Elem)])

  private[this] var viewMap: IdentifierMap[S#ID, S#Tx, ElemHandle] = _

  protected final def viewEventAfter(frame: Long)(implicit tx: S#Tx): Long =
    BiGroupImpl.eventAfter(tree)(frame)(iSys(tx)).getOrElse(Long.MaxValue)

  protected final def modelEventAfter(frame: Long)(implicit tx: S#Tx): Long =
    obj().eventAfter(frame).getOrElse(Long.MaxValue)

  protected final def processPlay(timeRef: Apply, target: Target)(implicit tx: S#Tx): Unit = {
    val toStart = intersect(timeRef.frame)
    playViews(toStart, timeRef, target)
  }

  @inline
  private[this] def intersect(frame: Long)(implicit tx: S#Tx): Iterator[Leaf] =
    BiGroupImpl.intersectTime(tree)(frame)(iSys(tx))

  protected final def processPrepare(span: Span, timeRef: Apply, initial: Boolean)
                                    (implicit tx: S#Tx): Iterator[PrepareResult] = {
    val tl          = obj()
    // search for new regions starting within the look-ahead period
    val startSpan   = if (initial) Span.until(span.stop) else span
    val stopSpan    = Span.from(span.start)
    val it          = tl.rangeSearch(start = startSpan, stop = stopSpan)
    it.flatMap { case (childSpan, elems) =>
      val childTime = timeRef.intersect(childSpan)
      val sub: Vec[(ViewID, SpanLike, Obj[S])] = if (childTime.span.isEmpty) Vector.empty else {
        elems.map { timed =>
          (timed.id, childSpan, timed.value)
        }
      }
      sub
    }
  }

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

    stopViews(toStop)
    playViews(toStart, timeRef, play.target)
  }

  private[this] def stopViews(it: Iterator[Leaf])(implicit tx: S#Tx): Unit = {
    // logA("timeline - stopViews")
    implicit val itx: I#Tx = iSys(tx)
    // Note: `toList` makes sure the iterator is not
    // invalidated when `removeView` removes element from `tree`!
    if (it.hasNext) it.toList.foreach { case (span, views) =>
      views.foreach { case (idH, view) =>
        stopView(ElemHandle(idH, span, view))
      }
    }
  }

  private[this] def playViews(it: Iterator[Leaf], timeRef: TimeRef.Apply, target: Target)(implicit tx: S#Tx): Unit =
    if (it.hasNext) it.foreach { case (span, views) =>
      val tr = timeRef.intersect(span)
      views.foreach { case (idH, elem) =>
        playView(ElemHandle(idH, span, elem), tr, target)
      }
    }

  // this can be easily implemented with two rectangular range searches
  // return: (things-that-start, things-that-stop)
  @inline
  private[this] def eventsAt(frame: Long)(implicit tx: S#Tx): (Iterator[Leaf], Iterator[Leaf]) =
    BiGroupImpl.eventsAt(tree)(frame)(iSys(tx))

  def init(tl: Timeline[S])(implicit tx: S#Tx): this.type = {
    viewMap     = tx.newInMemoryIDMap[ElemHandle]
    tlObserver  = tl.changed.react { implicit tx => upd =>
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

  final def getView(timed: Timeline.Timed[S])(implicit tx: S#Tx): Option[Elem] =
    viewMap.get(timed.id).map(_.view)

  final def addObject(id: S#ID, span: SpanLikeObj[S], obj: Obj[S])(implicit tx: S#Tx): Unit =
    elemAdded(id, span.value, obj)

  final def removeObject(id: S#ID, span: SpanLikeObj[S], obj: Obj[S])(implicit tx: S#Tx): Unit =
    elemRemoved(id, span.value, obj)

  protected final def mkView(tid: S#ID, span: SpanLike, obj: Obj[S])(implicit tx: S#Tx): ElemHandle = {
    logA(s"timeline - elemAdded($span, $obj)")

    // create a view for the element and add it to the tree and map
    val childView = makeViewElem(obj) // AuralObj(obj)
    val idH       = tx.newHandle(tid)
    val h         = ElemHandle(idH, span, childView)
    viewMap.put(tid, h)
    tree.transformAt(spanToPoint(span)) { opt =>
      // import expr.IdentifierSerializer
      val tup       = (idH, childView)
      val newViews  = opt.fold(span -> Vec(tup)) { case (span1, views) => (span1, views :+ tup) }
      Some(newViews)
    } (iSys(tx))

    // elemAddedPreparePlay(st, tid, span, childView)
    h
  }

  private[this] def elemRemoved(tid: S#ID, span: SpanLike, obj: Obj[S])(implicit tx: S#Tx): Unit =
    viewMap.get(tid).foreach { h =>
      // finding the object in the view-map implies that it
      // is currently preparing or playing
      logA(s"timeline - elemRemoved($span, $obj)")
      elemRemoved(h)
    }

  protected final def checkReschedule(h: ElemHandle, currentFrame: Long, oldTarget: Long, elemPlays: Boolean)
                                     (implicit tx: S#Tx): Boolean =
    if (elemPlays) {
      // reschedule if the span has a stop and elem.stop == oldTarget
      h.span match {
        case hs: Span.HasStop => hs.stop == oldTarget
        case _ => false
      }
    } else {
      // reschedule if the span has a start and that start is greater than the current frame,
      // and elem.start == oldTarget
      h.span match {
        case hs: Span.HasStart => hs.start > currentFrame && hs.start == oldTarget
        case _ => false
      }
    }

  protected final def removeView(h: ElemHandle)(implicit tx: S#Tx): Unit = {
    import h._
    logA(s"timeline - removeView - $span - $view")

    // note: this doesn't have to check for `IPreparing`, as it is called only
    // via `eventReached`, thus during playing. correct?

    // preparingViews.remove(view).foreach(_.dispose())
    tree.transformAt(spanToPoint(span)) { opt =>
      opt.flatMap { case (span1, views) =>
        val i = views.indexWhere(_._2 == view)
        val views1 = if (i >= 0) {
          views.patch(i, Nil, 1)
        } else {
          Console.err.println(s"Warning: timeline - removeView - view for $obj not in tree")
          views
        }
        if (views1.isEmpty) None else Some(span1 -> views1)
      }
    } (iSys(tx))

    viewMap.remove(idH())
  }

  override def dispose()(implicit tx: S#Tx): Unit = {
    super.dispose()
    // this may be the case for `AuralTimelineImpl.empty` where `init` is not called.
    if (tlObserver != null) tlObserver.dispose()
  }
}