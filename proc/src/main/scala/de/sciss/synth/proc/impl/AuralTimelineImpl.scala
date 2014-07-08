/*
 *  AuralTimelineImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.geom.{LongDistanceMeasure2D, LongRectangle, LongPoint2D, LongSquare, LongSpace}
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.stm
import de.sciss.lucre.synth.Sys
import de.sciss.span.{Span, SpanLike}
import de.sciss.lucre.data
import de.sciss.synth.proc.{logAural => logA}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{TSet, Ref}

object AuralTimelineImpl {
  private val MAX_SQUARE  = LongSquare(0, 0, 0x2000000000000000L)
  private val MIN_COORD   = MAX_SQUARE.left
  private val MAX_COORD   = MAX_SQUARE.right
  private val MAX_SIDE    = MAX_SQUARE.side

  private type Leaf[S <: Sys[S]] = (SpanLike, Vec[AuralObj[S]])

  // XXX TODO - DRY - large overlap with BiGroupImpl
  private def spanToPoint(span: SpanLike): LongPoint2D = span match {
    case Span(start, stop)  => LongPoint2D(start,     stop     )
    case Span.From(start)   => LongPoint2D(start,     MAX_COORD)
    case Span.Until(stop)   => LongPoint2D(MIN_COORD, stop     )
    case Span.All           => LongPoint2D(MIN_COORD, MAX_COORD)
    case Span.Void          => LongPoint2D(MAX_COORD, MIN_COORD) // ?? what to do with this case ?? forbid?
  }

  //  private def searchSpanToPoint(span: SpanLike): LongPoint2D = span match {
  //    case Span(start, stop)  => LongPoint2D(start,     stop         )
  //    case Span.From(start)   => LongPoint2D(start,     MAX_COORD + 1)
  //    case Span.Until(stop)   => LongPoint2D(MIN_COORD, stop         )
  //    case Span.All           => LongPoint2D(MIN_COORD, MAX_COORD + 1)
  //    case Span.Void          => LongPoint2D(MAX_COORD, MIN_COORD    ) // ?? what to do with this case ?? forbid?
  //  }

  // ... accepted are points with x > LRP || y > LRP ...
  private val advanceNNMetric = LongDistanceMeasure2D.nextSpanEvent(MAX_SQUARE)
  // private val regressNNMetric = LongDistanceMeasure2D.prevSpanEvent(MAX_SQUARE)

  def apply[S <: Sys[S]](tlObj: Timeline.Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline[S] = {
    val tl                = tlObj.elem.peer
    val system            = tx.system
    type I                = system.I
    implicit val iSys     = system.inMemoryTx _
    implicit val itx      = iSys(tx)
    //    val dummyEvent        = evt.Dummy[system.I, Unit]
    //    implicit val dummySer = DummySerializerFactory[system.I].dummySerializer[AuralObj[S]]
    //    val map               = BiGroup.Modifiable[system.I, AuralObj[S], Unit](_ => dummyEvent)
    implicit val pointView = (l: Leaf[S], tx: I#Tx) => spanToPoint(l._1)
    implicit val dummyKeySer = DummySerializerFactory[system.I].dummySerializer[Leaf[S]]
    val tree = SkipOctree.empty[I, LongSpace.TwoDim, Leaf[S]](MAX_SQUARE)

    // Note: in the future, we might want to
    // restrict the view build-up to a particular
    // time window. Right now, let's just eagerly
    // create views for all objects
    tl.iterator.foreach {
      case (span, elems) =>
        val views = elems.map { timed =>
          val obj = timed.value
          AuralObj(obj)
        }
        // logA(s"timeline - init. add $span -> $views")
        tree.add(span -> views)
    }

    val res = new Impl[S, I](tx.newHandle(tlObj), tree)
    res.init(tlObj)
    res
  }

  private final class Impl[S <: Sys[S], I <: stm.Sys[I]](val obj: stm.Source[S#Tx, Timeline.Obj[S]],
                                                         tree: SkipOctree[I, LongSpace.TwoDim, Leaf[S]])
                                                        (implicit context: AuralContext[S], iSys: S#Tx => I#Tx)
    extends AuralObj.Timeline[S] with ObservableImpl[S, AuralObj.State] {

    def typeID: Int = Timeline.typeID

    private def sched = context.scheduler

    private val currentStateRef = Ref[AuralObj.State](AuralObj.Stopped)
    private val activeViews     = TSet.empty[AuralObj[S]]
    private var tlObserver: Disposable[S#Tx] = _
    private val schedToken      = Ref(-1)   // -1 indicates no scheduled function

    def state(implicit tx: S#Tx): AuralObj.State = currentStateRef.get(tx.peer)

    private def state_=(value: AuralObj.State)(implicit tx: S#Tx): Unit = {
      val old = currentStateRef.swap(value)(tx.peer)
      if (value != old) fire(value)
    }

    def init(tl: Timeline.Obj[S])(implicit tx: S#Tx): Unit = {
      tlObserver = tl.elem.peer.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case Timeline.Added  (span, timed)    => elemAdded  (span, timed)
          case Timeline.Removed(span, timed)    => elemRemoved(span, timed)
          case Timeline.Moved  (timed, spanCh)  =>
            elemRemoved(spanCh.before, timed)
            elemAdded  (spanCh.now   , timed)
          case Timeline.Element(_, _) =>  // we don't care
        }
      }
    }

    private def elemAdded(span: SpanLike, timed: Timeline.Timed[S])(implicit tx: S#Tx): Unit = {
      logA(s"timeline - elemAdded($span, ${timed.value})")
      implicit val itx: I#Tx = iSys(tx)
      val view = AuralObj(timed.value)
      tree.transformAt(spanToPoint(span)) { opt =>
        val newViews = opt.fold(span -> Vec(view)) { case (span1, views) => (span1, views :+ view) }
        Some(newViews)
      }
      if (state == AuralObj.Playing) {
        logA("--todo: elemAdded -> play--")
      }
    }

    private def elemRemoved(span: SpanLike, timed: Timeline.Timed[S])(implicit tx: S#Tx): Unit = {
      logA(s"timeline - elemRemoved($span, ${timed.value})")
      logA("--todo--")
    }

    def play()(implicit tx: S#Tx): Unit = {
      if (state != AuralObj.Stopped) return

      implicit val ptx = tx.peer
      implicit val itx: I#Tx = iSys(tx)
      val frame = 0L    // XXX TODO - eventually should be provided by the `play` method
      val toStart = intersect(frame)
      playViews(toStart)
      scheduleNext(frame)
      state = AuralObj.Playing
    }

    private def playViews(it: data.Iterator[I#Tx, Leaf[S]])(implicit tx: S#Tx): Unit = {
      logA("timeline - playViews")
      implicit val itx: I#Tx = iSys(tx)
      if (it.hasNext) it.foreach { case (span, views) =>
        views.foreach { view =>
          view.play()
          activeViews.add(view)(tx.peer)
        }
      }
    }

    private def stopViews(it: data.Iterator[I#Tx, Leaf[S]])(implicit tx: S#Tx): Unit = {
      logA("timeline - stopViews")
      implicit val itx: I#Tx = iSys(tx)
      if (it.hasNext) it.foreach { case (span, views) =>
        views.foreach { view =>
          view.stop()
          activeViews.remove(view)(tx.peer)
        }
      }
    }

    private def scheduleNext(currentFrame: Long)(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      val targetFrame = nearestEventAfter(currentFrame + 1)
      if (targetFrame != Long.MinValue) {
        logA(s"timeline - scheduleNext($currentFrame) -> $targetFrame")
        val targetTime = sched.time + (targetFrame - currentFrame)
        schedToken() = sched.schedule(targetTime) { implicit tx =>
          eventReached(frame = targetFrame)
        }
      }
    }

    private def eventReached(frame: Long)(implicit tx: S#Tx): Unit = {
      logA(s"timeline - eventReached($frame)")
      val (toStart, toStop) = eventsAt(frame)
      playViews(toStart)
      stopViews(toStop )
      scheduleNext(frame)
    }

    def stop()(implicit tx: S#Tx): Unit = {
      freeNodes()
      state = AuralObj.Stopped
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      tlObserver.dispose()
      freeNodes()
    }

    private def freeNodes()(implicit tx: S#Tx): Unit = {
      implicit val ptx = tx.peer
      activeViews.foreach(_.stop())
      sched.cancel(schedToken())
      clearSet(activeViews)
    }

    private def clearSet[A](s: TSet[A])(implicit tx: S#Tx): Unit =
      s.retain(_ => false)(tx.peer) // no `clear` method

    // ---- bi-group functionality TODO - DRY ----

    private def intersect(frame: Long)(implicit tx: S#Tx): data.Iterator[I#Tx, Leaf[S]] = {
      val start = frame
      val stop  = frame + 1
      //         val shape = Rectangle( ti, MIN_COORD, MAX_COORD - ti + 1, ti - MIN_COORD + 1 )
      // horizontally: until query_stop; vertically: from query_start
      // start < query.stop && stop > query.start
      val shape = LongRectangle(MIN_COORD, start + 1, stop - MIN_COORD, MAX_COORD - start)
      rangeSearch(shape)
    }

    // this can be easily implemented with two rectangular range searches
    // return: (things-that-start, things-that-stop)
    private def eventsAt(frame: Long)(implicit tx: S#Tx): (data.Iterator[I#Tx, Leaf[S]], data.Iterator[I#Tx, Leaf[S]]) = {
      val startShape = LongRectangle(frame, MIN_COORD, 1, MAX_SIDE)
      val stopShape  = LongRectangle(MIN_COORD, frame, MAX_SIDE, 1)
      (rangeSearch(startShape), rangeSearch(stopShape))
    }

    // Long.MinValue indicates _no event_; frame is inclusive!
    private def nearestEventAfter(frame: Long)(implicit tx: S#Tx): Long = {
      implicit val itx: I#Tx = iSys(tx)
      val point = LongPoint2D(frame, frame) // + 1
      val span  = tree.nearestNeighborOption(point, advanceNNMetric).map(_._1).getOrElse(Span.Void)
      span match {
        case sp @ Span.From(start) => assert(start >= frame, sp); start // else None
        case sp @ Span.Until(stop) => assert(stop  >= frame, sp); stop  // else None
        case sp @ Span(start, stop) =>
          if (start >= frame) {
            start
          } else {
            assert(stop >= frame, sp)
            stop
          }
        case _ => Long.MinValue // All or Void
      }
    }

    private def rangeSearch(shape: LongRectangle)(implicit tx: S#Tx): data.Iterator[I#Tx, Leaf[S]] = {
      implicit val itx: I#Tx = iSys(tx)
      tree.rangeQuery(shape)
    }
  }
}