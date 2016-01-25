/*
 *  AuralGraphemeBase.scala
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

import de.sciss.lucre.bitemp.BiPin
import de.sciss.lucre.data.SkipList
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.TimeRef.Apply
import de.sciss.synth.proc.{logAural => logA}

import scala.collection.immutable.{IndexedSeq => Vec}

object AuralGraphemeBase {
  protected final case class ElemHandle[S <: Sys[S], Elem](start: Long, view: Elem)
}
trait AuralGraphemeBase[S <: Sys[S], I <: stm.Sys[I], Target, Elem <: AuralView[S, Target]]
  extends AuralScheduledBase[S, Target, Elem] with ObservableImpl[S, AuralView.State] { impl =>

  // ---- abstract ----

  def obj: stm.Source[S#Tx, Grapheme[S]]

  // protected def tree: SkipOctree[I, LongSpace.TwoDim, (SpanLike, Vec[(stm.Source[S#Tx, S#ID], Elem)])]
  protected def tree: SkipList.Map[I, Long, Vec[Elem]]

  protected def iSys: S#Tx => I#Tx

  protected def makeViewElem(obj: Obj[S])(implicit tx: S#Tx): Elem

  // ---- impl ----

  private[this] var grObserver: Disposable[S#Tx] = _

  final def typeID: Int = Grapheme.typeID

  protected type ViewID     = Unit
  protected type ElemHandle = AuralGraphemeBase.ElemHandle[S, Elem]

  private[this] final def ElemHandle(start: Long, view: Elem): ElemHandle =
    AuralGraphemeBase.ElemHandle(start, view)

  protected final def viewEventAfter(frame: Long)(implicit tx: S#Tx): Long =
    tree.ceil(frame + 1)(iSys(tx)).fold(Long.MaxValue)(_._1)

  protected final def modelEventAfter(frame: Long)(implicit tx: S#Tx): Long =
    obj().eventAfter(frame).getOrElse(Long.MaxValue)

  protected final def processPlay(timeRef: Apply, target: Target)(implicit tx: S#Tx): Unit = {
    implicit val itx = iSys(tx)
    tree.floor(timeRef.frame).foreach { case (start, entries) =>
      val toStart   = entries.head
      val stop      = viewEventAfter(start)
      val span      = if (stop == Long.MaxValue) Span.From(start) else Span(start, stop)
      val h         = ElemHandle(start, toStart)
      val tr0       = timeRef.intersect(span)
      playView(h, tr0, target)
    }
  }

  protected final def processPrepare(spanP: Span, timeRef: Apply, initial: Boolean)
                                    (implicit tx: S#Tx): Iterator[PrepareResult] = {
    // println(s"processPrepare($span, $timeRef, initial = $initial")
    val gr    = obj()
    val opt0  = if (initial) gr.floor(spanP.start) else gr.ceil(spanP.start)
    opt0.fold[Iterator[PrepareResult]](Iterator.empty) { e0 =>
      new Iterator[PrepareResult] {
        // updated in `advance`:
        private[this] var _child    : Obj[S]                  = _
        private[this] var _childSpan: Span.HasStart           = _
        private[this] var _ended    : Boolean                 = _
        private[this] var _succOpt  : Option[(Obj[S], Long)]  = _

        def hasNext(): Boolean = !_ended

        private[this] def advance(child: Obj[S], start: Long): Unit =
          if (start >= spanP.stop) {
            _succOpt = None
            _ended   = true
          } else {
            _child = child
            gr.ceil(start + 1) match {
              case Some(succ) =>
                val stop      = succ.key.value
                _childSpan    = Span(start, stop)
                val childTime = timeRef.intersect(_childSpan)
                _ended        = childTime.span.nonEmpty
                _succOpt      = if (_ended) None else Some((succ.value, stop))

              case None =>
                _childSpan    = Span.from(start)
                val childTime = timeRef.intersect(_childSpan)
                _ended        = childTime.span.nonEmpty
                _succOpt      = None
            }
          }

        advance(e0.value, e0.key.value)

        def next(): (ViewID, SpanLike, Obj[S]) = {
          if (_ended) throw new NoSuchElementException("next on empty iterator")

          val res = ((), _childSpan, _child)
          _succOpt.fold[Unit] { _ended = true } { case (succ, stop) =>
            advance(succ, stop)
          }
          res
        }
      }
    }
  }

  //  protected final def clearViewsTree()(implicit tx: S#Tx): Unit = {
  //    // println("tree.clear()")
  //    tree.clear()(iSys(tx))
  //  }

  protected final def processEvent(play: IPlaying, timeRef: Apply)(implicit tx: S#Tx): Unit = {
    val start   = timeRef.frame
    val toStart = tree.get(start)(iSys(tx))
      .getOrElse(throw new IllegalStateException(s"No element at event ${timeRef.frame}"))
      .head
    // val stop    = viewEventAfter(start)
    // val span    = if (stop == Long.MaxValue) Span.From(start) else Span(start, stop)
    val h       = ElemHandle(start, toStart)
    stopViews()
    playView(h, timeRef, play.target)
  }

  def init(gr: Grapheme[S])(implicit tx: S#Tx): this.type = {
    grObserver = gr.changed.react { implicit tx => upd =>
      upd.changes.foreach {
        case Grapheme.Added  (time, entry)    => elemAdded  (upd.pin, time, entry.value)
        case Grapheme.Removed(time, entry)    => elemRemoved(         time, entry.value)
        case Grapheme.Moved  (timeCh, entry)  =>
          // for simplicity just remove and re-add
          // ; in the future this could be optimized
          // (e.g., not deleting and re-creating the AuralObj)
          elemRemoved(         timeCh.before, entry.value)
          elemAdded  (upd.pin, timeCh.now   , entry.value)
      }
    }
    this
  }

//  override protected def playView(id: Unit, view: Elem, timeRef: TimeRef, target: Target)
//                                 (implicit tx: S#Tx): Unit = {
//    views.foreach(stopView) // there ought to be either zero or one view
//    super.playView(id, view, timeRef, target)
//  }

//  private[this] def playView1(view: Elem, timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit = {
//    views.foreach(stopView) // there ought to be either zero or one view
//    playView((), view, timeRef, target)
//  }

  protected def removeView(h: ElemHandle)(implicit tx: S#Tx): Unit = {
    implicit val itx = iSys(tx)
    val start = h.start
    val seq0  = tree.get(start).get
    val idx   = seq0.indexOf(h.view)
    if (idx < 0) throw new IllegalStateException(s"View ${h.view} not found.")
    val seq1  = seq0.patch(idx, Nil, 1)
    if (seq1.isEmpty) tree.remove(start) else tree.add(start -> seq1)
  }

  protected def elemFromHandle(h: ElemHandle): Elem = h.view

  /** A notification method that may be used to `fire` an event
    * such as `AuralObj.Timeline.ViewAdded`.
    */
  protected def viewPlaying(h: ElemHandle)(implicit tx: S#Tx): Unit = ()
  protected def viewStopped(h: ElemHandle)(implicit tx: S#Tx): Unit = ()

  protected def mkView(vid: Unit, span: SpanLike, obj: Obj[S])(implicit tx: S#Tx): ElemHandle =
    span match {
      case hs: Span.HasStart =>
        val view = makeViewElem(obj)
        ElemHandle(hs.start, view)
      case _ =>
        throw new IllegalArgumentException(s"Span should have start: $span")
    }

  private[this] def elemAdded(pin: BiPin[S, Obj[S]], start: Long, child: Obj[S])(implicit tx: S#Tx): Unit = {
    val span = pin.eventAfter(start).fold[SpanLike](Span.From(start))(Span(start, _))
    elemAdded(vid = (), span = span, obj = child)
  }

  private[this] def elemRemoved(start: Long, child: Obj[S])(implicit tx: S#Tx): Unit = {
    implicit val itx = iSys(tx)
    tree.get(start).foreach { seq =>
      seq.find(_.obj() == child).foreach { view =>
        logA(s"timeline - elemRemoved($start, $child)")
        val h = ElemHandle(start, view)
        elemRemoved(h)
      }
    }

//    views.find(_.obj() == child).foreach { view =>
//      // finding the object in the view-map implies that it
//      // is currently preparing or playing
//      logA(s"timeline - elemRemoved($start, $child)")
//      elemRemoved(view)
//    }
  }

  protected def checkReschedule(h: ElemHandle, currentFrame: Long, oldTarget: Long, elemPlays: Boolean)
                               (implicit tx: S#Tx): Boolean =
    !elemPlays && {
      // reschedule if the span has a start and that start is greater than the current frame,
      // and elem.start == oldTarget
      h.start > currentFrame && h.start == oldTarget
    }

  override def dispose()(implicit tx: S#Tx): Unit = {
    super.dispose()
    grObserver.dispose()
  }
}