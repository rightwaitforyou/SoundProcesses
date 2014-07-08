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
import de.sciss.lucre.geom.{LongRectangle, LongPoint2D, LongSquare, LongSpace}
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.stm
import de.sciss.lucre.synth.Sys
import de.sciss.span.{Span, SpanLike}
import de.sciss.lucre.data

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

  private def searchSpanToPoint(span: SpanLike): LongPoint2D = span match {
    case Span(start, stop)  => LongPoint2D(start,     stop         )
    case Span.From(start)   => LongPoint2D(start,     MAX_COORD + 1)
    case Span.Until(stop)   => LongPoint2D(MIN_COORD, stop         )
    case Span.All           => LongPoint2D(MIN_COORD, MAX_COORD + 1)
    case Span.Void          => LongPoint2D(MAX_COORD, MIN_COORD    ) // ?? what to do with this case ?? forbid?
  }

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
    val map = SkipOctree.empty[I, LongSpace.TwoDim, Leaf[S]](MAX_SQUARE)

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
        map.add(span -> views)
    }

    val res = new Impl[S, I](tx.newHandle(tlObj), map)
    res.init(tlObj)
    res
  }

  private final class Impl[S <: Sys[S], I <: stm.Sys[I]](val obj: stm.Source[S#Tx, Timeline.Obj[S]],
      map: SkipOctree[I, LongSpace.TwoDim, Leaf[S]])(implicit iSys: S#Tx => I#Tx)
    extends AuralObj.Timeline[S] with ObservableImpl[S, AuralObj.State] {

    def typeID: Int = Timeline.typeID

    private val currentStateRef = Ref[AuralObj.State](AuralObj.Stopped)
    private val activeViews     = TSet.empty[AuralObj[S]]
    private var tlObserver: Disposable[S#Tx] = _

    def state(implicit tx: S#Tx): AuralObj.State = currentStateRef.get(tx.peer)

    private def state_=(value: AuralObj.State)(implicit tx: S#Tx): Unit = {
      val old = currentStateRef.swap(value)(tx.peer)
      if (value != old) fire(value)
    }

    def init(tl: Timeline.Obj[S])(implicit tx: S#Tx): Unit = {
      tlObserver = tl.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case _ => // XXX TODO
        }
      }
    }

    def play()(implicit tx: S#Tx): Unit = {
      if (state != AuralObj.Stopped) return

      implicit val ptx = tx.peer
      implicit val itx: I#Tx = iSys(tx)
      val frame = 0L    // XXX TODO - eventually should be provided by the `play` method
      intersect(frame).foreach { case (span, views) =>
        views.foreach { view =>
          view.play()
          activeViews.add(view)
        }
      }
      state = AuralObj.Playing
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

    private def rangeSearch(shape: LongRectangle)(implicit tx: S#Tx): data.Iterator[I#Tx, Leaf[S]] = {
      implicit val itx: I#Tx = iSys(tx)
      map.rangeQuery(shape)
    }
  }
}