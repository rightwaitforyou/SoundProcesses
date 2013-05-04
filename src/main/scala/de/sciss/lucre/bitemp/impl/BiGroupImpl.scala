/*
 *  BiGroupImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre
package bitemp
package impl

import de.sciss.lucre.{event => evt}
import evt.{Event, EventLike, impl => evti, Sys}
import data.{SkipOctree, Iterator}
import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.breakOut
import scala.annotation.{elidable, switch}
import geom.{LongDistanceMeasure2D, LongRectangle, LongPoint2DLike, LongPoint2D, LongSquare, LongSpace}
import LongSpace.TwoDim
import expr.{Expr, Type}
import de.sciss.span.{SpanLike, Span}
import de.sciss.serial.{DataOutput, DataInput, Serializer}
import de.sciss.lucre.geom.LongRectangle

object BiGroupImpl {
  import BiGroup.{Leaf, TimedElem, Modifiable}

  var showLog = false

  @elidable(elidable.CONFIG) private def log(what: => String) {
    if (showLog) println(s"<bigroup> $what")
  }

  private val MAX_SQUARE  = LongSquare(0, 0, 0x2000000000000000L)
  private val MIN_COORD   = MAX_SQUARE.left
  private val MAX_COORD   = MAX_SQUARE.right
  private val MAX_SIDE    = MAX_SQUARE.side

  //   private final case class Entry[ Elem ]( )

  private type LeafImpl[S <: Sys[S], Elem, U] = (SpanLike, IIdxSeq[TimedElemImpl[S, Elem, U]])
  private type Tree    [S <: Sys[S], Elem, U] = SkipOctree[S, TwoDim, LeafImpl[S, Elem, U]]

  private def opNotSupported: Nothing = sys.error("Operation not supported")

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

  def serializer[S <: Sys[S], Elem, U](eventView: Elem => EventLike[S, U, Elem])(
    implicit elemSerializer: Serializer[S#Tx, S#Acc, Elem],
    spanType: Type[SpanLike]): evt.NodeSerializer[S, BiGroup[S, Elem, U]] = new Ser(eventView)

  def modifiableSerializer[S <: Sys[S], Elem, U](eventView: Elem => EventLike[S, U, Elem])(
    implicit elemSerializer: Serializer[S#Tx, S#Acc, Elem],
    spanType: Type[SpanLike]): evt.NodeSerializer[S, BiGroup.Modifiable[S, Elem, U]] = new ModSer(eventView)

  def readModifiable[S <: Sys[S], Elem, U](in: DataInput, access: S#Acc, eventView: Elem => EventLike[S, U, Elem])
                                          (implicit tx: S#Tx, elemSerializer: Serializer[S#Tx, S#Acc, Elem],
                                           spanType: Type[SpanLike]): BiGroup.Modifiable[S, Elem, U] = {

    val targets = evt.Targets.read[S](in, access)
    read(in, access, targets, eventView)
  }

  private class Ser[S <: Sys[S], Elem, U](eventView: Elem => EventLike[S, U, Elem])
                                         (implicit elemSerializer: Serializer[S#Tx, S#Acc, Elem],
                                          spanType: Type[SpanLike])
    extends evt.NodeSerializer[S, BiGroup[S, Elem, U]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): BiGroup[S, Elem, U] = {
      BiGroupImpl.read(in, access, targets, eventView)
    }
  }

  private class ModSer[S <: Sys[S], Elem, U](eventView: Elem => EventLike[S, U, Elem])
                                            (implicit elemSerializer: Serializer[S#Tx, S#Acc, Elem],
                                             spanType: Type[SpanLike])
    extends evt.NodeSerializer[S, BiGroup.Modifiable[S, Elem, U]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): BiGroup.Modifiable[S, Elem, U] = {
      BiGroupImpl.read(in, access, targets, eventView)
    }
  }

  // ... accepted are points with x > LRP || y > LRP ...
  private val advanceNNMetric = LongDistanceMeasure2D.nextSpanEvent(MAX_SQUARE)
  //   private val advanceNNMetric   = LongDistanceMeasure2D.vehsybehc.exceptOrthant( 1 )

  private val regressNNMetric = LongDistanceMeasure2D.prevSpanEvent(MAX_SQUARE)

  private final class TimedElemImpl[S <: Sys[S], Elem, U](group: Impl[S, Elem, U],
                                                          protected val targets: evt.Targets[S], val span: Expr[S, SpanLike], val value: Elem)
  //   extends TimedElem[ S, Elem, U ] with evt.StandaloneLike[ S, IIdxSeq[ BiGroup.ElementUpdate[ U ]], TimedElemImpl[ S, Elem, U ]]
    extends evti.StandaloneLike[S, BiGroup.Update[S, Elem, U], TimedElem[S, Elem]] with TimedElem[S, Elem] {

    import group.{eventView, elemSerializer}

    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[BiGroup.Update[S, Elem, U]] = {
      var res     = Vector.empty[BiGroup.Change[S, Elem, U]]
      val spanEvt = span.changed
      if (spanEvt.isSource(pull)) {
        pull(spanEvt).foreach { ch =>
          log(s"$this.pullUpdate -> ElementMoved")
          res :+= BiGroup.ElementMoved(this, ch)
        }
      }
      val valueEvt = eventView(value)
      if (valueEvt.isSource(pull)) {
        pull(valueEvt).foreach {
          ch => res :+= BiGroup.ElementMutated(this, ch)
        }
      }

      if (res.nonEmpty) Some(BiGroup.Update(group, res)) else None
    }

    //      def changed: Event[ S, IIdxSeq[ BiGroup.ElementUpdate[ U ]], TimedElem[ S, Elem ]] = this

    protected def writeData(out: DataOutput) {
      span.write(out)
      elemSerializer.write(value, out)
    }

    protected def disposeData()(implicit tx: S#Tx) {}

    /* private[lucre] */ def connect()(implicit tx: S#Tx) {
      log(s"$this.connect()")
      span.changed     ---> this
      eventView(value) ---> this
    }

    /* private[lucre] */ def disconnect()(implicit tx: S#Tx) {
      log(s"$this.disconnect()")
      span.changed     -/-> this
      eventView(value) -/-> this
    }

    protected def reader: evt.Reader[S, TimedElemImpl[S, Elem, U]] = group.TimedSer // timedSerializer[ S, Elem, U ]
  }

  private abstract class Impl[S <: Sys[S], Elem, U](protected val targets: evt.Targets[S],
                                                    val eventView: Elem => EventLike[S, U, Elem])
                                                   (implicit val elemSerializer: Serializer[S#Tx, S#Acc, Elem],
                                                    val spanType: Type[SpanLike])
    extends Modifiable[S, Elem, U] {
    //   with evt.Compound[ S, Impl[ S, Elem, U ], Impl.type ]
    //   with evt.Trigger.Impl[ S, BiGroup.Update[ S, Elem, U ], BiGroup.Update[ S, Elem, U ], BiGroup[ S, Elem, U ]]
    //   with evt.StandaloneLike[ S, BiGroup.Update[ S, Elem, U ], BiGroup[ S, Elem, U ]]
    //   with evt.Node[ S ] {
    group =>

    implicit def pointView: (Leaf[S, Elem], S#Tx) => LongPoint2DLike = (tup, tx) => spanToPoint(tup._1)

    // implicit def hyperSer: ImmutableSerializer[LongSquare] = SpaceSerializers.LongSquareSerializer

    protected def tree: Tree[S, Elem, U]

    //      def eventView: Elem => EventLike[ S, U, Elem ]
    //      implicit def elemSerializer: Serializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ]
    //      implicit def spanType: Type[ SpanLike ]

    override def toString() = "BiGroup" + tree.id

    final def modifiableOption: Option[BiGroup.Modifiable[S, Elem, U]] = Some(this)

    implicit object TimedSer extends evt.NodeSerializer[S, TimedElemImpl[S, Elem, U]] {
      def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): TimedElemImpl[S, Elem, U] = {
        //            readTimedElemImpl( in, access )
        val span  = spanType.readExpr(in, access)
        val value = elemSerializer.read(in, access)
        new TimedElemImpl(group, targets, span, value)
      }
    }

    // ---- event behaviour ----

    private object CollectionEvent
      extends evti.TriggerImpl[S, BiGroup.Update[S, Elem, U], BiGroup[S, Elem, U]]
      with evti.EventImpl     [S, BiGroup.Update[S, Elem, U], BiGroup[S, Elem, U]]
      with evt.InvariantEvent [S, BiGroup.Update[S, Elem, U], BiGroup[S, Elem, U]]
      with evti.Root          [S, BiGroup.Update[S, Elem, U]] {

      protected def reader: evt.Reader[S, BiGroup[S, Elem, U]] = serializer(eventView)

      override def toString = node.toString + ".CollectionEvent"
      def slot: Int = 1
      def node: BiGroup[S, Elem, U] = group
    }

    private object ElementEvent
      extends evti.EventImpl [S, BiGroup.Update[S, Elem, U], BiGroup[S, Elem, U]]
      with evt.InvariantEvent[S, BiGroup.Update[S, Elem, U], BiGroup[S, Elem, U]] {

      protected def reader: evt.Reader[S, BiGroup[S, Elem, U]] = serializer(eventView)

      override def toString = node.toString + ".ElementEvent"
      def slot: Int = 2
      def node: BiGroup[S, Elem, U] = group

      //      def connect   ()(implicit tx: S#Tx) {}
      //      def disconnect()(implicit tx: S#Tx) {}

      private def foreach(fun: TimedElemImpl[S, Elem, U] => Unit)(implicit tx: S#Tx) {
        tree.iterator.foreach {
          case (_, seq) => seq.foreach(fun)
        }
      }

      def connect()(implicit tx: S#Tx) {
        log(s"$this.connect()")
        foreach(+= _)
      }

      def disconnect()(implicit tx: S#Tx) {
        log(s"$this.disconnect()")
        foreach(-= _)
      }

      def +=(elem: TimedElemImpl[S, Elem, U])(implicit tx: S#Tx) {
        // log(s"$this += $elem")
        elem ---> this
      }

      def -=(elem: TimedElemImpl[S, Elem, U])(implicit tx: S#Tx) {
        // log(s"$this -= $elem")
        elem -/-> this
      }

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[BiGroup.Update[S, Elem, U]] = {
        val par = pull.parents(this)
        log(s"$this.pullUpdate -> parents = $par")

        val changes: IIdxSeq[BiGroup.Change[S, Elem, U]] = par.flatMap(sel => {
          //               val elem = sel.devirtualize( elemReader ).node.asInstanceOf[ Elem ]
          //val elem = sel.devirtualize( elemSerializer.asInstanceOf[ evt.Reader[ S, evt.Node[ S ]]]).node.
          //   asInstanceOf[ Elem ]
          val timed = sel.devirtualize(TimedSer /* timedSerializer[ S, Elem, U ]*/).node.asInstanceOf[TimedElemImpl[S, Elem, U]]
          val ch0   = pull(timed).map(_.changes).getOrElse(Vector.empty)
          log(s"$this.pullUpdate -> from timed $timed pulled $ch0")
          ch0.foreach {
            case BiGroup.ElementMoved(_, evt.Change(spanValOld, spanValNew)) =>
              assert(removeNoFire(spanValOld, timed))
              addNoFire          (spanValNew, timed)

            case _ =>
          }
          //               ch0.map {
          //                  case ch @ BiGroup.ElementMoved( _, evt.Change( spanValOld, spanValNew )) =>
          //                     removeNoFire( spanValOld, timed )
          //                     addNoFire(    spanValNew, timed )
          //                     timed -> ch
          //
          //                  case ch => timed -> ch
          //               }
          ch0
        })(breakOut)

        if (changes.isEmpty) None else Some(BiGroup.Update(group, changes))
      }
    }

    private object ChangeEvent
      extends evt.Event[S, BiGroup.Update[S, Elem, U], BiGroup.Modifiable[S, Elem, U]]
      with evt.InvariantSelector[S] {

      protected def reader: evt.Reader[S, BiGroup[S, Elem, U]] = serializer(eventView)

      override def toString = node.toString + ".ChangeEvent"  // default toString invokes `slot`!
      def slot: Int = opNotSupported

      def node: BiGroup.Modifiable[S, Elem, U] = group

      def connect   ()(implicit tx: S#Tx) {}
      def disconnect()(implicit tx: S#Tx) {}

      def --->(r: evt.Selector[S])(implicit tx: S#Tx) {
        log(s"$this.--->($r)")
        CollectionEvent ---> r
        ElementEvent    ---> r
      }

      def -/->(r: evt.Selector[S])(implicit tx: S#Tx) {
        log(s"$this.-/->($r)")
        CollectionEvent -/-> r
        ElementEvent    -/-> r
      }

      // XXX TODO: potential problem with event collapsing
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[BiGroup.Update[S, Elem, U]] = {
        if      (CollectionEvent.isSource(pull)) pull(CollectionEvent)
        else if (ElementEvent   .isSource(pull)) pull(ElementEvent   )
        else None
      }

      def react[A1 >: BiGroup.Update[S, Elem, U]](fun: A1 => Unit)(implicit tx: S#Tx): evt.Observer[S, A1, BiGroup.Modifiable[S, Elem, U]] =
        reactTx[A1](_ => fun)

      def reactTx[A1 >: BiGroup.Update[S, Elem, U]](fun: S#Tx => A1 => Unit)(implicit tx: S#Tx): evt.Observer[S, A1, BiGroup.Modifiable[S, Elem, U]] = {
        val obs = evt.Observer(modifiableSerializer(eventView), fun)
        obs.add(CollectionEvent)
        obs.add(ElementEvent)
        obs
      }

      def isSource(pull: evt.Pull[S]): Boolean = CollectionEvent.isSource(pull) || ElementEvent.isSource(pull)
    }

    final protected def disposeData()(implicit tx: S#Tx) {
      tree.dispose()
    }

    final protected def writeData(out: DataOutput) {
      tree.write(out)
    }

    //    private def foreach(fun: TimedElemImpl[S, Elem, U] => Unit)(implicit tx: S#Tx) {
    //      tree.iterator.foreach {
    //        case (_, seq) => seq.foreach(fun)
    //      }
    //    }

    //    final def connect()(implicit tx: S#Tx) {
    //      log(s"$this.connect()")
    //      foreach(ElementEvent += _)
    //    }
    //
    //    final def disconnect()(implicit tx: S#Tx) {
    //      log(s"$this.disconnect()")
    //      foreach(ElementEvent -= _)
    //    }

    final def select(slot: Int, invariant: Boolean): evt.Event[S, Any, Any] = (slot: @switch) match {
      case 1 => CollectionEvent
      case 2 => ElementEvent
    }

    // ---- collection behaviour ----

    @inline private def isConnected(implicit tx: S#Tx): Boolean = targets.nonEmpty

    final def clear()(implicit tx: S#Tx) {
      if (isConnected) {
        val changes = tree.iterator.toIndexedSeq.flatMap {
          case (spanVal, seq) =>
            seq.map {
              timed => BiGroup.Removed(spanVal, timed)
            }
        }
        tree.clear()
        if (changes.nonEmpty) CollectionEvent(BiGroup.Update(group, changes))

      } else {
        tree.clear()
      }
    }

    final def add(span: Expr[S, SpanLike], elem: Elem)(implicit tx: S#Tx): TimedElem[S, Elem] = {
      log(s"$this.add($span, $elem); isConnected? $isConnected")
      val spanVal = span.value
      val tgt     = evt.Targets[S] // XXX partial?
      val timed   = new TimedElemImpl[S, Elem, U](group, tgt, span, elem)
      addNoFire(spanVal, timed)
      if (isConnected) {
        ElementEvent += timed
        CollectionEvent(BiGroup.Update(group, IIdxSeq(BiGroup.Added(spanVal, timed))))
      }
      timed
    }

    private def addNoFire(spanVal: SpanLike, timed: TimedElemImpl[S, Elem, U])(implicit tx: S#Tx) {
      val point = spanToPoint(spanVal)
      //if( showLog ) println( "add at point " + point )
      //         val entry   = (span, elem)
      tree.transformAt(point) {
        case None => Some(spanVal -> IIdxSeq(timed))
        case Some((_, seq)) => Some(spanVal -> (seq :+ timed))
      }
    }

    final def remove(span: Expr[S, SpanLike], elem: Elem)(implicit tx: S#Tx): Boolean = {
      val spanVal = span.value
      val point   = spanToPoint(spanVal)
      val timedO  = tree.get(point).flatMap {
        case (_, IIdxSeq(single)) =>
          if (single.span == span && single.value == elem) {
            tree.removeAt(point)
            Some(single)
          } else {
            None
          }
        case (_, seq) =>
          val (equal, diff) = seq.partition(timed => timed.span == span && timed.value == elem)
          if (equal.nonEmpty) {
            tree.add((spanVal, diff))
            equal.headOption
          } else {
            None
          }
      }

      if (isConnected) timedO.foreach { timed =>
        ElementEvent -= timed
        CollectionEvent(BiGroup.Update(group, IIdxSeq(BiGroup.Removed(spanVal, timed))))
      }

      timedO.isDefined
    }

    private def removeNoFire(spanVal: SpanLike, timed: TimedElemImpl[S, Elem, U])(implicit tx: S#Tx): Boolean = {
      val point = spanToPoint(spanVal)
      val entry = tree.get(point)
      entry match {
        case Some((_, IIdxSeq(single))) =>
          if (single == timed) {
            assert(tree.removeAt(point).isDefined)
            true
          } else {
            false
          }
        case Some((_, seq)) =>
          val seqNew = seq.filterNot(_ == timed)
          if (seqNew.size != seq.size) {
            assert(tree.add((spanVal, seqNew)))
            true
          } else {
            false
          }
        case None => false
      }
    }

    final def debugList()(implicit tx: S#Tx): List[(SpanLike, Elem)] =
      tree.toList.flatMap {
        case (span, seq) => seq.map(span -> _.value)
      }

    //      final def iterator( implicit tx: S#Tx, chr: Chronos[ S ]) : txn.Iterator[ S#Tx, Leaf[ S, Elem ]]  =
    //         intersect( chr.time.value )

    final def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Leaf[S, Elem]] = tree.iterator

    final def intersect(time: Long)(implicit tx: S#Tx): Iterator[S#Tx, Leaf[S, Elem]] = {
      val start = time
      val stop = time + 1
      //         val shape = Rectangle( ti, MIN_COORD, MAX_COORD - ti + 1, ti - MIN_COORD + 1 )
      // horizontally: until query_stop; vertically: from query_start
      // start < query.stop && stop > query.start
      val shape = LongRectangle(MIN_COORD, start + 1, stop - MIN_COORD, MAX_COORD - start)
      rangeSearch(shape)
    }

    final def intersect(span: SpanLike)(implicit tx: S#Tx): Iterator[S#Tx, Leaf[S, Elem]] = {
      // horizontally: until query_stop; vertically: from query_start
      span match {
        case Span(start, stop) =>
          val shape = LongRectangle(MIN_COORD, start + 1, stop - MIN_COORD, MAX_COORD - start)
          rangeSearch(shape)

        case Span.From(start) =>
          val shape = LongRectangle(MIN_COORD, start + 1, MAX_SIDE, MAX_COORD - start)
          rangeSearch(shape)

        case Span.Until(stop) =>
          val shape = LongRectangle(MIN_COORD, MIN_COORD, stop - MIN_COORD, MAX_SIDE)
          rangeSearch(shape)

        case Span.All  => tree.iterator
        case Span.Void => Iterator.empty
      }
    }

    final def rangeSearch(start: SpanLike, stop: SpanLike)(implicit tx: S#Tx): Iterator[S#Tx, Leaf[S, Elem]] = {
      if (start == Span.Void || stop == Span.Void) return Iterator.empty

      val startP = searchSpanToPoint(start)
      val stopP  = searchSpanToPoint(stop)
      val shape  = LongRectangle(startP.x, stopP.x, startP.y - startP.x /* + 1 */ , stopP.y - stopP.x /* + 1 */)
      //println( "RANGE " + shape )
      rangeSearch(shape)
    }

    // this can be easily implemented with two rectangular range searches
    final def eventsAt(time: Long)(implicit tx: S#Tx): (Iterator[S#Tx, Leaf[S, Elem]], Iterator[S#Tx, Leaf[S, Elem]]) = {
      val startShape = LongRectangle(time, MIN_COORD, 1, MAX_SIDE)
      val stopShape  = LongRectangle(MIN_COORD, time, MAX_SIDE, 1)
      (rangeSearch(startShape), rangeSearch(stopShape))
    }

    final def nearestEventAfter(time: Long)(implicit tx: S#Tx): Option[Long] = {
      val point = LongPoint2D(time, time) // + 1
      val span  = tree.nearestNeighborOption(point, advanceNNMetric).map(_._1).getOrElse(Span.Void)
      span match {
        case sp @ Span.From(start) => assert(start >= time, sp); Some(start) // else None
        case sp @ Span.Until(stop) => assert(stop  >= time, sp); Some(stop ) // else None
        case sp @ Span(start, stop) =>
          if (start >= time) {
            Some(start)
          } else {
            assert(stop >= time, sp)
            Some(stop)
          }
        case _ => None // All or Void
      }
    }

    final def nearestEventBefore(time: Long)(implicit tx: S#Tx): Option[Long] = {
      val point = LongPoint2D(time, time)
      val span = tree.nearestNeighborOption(point, regressNNMetric).map(_._1).getOrElse(Span.Void)
      span match {
        case sp @ Span.From(start)  => assert(start <= time, sp); Some(start) // else None
        case sp @ Span.Until(stop)  => assert(stop  <= time, sp); Some(stop ) // else None
        case sp @ Span(start, stop) =>
          if (stop <= time) {
            Some(stop)
          } else {
            assert(start <= time, sp)
            Some(start)
          }
        case _ => None // All or Void
      }
    }

    private def rangeSearch(shape: LongRectangle)(implicit tx: S#Tx): Iterator[S#Tx, Leaf[S, Elem]] = {
      val res = tree.rangeQuery(shape) // .flatMap ....
      //if( showLog ) println( "Range in " + shape + " --> right = " + shape.right + "; bottom = " + shape.bottom + " --> found some? " + !res.isEmpty )
      res
    }

    //      final def collectionChanged : Event[ S, BiGroup.Collection[ S, Elem, U ], BiGroup[ S, Elem, U ]] = CollectionEvent
    //      final def elementChanged    : Event[ S, BiGroup.Element[    S, Elem, U ], BiGroup[ S, Elem, U ]] = ElementEvent
    final def changed: Event[S, BiGroup.Update[S, Elem, U], BiGroup.Modifiable[S, Elem, U]] = ChangeEvent
  }

  def newModifiable[S <: Sys[S], Elem, U](eventView: Elem => EventLike[S, U, Elem])(
    implicit tx: S#Tx, elemSerializer: Serializer[S#Tx, S#Acc, Elem],
    spanType: Type[SpanLike]): Modifiable[S, Elem, U] = {

    new Impl(evt.Targets[S], eventView) {
      group =>

      val tree: Tree[S, Elem, U] = {
        SkipOctree.empty[S, TwoDim, LeafImpl[S, Elem, U]](MAX_SQUARE) // ( tx, view, space, )
      }
    }
  }

  private def read[S <: Sys[S], Elem, U](in: DataInput, access: S#Acc, targets: evt.Targets[S], eventView: Elem => EventLike[S, U, Elem])
                                        (implicit tx: S#Tx,
                                         elemSerializer: Serializer[S#Tx, S#Acc, Elem],
                                         spanType: Type[SpanLike]): Impl[S, Elem, U] = {
    new Impl(targets, eventView) {
      val tree: Tree[S, Elem, U] = {
        SkipOctree.read[S, TwoDim, LeafImpl[S, Elem, U]](in, access)
      }
    }
  }
}
