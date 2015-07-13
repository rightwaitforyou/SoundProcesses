/*
 *  BiGroupImpl.scala
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

package de.sciss.lucre
package bitemp.impl

import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.data.{DeterministicSkipOctree, Iterator, SkipOctree}
import de.sciss.lucre.event.{Event, EventLike, Sys, impl => evti}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.geom.LongSpace.TwoDim
import de.sciss.lucre.geom.{DistanceMeasure, LongDistanceMeasure2D, LongPoint2D, LongPoint2DLike, LongRectangle, LongSpace}
import de.sciss.lucre.stm.Identifiable
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.span.{Span, SpanLike}
import de.sciss.{model => m}

import scala.annotation.{elidable, switch}
import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}

object BiGroupImpl {
  import BiGroup.{Leaf, MaxCoordinate, MaxSide, MaxSquare, MinCoordinate, Modifiable, TimedElem}

  def spanToPoint(span: SpanLike): LongPoint2D = span match {
    case Span(start, stop)  => LongPoint2D(start,     stop     )
    case Span.From(start)   => LongPoint2D(start,     MaxCoordinate)
    case Span.Until(stop)   => LongPoint2D(MinCoordinate, stop     )
    case Span.All           => LongPoint2D(MinCoordinate, MaxCoordinate)
    case Span.Void          => LongPoint2D(MaxCoordinate, MinCoordinate) // ?? what to do with this case ?? forbid?
  }

  def searchSpanToPoint(span: SpanLike): LongPoint2D = span match {
    case Span(start, stop)  => LongPoint2D(start,     stop         )
    case Span.From(start)   => LongPoint2D(start,     MaxCoordinate + 1)
    case Span.Until(stop)   => LongPoint2D(MinCoordinate, stop         )
    case Span.All           => LongPoint2D(MinCoordinate, MaxCoordinate + 1)
    case Span.Void          => LongPoint2D(MaxCoordinate, MinCoordinate    ) // ?? what to do with this case ?? forbid?
  }

  final def intersectTime[S <: stm.Sys[S], A](tree: Tree[S, A])(time: Long)
                                           (implicit tx: S#Tx): Iterator[S#Tx, A] = {
    val start = time
    val stop = time + 1
    //         val shape = Rectangle( ti, MinCoordinate, MaxCoordinate - ti + 1, ti - MinCoordinate + 1 )
    // horizontally: until query_stop; vertically: from query_start
    // start < query.stop && stop > query.start
    val shape = LongRectangle(MinCoordinate, start + 1, stop - MinCoordinate, MaxCoordinate - start)
    tree.rangeQuery(shape)
  }

  final def intersectSpan[S <: stm.Sys[S], A](tree: Tree[S, A])(span: SpanLike)
                                           (implicit tx: S#Tx): Iterator[S#Tx, A] = {
    // horizontally: until query_stop; vertically: from query_start
    span match {
      case Span(start, stop) =>
        val shape = LongRectangle(MinCoordinate, start + 1, stop - MinCoordinate, MaxCoordinate - start)
        tree.rangeQuery(shape)

      case Span.From(start) =>
        val shape = LongRectangle(MinCoordinate, start + 1, MaxSide, MaxCoordinate - start)
        tree.rangeQuery(shape)

      case Span.Until(stop) =>
        val shape = LongRectangle(MinCoordinate, MinCoordinate, stop - MinCoordinate, MaxSide)
        tree.rangeQuery(shape)

      case Span.All  => tree.iterator
      case Span.Void => Iterator.empty
    }
  }

  final def rangeSearch[S <: stm.Sys[S], A](tree: Tree[S, A])(start: SpanLike, stop: SpanLike)
                                             (implicit tx: S#Tx): Iterator[S#Tx, A] = {
    if (start == Span.Void || stop == Span.Void) return Iterator.empty

    val startP = searchSpanToPoint(start)
    val stopP  = searchSpanToPoint(stop)
    val shape  = LongRectangle(startP.x, stopP.x, startP.y - startP.x /* + 1 */ , stopP.y - stopP.x /* + 1 */)
    //println( "RANGE " + shape )
    tree.rangeQuery(shape)
  }

  // this can be easily implemented with two rectangular range searches
  final def eventsAt[S <: stm.Sys[S], A](tree: Tree[S, A])(time: Long)
                                          (implicit tx: S#Tx): (Iterator[S#Tx, A], Iterator[S#Tx, A]) = {
    val startShape = LongRectangle(time, MinCoordinate, 1, MaxSide)
    val stopShape  = LongRectangle(MinCoordinate, time, MaxSide, 1)
    (tree.rangeQuery(startShape), tree.rangeQuery(stopShape))
  }

  final def nearestEventAfter[S <: stm.Sys[S], T2](tree: Tree[S, (SpanLike, T2)])(time: Long)
                                                   (implicit tx: S#Tx): Option[Long] = {
    val point = LongPoint2D(time, time) // + 1
    val span  = tree.nearestNeighborOption(point, AdvanceNextNeighborMetric).map(_._1).getOrElse(Span.Void)
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

  final def nearestEventBefore[S <: Sys[S], T2](tree: Tree[S, (SpanLike, T2)])(time: Long)
                                                    (implicit tx: S#Tx): Option[Long] = {
    val point = LongPoint2D(time, time)
    val span  = tree.nearestNeighborOption(point, RegressNextNeighborMetric).map(_._1).getOrElse(Span.Void)
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

  // ... accepted are points with x > LRP || y > LRP ...
  final val AdvanceNextNeighborMetric: DistanceMeasure.Ops[Long, LongSpace.TwoDim] = LongDistanceMeasure2D.nextSpanEvent(MaxSquare)
  final val RegressNextNeighborMetric: DistanceMeasure.Ops[Long, LongSpace.TwoDim] = LongDistanceMeasure2D.prevSpanEvent(MaxSquare)
  
  var showLog = false

  @elidable(elidable.CONFIG) private def log(what: => String): Unit =
    if (showLog) println(s"<bigroup> $what")

  type Tree    [S <: stm.Sys[S], A] = SkipOctree[S, TwoDim, A]
  type LeafImpl[S <: Sys[S], Elem, U] = (SpanLike, Vec[TimedElemImpl[S, Elem, U]])
  type TreeImpl[S <: Sys[S], Elem, U] = SkipOctree[S, TwoDim, LeafImpl[S, Elem, U]]
  
  def verifyConsistency[S <: Sys[S], Elem, U](group: BiGroup[S, Elem, U], reportOnly: Boolean)
                                             (implicit tx: S#Tx): Vec[String] = {
    group match {
      case impl: Impl[S, Elem, U] => impl.treeHandle match {
        case t: DeterministicSkipOctree[S, _, _] =>
          DeterministicSkipOctree.verifyConsistency(t, reportOnly)
        case _ => sys.error("Not a deterministic octree implementation")
      }
    }
  }

  def serializer[S <: Sys[S], Elem, U](eventView: Elem => EventLike[S, U])(
      implicit elemSerializer: Serializer[S#Tx, S#Acc, Elem]): evt.NodeSerializer[S, BiGroup[S, Elem, U]] =
    new Ser(eventView)

  def modifiableSerializer[S <: Sys[S], Elem, U](eventView: Elem => EventLike[S, U])(
    implicit elemSerializer: Serializer[S#Tx, S#Acc, Elem]): evt.NodeSerializer[S, BiGroup.Modifiable[S, Elem, U]] =
    new ModSer(eventView)

  def readModifiable[S <: Sys[S], Elem, U](in: DataInput, access: S#Acc, eventView: Elem => EventLike[S, U])
              (implicit tx: S#Tx, elemSerializer: Serializer[S#Tx, S#Acc, Elem]): BiGroup.Modifiable[S, Elem, U] = {
    val targets = evt.Targets.read[S](in, access)
    read(in, access, targets, eventView)
  }

  private class Ser[S <: Sys[S], Elem, U](eventView: Elem => EventLike[S, U])
                                         (implicit elemSerializer: Serializer[S#Tx, S#Acc, Elem])
    extends evt.NodeSerializer[S, BiGroup[S, Elem, U]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): BiGroup[S, Elem, U] = {
      BiGroupImpl.read(in, access, targets, eventView)
    }
  }

  private class ModSer[S <: Sys[S], Elem, U](eventView: Elem => EventLike[S, U])
                                            (implicit elemSerializer: Serializer[S#Tx, S#Acc, Elem])
    extends evt.NodeSerializer[S, BiGroup.Modifiable[S, Elem, U]] {

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): BiGroup.Modifiable[S, Elem, U] = {
      BiGroupImpl.read(in, access, targets, eventView)
    }
  }

  private[lucre] final class TimedElemImpl[S <: Sys[S], Elem, U](group          : Impl[S, Elem, U],
                                                          protected val targets : evt.Targets[S],
                                                          val span              : Expr[S, SpanLike],
                                                          val value             : Elem)
    extends evti.StandaloneLike[S, BiGroup.Update[S, Elem, U], TimedElem[S, Elem]] with TimedElem[S, Elem] {

    import group.{elemSerializer, eventView}

    override def toString = s"TimedElem$id"

    /** Tricky override to allow comparison with BiGroup.TimedElem.Wrapper */
    override def equals(that: Any): Boolean = that match {
      case m: Identifiable[_] => this.id == m.id
      case _ => super.equals(that)
    }

    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[BiGroup.Update[S, Elem, U]] = {
      var res     = Vector.empty[BiGroup.Change[S, Elem, U]]
      val spanEvt = span.changed
      if (pull.contains(spanEvt)) {
        pull(spanEvt).foreach { ch =>
          log(s"$this.pullUpdate -> ElementMoved")
          res :+= BiGroup.ElementMoved(this, ch)
        }
      }
      val valueEvt = eventView(value)
      if (pull.contains(valueEvt)) {
        pull(valueEvt).foreach {
          ch => res :+= BiGroup.ElementMutated(this, ch)
        }
      }

      if (res.nonEmpty) Some(BiGroup.Update(group, res)) else None
    }

    protected def writeData(out: DataOutput): Unit = {
      span.write(out)
      elemSerializer.write(value, out)
    }

    protected def disposeData()(implicit tx: S#Tx) = ()

    def connect()(implicit tx: S#Tx): Unit = {
      log(s"$this.connect()")
      span.changed     ---> this
      eventView(value) ---> this
    }

    def disconnect()(implicit tx: S#Tx): Unit = {
      log(s"$this.disconnect()")
      span.changed     -/-> this
      eventView(value) -/-> this
    }

    protected def reader: evt.Reader[S, TimedElemImpl[S, Elem, U]] = group.TimedSer
  }

  abstract class Impl[S <: Sys[S], Elem, U] extends Modifiable[S, Elem, U] {
    group =>

    // ---- abstract ----

    implicit final def pointView: (Leaf[S, Elem], S#Tx) => LongPoint2DLike = (tup, tx) => spanToPoint(tup._1)

    protected def tree: TreeImpl[S, Elem, U]
    def eventView(elem: Elem): EventLike[S, U]
    implicit def elemSerializer: Serializer[S#Tx, S#Acc, Elem]

    // ---- implemented ----

    protected final def newTree()(implicit tx: S#Tx): TreeImpl[S, Elem, U] =
      SkipOctree.empty[S, TwoDim, LeafImpl[S, Elem, U]](BiGroup.MaxSquare)

    protected final def readTree(in: DataInput, access: S#Acc)(implicit tx: S#Tx): TreeImpl[S, Elem, U] =
      SkipOctree.read[S, TwoDim, LeafImpl[S, Elem, U]](in, access)

    final def treeHandle = tree

    override def toString() = s"BiGroup${tree.id}"

    def modifiableOption: Option[BiGroup.Modifiable[S, Elem, U]] = Some(this)

    implicit object TimedSer extends evt.NodeSerializer[S, TimedElemImpl[S, Elem, U]] {
      def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): TimedElemImpl[S, Elem, U] = {
        val span  = bitemp.SpanLike .read(in, access)
        val value = elemSerializer  .read(in, access)
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

      override def toString() = s"$node.CollectionEvent"
      final val slot = 0
      def node: BiGroup[S, Elem, U] = group
    }

    private object ElementEvent
      extends evti.EventImpl [S, BiGroup.Update[S, Elem, U], BiGroup[S, Elem, U]]
      with evt.InvariantEvent[S, BiGroup.Update[S, Elem, U], BiGroup[S, Elem, U]] {

      protected def reader: evt.Reader[S, BiGroup[S, Elem, U]] = serializer(eventView)

      override def toString() = s"$node.ElementEvent"
      final val slot = 1
      def node: BiGroup[S, Elem, U] = group

      private def foreach(fun: TimedElemImpl[S, Elem, U] => Unit)(implicit tx: S#Tx): Unit =
        tree.iterator.foreach {
          case (_, seq) => seq.foreach(fun)
        }

      def connect()(implicit tx: S#Tx): Unit = {
        log(s"$this.connect()")
        foreach(+=)
      }

      def disconnect()(implicit tx: S#Tx): Unit = {
        log(s"$this.disconnect()")
        foreach(-=)
      }

      def +=(elem: TimedElemImpl[S, Elem, U])(implicit tx: S#Tx): Unit =
        elem ---> (this: evt.Selector[S])

      def -=(elem: TimedElemImpl[S, Elem, U])(implicit tx: S#Tx): Unit =
        elem -/-> (this: evt.Selector[S])

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[BiGroup.Update[S, Elem, U]] = {
        val par = pull.parents(this)
        log(s"$this.pullUpdate -> parents = $par")

        val changes: Vec[BiGroup.Change[S, Elem, U]] = par.flatMap(sel => {
          val timed = sel.devirtualize(TimedSer).node.asInstanceOf[TimedElemImpl[S, Elem, U]]
          val ch0   = pull(timed).map(_.changes).getOrElse(Vector.empty)
          log(s"$this.pullUpdate -> from timed $timed pulled $ch0")
          ch0.foreach {
            case BiGroup.ElementMoved(_, m.Change(spanValOld, spanValNew)) =>
              assert(removeNoFire(spanValOld, timed))
              addNoFire          (spanValNew, timed)

            case _ =>
          }
          ch0
        })(breakOut)

        if (changes.isEmpty) None else Some(BiGroup.Update(group, changes))
      }
    }

    private object ChangeEvent
      extends evt.impl.EventImpl[S, BiGroup.Update[S, Elem, U], BiGroup.Modifiable[S, Elem, U]]
      with evt.InvariantEvent   [S, BiGroup.Update[S, Elem, U], BiGroup.Modifiable[S, Elem, U]] {

      protected def reader: evt.Reader[S, BiGroup.Modifiable[S, Elem, U]] = modifiableSerializer(eventView)

      override def toString() = s"$node.ChangeEvent"  // default toString invokes `slot`!
      final val slot = 2

      def node: BiGroup.Modifiable[S, Elem, U] = group

      def connect()(implicit tx: S#Tx): Unit = {
        log(s"$this.connect()")
        CollectionEvent ---> this
        ElementEvent    ---> this
      }

      def disconnect()(implicit tx: S#Tx): Unit = {
        log(s"$this.disconnect()")
        CollectionEvent -/-> this
        ElementEvent    -/-> this
      }

      // XXX TODO: potential problem with event collapsing
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[BiGroup.Update[S, Elem, U]] = {
        val collOpt = if (pull.contains(CollectionEvent)) pull(CollectionEvent) else None
        val elemOpt = if (pull.contains(ElementEvent   )) pull(ElementEvent   ) else None

        (collOpt, elemOpt) match {
          case (Some(_), None)      => collOpt
          case (None, Some(_))      => elemOpt
          case (Some(coll), Some(elem)) => Some(coll.copy(changes = coll.changes ++ elem.changes))
          case _                    => None
        }
      }
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = tree.dispose()

    final protected def writeData(out: DataOutput)      : Unit = tree.write(out)

    final def select(slot: Int /*, invariant: Boolean */): evt.Event[S, Any, Any] = (slot: @switch) match {
      case ChangeEvent.slot     => ChangeEvent
      case CollectionEvent.slot => CollectionEvent
      case ElementEvent.slot    => ElementEvent
    }

    // ---- collection behaviour ----

    @inline private def isConnected(implicit tx: S#Tx): Boolean = targets.nonEmpty

    final def clear()(implicit tx: S#Tx): Unit =
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

    final def add(span: Expr[S, SpanLike], elem: Elem)(implicit tx: S#Tx): TimedElem[S, Elem] = {
      log(s"$this.add($span, $elem); isConnected? $isConnected")
      val spanVal = span.value
      val tgt     = evt.Targets[S] // XXX partial?
      val timed   = new TimedElemImpl[S, Elem, U](group, tgt, span, elem)
      addNoFire(spanVal, timed)
      if (isConnected) {
        ElementEvent += timed
        CollectionEvent(BiGroup.Update(group, Vec(BiGroup.Added(spanVal, timed))))
      }
      timed
    }

    private def addNoFire(spanVal: SpanLike, timed: TimedElemImpl[S, Elem, U])(implicit tx: S#Tx): Unit = {
      val point = spanToPoint(spanVal)
      //if( showLog ) println( "add at point " + point )
      //         val entry   = (span, elem)
      tree.transformAt(point) {
        case None           => Some(spanVal -> Vec    (timed))
        case Some((_, seq)) => Some(spanVal -> (seq :+ timed))
      }
    }

    final def remove(span: Expr[S, SpanLike], elem: Elem)(implicit tx: S#Tx): Boolean = {
      val spanVal = span.value
      val point   = spanToPoint(spanVal)
      val timedO  = tree.get(point).flatMap {
        case (_, Vec(single)) =>
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
        CollectionEvent(BiGroup.Update(group, Vec(BiGroup.Removed(spanVal, timed))))
      }

      timedO.isDefined
    }

    private def removeNoFire(spanVal: SpanLike, timed: TimedElemImpl[S, Elem, U])(implicit tx: S#Tx): Boolean = {
      val point = spanToPoint(spanVal)
      val entry = tree.get(point)
      entry match {
        case Some((_, Vec(single))) =>
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

    final def debugList(implicit tx: S#Tx): List[(SpanLike, Elem)] =
      tree.toList.flatMap {
        case (span, seq) => seq.map(span -> _.value)
      }

    final def debugPrint(implicit tx: S#Tx): String = tree.debugPrint()

    final def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Leaf[S, Elem]] = tree.iterator

    final def intersect(time: Long)(implicit tx: S#Tx): Iterator[S#Tx, Leaf[S, Elem]] =
      BiGroupImpl.intersectTime(tree)(time)

    final def intersect(span: SpanLike)(implicit tx: S#Tx): Iterator[S#Tx, Leaf[S, Elem]] =
      BiGroupImpl.intersectSpan(tree)(span)

    final def rangeSearch(start: SpanLike, stop: SpanLike)(implicit tx: S#Tx): Iterator[S#Tx, Leaf[S, Elem]] =
      BiGroupImpl.rangeSearch(tree)(start, stop)

    // this can be easily implemented with two rectangular range searches
    final def eventsAt(time: Long)(implicit tx: S#Tx): (Iterator[S#Tx, Leaf[S, Elem]], Iterator[S#Tx, Leaf[S, Elem]]) =
      BiGroupImpl.eventsAt(tree)(time)

    final def nearestEventAfter(time: Long)(implicit tx: S#Tx): Option[Long] =
      BiGroupImpl.nearestEventAfter(tree)(time)

    final def nearestEventBefore(time: Long)(implicit tx: S#Tx): Option[Long] =
      BiGroupImpl.nearestEventBefore(tree)(time)

    //      final def collectionChanged : Event[ S, BiGroup.Collection[ S, Elem, U ], BiGroup[ S, Elem, U ]] = CollectionEvent
    //      final def elementChanged    : Event[ S, BiGroup.Element[    S, Elem, U ], BiGroup[ S, Elem, U ]] = ElementEvent
    final def changed: Event[S, BiGroup.Update[S, Elem, U], BiGroup.Modifiable[S, Elem, U]] = ChangeEvent
  }

  def newModifiable[S <: Sys[S], Elem, U](eventViewFun: Elem => EventLike[S, U])(
      implicit tx: S#Tx, _elemSerializer: Serializer[S#Tx, S#Acc, Elem]): Modifiable[S, Elem, U] =
    new Impl[S, Elem, U] {
      group =>

      def eventView(elem: Elem): EventLike[S, U] = eventViewFun(elem)

      val elemSerializer: Serializer[S#Tx, S#Acc, Elem] = _elemSerializer

      protected val targets = evt.Targets[S]

      val tree: TreeImpl[S, Elem, U] = newTree()
    }

  private def read[S <: Sys[S], Elem, U](in: DataInput, access: S#Acc, _targets: evt.Targets[S],
                                         eventViewFun: Elem => EventLike[S, U])
                                        (implicit tx: S#Tx,
                                          _elemSerializer: Serializer[S#Tx, S#Acc, Elem]): Impl[S, Elem, U] =
    new Impl[S, Elem, U] {
      def eventView(elem: Elem): EventLike[S, U] = eventViewFun(elem)

      implicit def elemSerializer: Serializer[S#Tx, S#Acc, Elem] = _elemSerializer

      protected val targets: evt.Targets[S] = _targets

      val tree: TreeImpl[S, Elem, U] = readTree(in, access)
    }
}
