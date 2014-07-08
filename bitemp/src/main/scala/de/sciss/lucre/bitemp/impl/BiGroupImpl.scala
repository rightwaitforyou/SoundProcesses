/*
 *  BiGroupImpl.scala
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

package de.sciss.lucre
package bitemp.impl

import de.sciss.lucre.{event => evt}
import evt.{Event, EventLike, impl => evti, Sys}
import de.sciss.lucre.data.{DeterministicSkipOctree, SkipOctree, Iterator}
import collection.immutable.{IndexedSeq => Vec}
import collection.breakOut
import scala.annotation.{elidable, switch}
import geom.{LongDistanceMeasure2D, LongPoint2DLike, LongPoint2D, LongSquare, LongSpace}
import LongSpace.TwoDim
import expr.Expr
import de.sciss.span.{SpanLike, Span}
import de.sciss.serial.{DataOutput, DataInput, Serializer}
import de.sciss.lucre.geom.LongRectangle
import de.sciss.{model => m}
import de.sciss.lucre.bitemp.BiGroup

object BiGroupImpl {
  import BiGroup.{Leaf, TimedElem, Modifiable}

  var showLog = false

  @elidable(elidable.CONFIG) private def log(what: => String): Unit =
    if (showLog) println(s"<bigroup> $what")

  private val MAX_SQUARE  = LongSquare(0, 0, 0x2000000000000000L)
  private val MIN_COORD   = MAX_SQUARE.left
  private val MAX_COORD   = MAX_SQUARE.right
  private val MAX_SIDE    = MAX_SQUARE.side

  //   private final case class Entry[ Elem ]( )

  type LeafImpl[S <: Sys[S], Elem, U] = (SpanLike, Vec[TimedElemImpl[S, Elem, U]])
  type Tree    [S <: Sys[S], Elem, U] = SkipOctree[S, TwoDim, LeafImpl[S, Elem, U]]

  // private def opNotSupported: Nothing = sys.error("Operation not supported")

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

  // ... accepted are points with x > LRP || y > LRP ...
  private val advanceNNMetric = LongDistanceMeasure2D.nextSpanEvent(MAX_SQUARE)
  private val regressNNMetric = LongDistanceMeasure2D.prevSpanEvent(MAX_SQUARE)

  private[lucre] final class TimedElemImpl[S <: Sys[S], Elem, U](group          : Impl[S, Elem, U],
                                                          protected val targets : evt.Targets[S],
                                                          val span              : Expr[S, SpanLike],
                                                          val value             : Elem)
    extends evti.StandaloneLike[S, BiGroup.Update[S, Elem, U], TimedElem[S, Elem]] with TimedElem[S, Elem] {

    import group.{eventView, elemSerializer}

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

    protected def tree: Tree[S, Elem, U]
    def eventView(elem: Elem): EventLike[S, U]
    implicit def elemSerializer: Serializer[S#Tx, S#Acc, Elem]

    // ---- implemented ----

    protected final def newTree()(implicit tx: S#Tx): Tree[S, Elem, U] =
      SkipOctree.empty[S, TwoDim, LeafImpl[S, Elem, U]](MAX_SQUARE)

    protected final def readTree(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Tree[S, Elem, U] =
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

      override def toString() = s"$node..ElementEvent"
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
        log(s"$this.connect")
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
      val span  = tree.nearestNeighborOption(point, regressNNMetric).map(_._1).getOrElse(Span.Void)
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

  def newModifiable[S <: Sys[S], Elem, U](eventViewFun: Elem => EventLike[S, U])(
      implicit tx: S#Tx, _elemSerializer: Serializer[S#Tx, S#Acc, Elem]): Modifiable[S, Elem, U] =
    new Impl[S, Elem, U] {
      group =>

      def eventView(elem: Elem): EventLike[S, U] = eventViewFun(elem)

      val elemSerializer: Serializer[S#Tx, S#Acc, Elem] = _elemSerializer

      protected val targets = evt.Targets[S]

      val tree: Tree[S, Elem, U] = newTree()
    }

  private def read[S <: Sys[S], Elem, U](in: DataInput, access: S#Acc, _targets: evt.Targets[S],
                                         eventViewFun: Elem => EventLike[S, U])
                                        (implicit tx: S#Tx,
                                          _elemSerializer: Serializer[S#Tx, S#Acc, Elem]): Impl[S, Elem, U] =
    new Impl[S, Elem, U] {
      def eventView(elem: Elem): EventLike[S, U] = eventViewFun(elem)

      implicit def elemSerializer: Serializer[S#Tx, S#Acc, Elem] = _elemSerializer

      protected val targets: evt.Targets[S] = _targets

      val tree: Tree[S, Elem, U] = readTree(in, access)
    }
}
