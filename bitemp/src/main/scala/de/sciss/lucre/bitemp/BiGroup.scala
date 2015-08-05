/*
 *  BiGroup.scala
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
package bitemp

import de.sciss.lucre.bitemp.impl.{BiGroupImpl => Impl}
import de.sciss.lucre.data.Iterator
import de.sciss.lucre.event.{EventLike, Publisher, Sys}
import de.sciss.lucre.expr.{Expr, ExprType1}
import de.sciss.lucre.geom.LongSquare
import de.sciss.lucre.stm.Identifiable
import de.sciss.lucre.{event => evt}
import de.sciss.serial.DataInput
import de.sciss.span.{SpanLike => SpanLikeV}
import de.sciss.{model => m, serial}

import scala.collection.immutable.{IndexedSeq => Vec}

object BiGroup {
  final val MaxSquare     = LongSquare(0, 0, 0x2000000000000000L)
  final val MaxSide       = MaxSquare.side
  final val MinCoordinate = MaxSquare.left
  final val MaxCoordinate = MaxSquare.right

  // ---- updates ----

  final case class Update[S <: Sys[S], Elem, U](group: BiGroup[S, Elem, U], changes: Vec[Change[S, Elem, U]])

  sealed trait Change[S <: Sys[S], +Elem, +U]

  sealed trait Collection[S <: Sys[S], +Elem] extends Change[S, Elem, Nothing] {
    def elem: TimedElem[S, Elem]
    def span: SpanLikeV // Span.HasStart
  }

  final case class Added[S <: Sys[S], Elem](span: SpanLikeV /* Span.HasStart */ , elem: TimedElem[S, Elem])
    extends Collection[S, Elem]

  final case class Removed[S <: Sys[S], Elem](span: SpanLikeV /* Span.HasStart */ , elem: TimedElem[S, Elem])
    extends Collection[S, Elem]

  sealed trait Element[S <: Sys[S], Elem, +U] extends Change[S, Elem, U] {
    def elem: TimedElem[S, Elem]
  }

  final case class ElementMoved[S <: Sys[S], Elem](elem: TimedElem[S, Elem], change: m.Change[SpanLikeV])
    extends Element[S, Elem, Nothing]

  final case class ElementMutated[S <: Sys[S], Elem, U](elem: TimedElem[S, Elem], change: U)
    extends Element[S, Elem, U]

  // ---- structural data ----

  type Leaf[S <: Sys[S], +Elem] = (SpanLikeV /* Span.HasStart */ , Vec[TimedElem[S, Elem]])

  object TimedElem {
    def apply[S <: Sys[S], Elem](id: S#ID, span: Expr[S, SpanLikeV], value: Elem): TimedElem[S, Elem] =
      Wrapper(id, span, value)

    private final case class Wrapper[S <: Sys[S], Elem](id: S#ID, span: Expr[S, SpanLikeV], value: Elem)
      extends TimedElem[S, Elem] {

      override def toString = s"TimedElem$id"

      override def equals(that: Any): Boolean = that match {
        case m: Identifiable[_] => this.id == m.id
        case _ => super.equals(that)
      }

      //      override def equals(that: Any): Boolean = that match {
      //        case m: TimedElem[_, _] =>
      //          id == m.id
      //        case _ => super.equals(that)
      //      }

      override def hashCode = id.hashCode()
    }
  }

  trait TimedElem[S <: Sys[S], +Elem] extends Identifiable[S#ID] {
    def span : Expr[S, SpanLikeV]
    def value: Elem

    override def toString = s"TimedElem($id, $span, $value)"
  }

  object Expr {
    def serializer[S <: Sys[S], A](implicit elemType: ExprType1[A]): serial.Serializer[S#Tx, S#Acc, BiGroup[S, Expr[S, A], m.Change[A]]] with evt.Reader[S, BiGroup[S, Expr[S, A], m.Change[A]]] =
      Impl.serializer[S, Expr[S, A], m.Change[A]](_.changed)(elemType.serializer[S])

    object Modifiable {
      def serializer[S <: Sys[S], A](implicit elemType: ExprType1[A]): serial.Serializer[S#Tx, S#Acc, BiGroup.Modifiable[S, Expr[S, A], m.Change[A]]] with evt.Reader[S, BiGroup.Modifiable[S, Expr[S, A], m.Change[A]]] =
        Impl.modifiableSerializer[S, Expr[S, A], m.Change[A]](_.changed)(elemType.serializer[S])

      def apply[S <: Sys[S], A](implicit tx: S#Tx, elemType: ExprType1[A]): Modifiable[S, Expr[S, A], m.Change[A]] =
        Impl.newModifiable[S, Expr[S, A], m.Change[A]](_.changed)(tx, elemType.serializer[S])

      def read[S <: Sys[S], A](in: DataInput, access: S#Acc)
                              (implicit tx: S#Tx, elemType: ExprType1[A]): Modifiable[S, Expr[S, A], m.Change[A]] =
        Impl.readModifiable[S, Expr[S, A], m.Change[A]](in, access, _.changed)(tx, elemType.serializer[S])
    }
  }

  object Modifiable {
    def serializer[S <: Sys[S], Elem, U](eventView: Elem => EventLike[S, U])
        (implicit elemSerializer: serial.Serializer[S#Tx, S#Acc, Elem]): serial.Serializer[S#Tx, S#Acc, BiGroup.Modifiable[S, Elem, U]] with evt.Reader[S, BiGroup.Modifiable[S, Elem, U]] =
      Impl.modifiableSerializer[S, Elem, U](eventView)

    def apply[S <: Sys[S], Elem, U](eventView: Elem => EventLike[S, U])
        (implicit tx: S#Tx, elemSerializer: serial.Serializer[S#Tx, S#Acc, Elem]): Modifiable[S, Elem, U] =
      Impl.newModifiable(eventView)

    def read[S <: Sys[S], Elem, U](in: DataInput, access: S#Acc, eventView: Elem => EventLike[S, U])
        (implicit tx: S#Tx, elemSerializer: serial.Serializer[S#Tx, S#Acc, Elem]): Modifiable[S, Elem, U] =
      Impl.readModifiable(in, access, eventView)
  }

  trait Modifiable[S <: Sys[S], Elem, U] extends BiGroup[S, Elem, U] {
    def add(span: Expr[S, SpanLikeV], elem: Elem)(implicit tx: S#Tx): TimedElem[S, Elem]

    def remove(span: Expr[S, SpanLikeV], elem: Elem)(implicit tx: S#Tx): Boolean

    def clear()(implicit tx: S#Tx): Unit

    override def changed: EventLike[S, BiGroup.Update[S, Elem, U]]
  }

  def serializer[S <: Sys[S], Elem, U](eventView: Elem => EventLike[S, U])
                                      (implicit elemSerializer: serial.Serializer[S#Tx, S#Acc, Elem])
  : serial.Serializer[S#Tx, S#Acc, BiGroup[S, Elem, U]] with evt.Reader[S, BiGroup[S, Elem, U]] =
    Impl.serializer[S, Elem, U](eventView)
}

trait BiGroup[S <: Sys[S], Elem, U] extends evt.Node[S] with Publisher[S, BiGroup.Update[S, Elem, U]] {

  import BiGroup.Leaf

  def modifiableOption: Option[BiGroup.Modifiable[S, Elem, U]]

  def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Leaf[S, Elem]]

  /** Queries all elements intersecting a given point in time.
    * That is, returns an iterator of all elements whose span contains the time point
    * `(span start <= time && span.stop > time)`
    *
    * This methods makes no guarantees about the ordering of the returned iterator.
    *
    * @param time the point in time to search at
    * @return  a (possibly empty) iterator of the intersecting elements
    */
  def intersect(time: Long)(implicit tx: S#Tx): Iterator[S#Tx, Leaf[S, Elem]]

  /** Queries all elements intersecting a given time span.
    * That is, returns an iterator of all elements whose span contains or partly overlaps the query span.
    * `(span start < query.stop && span.stop > query.start)`
    *
    * This methods makes no guarantees about the ordering of the returned iterator.
    *
    * @param span the the span to search within (this may be a half-bounded interval or even `Span.All`)
    * @return  a (possibly empty) iterator of the intersecting elements
    */
  def intersect(span: SpanLikeV)(implicit tx: S#Tx): Iterator[S#Tx, Leaf[S, Elem]]

  /** Performs a range query according to separate intervals for the allowed start and stop positions
    * of the element spans. That is, returns an iterator of all elements whose span satisfies the
    * constraints given for start and stop positions
    * `(start.contains( elem.span.start ) && stop.contains( elem.span.stop ))`
    *
    * Both for the start and stop constraint, half-bounded or unbounded (`Span.All`) intervals can be used.
    * Examples
    *
    * - to find all elements which start between 10 (inclusive) and 20 (exclusive), use `start = Span( 10, 20 ), stop = Span.All`.
    * - to find all elements which start before (<) 10 and stop from (>=) 20, use `start = Span.until( 10 ), stop = Span.from( 20 )`.
    *
    * This methods makes no guarantees about the ordering of the returned iterator.
    *
    * @param start   the constraint for the start position of the spans of the elements filtered.
    * @param stop    the constraint for the stop position of the spans of the elements filtered.
    * @return  a (possibly empty) iterator of the intersecting elements
    */
  def rangeSearch(start: SpanLikeV, stop: SpanLikeV)(implicit tx: S#Tx): Iterator[S#Tx, Leaf[S, Elem]]

  /** Queries the closest event (an element's span starting or stopping) at the given time or later
    *
    * @param time the query time
    * @return a time, greater than or equal to the query time, at which the next event occurs, or `None` if
    *         there are no events at or after the query time
    */
  def eventAfter(time: Long)(implicit tx: S#Tx): Option[Long]

  /** Queries the closest event (an element's span starting or stopping) at the given time or earlier
    *
    * @param time the query time
    * @return a time, smaller than or equal to the query time, at which the previous event occurs, or `None` if
    *         there are no events at or before the query time
    */
  def eventBefore(time: Long)(implicit tx: S#Tx): Option[Long]

  /** Finds the first occurring event, if there is any. Ignores objects with `Span.All`. */
  def firstEvent(implicit tx: S#Tx): Option[Long]

  /** Finds the last occurring event, if there is any. Ignores objects with `Span.All`. */
  def lastEvent (implicit tx: S#Tx): Option[Long]

  /** Queries all elements which produce an event (starting or stopping) at a given time.
    *
    * @param time the time instant for which to gather the events
    * @return  a tuple of two iterators. the first iterator (`_1`) contains the events which
    *          start at the query time, the second iterator (`_2`) contains the event which
    *          stop at the query time
    */
  def eventsAt(time: Long)(implicit tx: S#Tx): (Iterator[S#Tx, Leaf[S, Elem]], Iterator[S#Tx, Leaf[S, Elem]])

  def debugList(implicit tx: S#Tx): List[(SpanLikeV, Elem)]

  def debugPrint(implicit tx: S#Tx): String
}