/*
 *  BiPinImpl.scala
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
package impl

import de.sciss.lucre.{event => evt}
import evt.{Event, EventLike, impl => evti, Sys}
import data.SkipList
import collection.immutable.{IndexedSeq => Vec}
import collection.breakOut
import annotation.switch
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.model
import expr.ExprType1

object BiPinImpl {
  import BiPin.{Leaf, Modifiable}

  private type Tree[S <: Sys[S], A] = SkipList.Map[S, Long, Leaf[S, A]]

  // ~private def opNotSupported: Nothing = sys.error("Operation not supported")

  private implicit def leafSerializer[S <: Sys[S], A](implicit biType: ExprType1[A]): Serializer[S#Tx, S#Acc, Leaf[S, A]] =
    new LeafSer

  private final class LeafSer[S <: Sys[S], A](implicit biType: ExprType1[A]) extends Serializer[S#Tx, S#Acc, Leaf[S, A]] {
    def write(leaf: BiPin.Leaf[S, A], out: DataOutput): Unit = {
      val sz = leaf.size
      out.writeInt(sz)
      if (sz == 0) return
      leaf.foreach(_.write(out))
    }

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): BiPin.Leaf[S, A] = {
      val sz = in.readInt()
      if (sz == 0) return Vec.empty

      val elemSer = BiExpr.serializer[S, A]
      Vec.fill(sz)(elemSer.read(in, access))
    }
  }

  def newModifiable[S <: Sys[S], A](implicit tx: S#Tx, biType: ExprType1[A]): Modifiable[S, A] = {
    val tree: Tree[S, A] = SkipList.Map.empty[S, Long, Leaf[S, A]]()
    new Impl(evt.Targets.partial[S], tree) // XXX TODO partial?
  }

  def serializer[S <: Sys[S], A](implicit biType: ExprType1[A]): evt.Serializer[S, BiPin[S, A]] =
    new Ser[S, A, BiPin[S, A]]

  def modifiableSerializer[S <: Sys[S], A](implicit biType: ExprType1[A]): evt.Serializer[S, BiPin.Modifiable[S, A]] =
    new Ser[S, A, BiPin.Modifiable[S, A]]

  def readModifiable[S <: Sys[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx, biType: ExprType1[A]): BiPin.Modifiable[S, A] = {
    val targets = evt.Targets.read[S](in, access)
    readImpl(in, access, targets)
  }

  def read[S <: Sys[S], A](in: DataInput, access: S#Acc)(implicit tx: S#Tx, biType: ExprType1[A]): BiPin[S, A] =
    readModifiable(in, access)

  private def readImpl[S <: Sys[S], A](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                      (implicit tx: S#Tx, biType: ExprType1[A]): Impl[S, A] = {
    val tree: Tree[S, A] = SkipList.Map.read[S, Long, Leaf[S, A]](in, access)
    new Impl(targets, tree)
  }

  private class Ser[S <: Sys[S], A, Repr >: Impl[S, A] <: BiPin[S, A]](implicit biType: ExprType1[A])
    extends Serializer[S#Tx, S#Acc, Repr] with evt.Reader[S, Repr] {

    def write(v: Repr, out: DataOutput): Unit = v.write(out)

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Repr with evt.Node[S] = {
      BiPinImpl.readImpl(in, access, targets)
    }

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Repr = {
      val targets = evt.Targets.read(in, access)
      read(in, access, targets)
    }
  }

  //   private class ModSer[ S <: Sys[ S ], A ]( implicit biType: ExprType1[ A ])
  //   extends evt.NodeSerializer[ S, BiPin.Modifiable[ S, A ]] {
  //      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : BiPin.Modifiable[ S, A ] with evt.Node[ S ] = {
  //         BiPinImpl.readImpl( in, access, targets)
  //      }
  //   }

  private final class Impl[S <: Sys[S], A](protected val targets: evt.Targets[S], tree: Tree[S, A])
                                          (implicit biType: ExprType1[A])
    extends Modifiable[S, A] with evt.Node[S] {
    //   with evt.Compound[ S, Impl[ S, A ], Impl.type ]
    //   with evt.Trigger.Impl[ S, BiPin.Update[ S, A ], BiPin.Update[ S, A ], BiPin[ S, A ]]
    //   with evt.StandaloneLike[ S, BiPin.Update[ S, A ], BiPin[ S, A ]]
    //   with evt.Node[ S ] {
    pin =>

    private type ElemChange = model.Change[(Long, A)]

    //      protected def tree: Tree[ S, A ]
    //      implicit protected def biType: ExprType1[ A ]

    override def toString() = "BiPin" + tree.id

    def modifiableOption: Option[BiPin.Modifiable[S, A]] = Some(this)

    // ---- event behaviour ----

    private object CollChanged
      extends evti.TriggerImpl[S, BiPin.Update[S, A], BiPin[S, A]]
      with evt.InvariantEvent[S, BiPin.Update[S, A], BiPin[S, A]]
      with evti.Root[S, BiPin.Update[S, A]] {
      protected def reader: evt.Reader[S, BiPin[S, A]] = serializer

      final val slot = 0

      def node: BiPin[S, A] with evt.Node[S] = pin

      //         def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ BiPin.Collection[ S, A ]] = {
      //            pull.resolve[ BiPin.Collection[ S, A ]]
      //         }
    }

    private object ElemChanged
      extends evti.EventImpl[S, BiPin.Update[S, A], BiPin[S, A]]
      with evt.InvariantEvent[S, BiPin.Update[S, A], BiPin[S, A]] {
      protected def reader: evt.Reader[S, BiPin[S, A]] = serializer

      final val slot = 1

      def node: BiPin[S, A] with evt.Node[S] = pin

      //         def connect()(implicit tx: S#Tx) = ()
      //         def disconnect()(implicit tx: S#Tx) = ()

      private def foreach(fun: Elem => Unit)(implicit tx: S#Tx): Unit =
        tree.iterator.foreach {
          case (_, seq) => seq.foreach(fun)
        }

      def connect   ()(implicit tx: S#Tx): Unit = foreach(+=) // XXX TODO: verify point in time
      def disconnect()(implicit tx: S#Tx): Unit = foreach(-=)

      // call this under the assumption that the event is connected
      def +=(elem: Elem)(implicit tx: S#Tx): Unit = elem.changed ---> this

      // call this under the assumption that the event is connected
      def -=(elem: Elem)(implicit tx: S#Tx): Unit = elem.changed -/-> this

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[BiPin.Update[S, A]] = {
        val changes: Vec[BiPin.Element[S, A]] = pull.parents(this).flatMap(sel => {
          // wow... how does this get the event update type right I'm wondering... ?
          // UPDATE: ha! it doesn't. hell, this produces a runtime exception re Nothing??
          // --> fix: evt needs type ascription!!!
          val e = sel.devirtualize[ElemChange, Elem](BiExpr.serializer[S, A])
          val elem = e.node
          val opt: Option[BiPin.Element[S, A]] = pull(e).map(BiPin.Element(elem, _))
          opt
        })(breakOut)

        if (changes.isEmpty) None
        else {
          changes.foreach {
            case BiPin.Element(elem, change) =>
              val (timeChange, _) = change.unzip
              if (timeChange.isSignificant) {
                removeNoFire(timeChange.before, elem)
                addNoFire   (timeChange.now   , elem)
              }
          }
          Some(BiPin.Update(pin, changes))
        }
      }
    }

    private object Changed
      extends evt.impl.EventImpl[S, BiPin.Update[S, A], BiPin[S, A]]
      with evt.InvariantEvent   [S, BiPin.Update[S, A], BiPin[S, A]] {

      protected def reader: evt.Reader[S, BiPin[S, A]] = serializer

      // def slot: Int = opNotSupported
      final val slot = 2

      def node: BiPin[S, A] with evt.Node[S] = pin

      def connect()(implicit tx: S#Tx): Unit = {
        CollChanged ---> this
        ElemChanged ---> this
      }

      def disconnect()(implicit tx: S#Tx): Unit = {
        CollChanged -/-> this
        ElemChanged -/-> this
      }

      //      def --->(r: evt.Selector[S])(implicit tx: S#Tx): Unit = {
      //        CollChanged ---> r
      //        ElemChanged ---> r
      //      }
      //
      //      def -/->(r: evt.Selector[S])(implicit tx: S#Tx): Unit = {
      //        CollChanged -/-> r
      //        ElemChanged -/-> r
      //      }

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[BiPin.Update[S, A]] = {
        val collOpt = if (pull.contains(CollChanged)) pull(CollChanged) else None
        val elemOpt = if (pull.contains(ElemChanged)) pull(ElemChanged) else None

        (collOpt, elemOpt) match {
          case (Some(_), None) => collOpt
          case (None, Some(_)) => elemOpt
          case (Some(BiPin.Update(_, coll)), Some(BiPin.Update(_, elem))) =>
            Some(BiPin.Update(pin, coll ++ elem))
          case _ => None
        }
      }
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = tree.dispose()
    protected def writeData(out: DataOutput)      : Unit = tree.write(out)

    def select(slot: Int /*, invariant: Boolean */): Event[S, Any, Any] = (slot: @switch) match {
      case Changed.slot     => Changed
      case CollChanged.slot => CollChanged
      case ElemChanged.slot => ElemChanged
    }

    // ---- collection behaviour ----

    @inline private def isConnected(implicit tx: S#Tx): Boolean = targets.nonEmpty

    def clear()(implicit tx: S#Tx): Unit =
      if (isConnected) {
        val it = tree.iterator
        if (it.hasNext) {
          val changes = it.toIndexedSeq.flatMap {
            case (spanVal, seq) =>
             seq.map { elem =>
               BiPin.Removed[S, A](spanVal -> elem.magValue, elem)
             }
          }
          tree.clear()
          CollChanged(BiPin.Update(this, changes))
        }

      } else {
        tree.clear()
      }

    def add(elem: Elem)(implicit tx: S#Tx): Unit = {
      val timeVal = elem.timeValue
      addNoFire(timeVal, elem)
      if (isConnected) {
        //            CollChanged += time
        ElemChanged += elem
        CollChanged(BiPin.Update(pin, Vec(BiPin.Added(timeVal -> elem.magValue, elem))))
      }
    }

    def intersect(time: Long)(implicit tx: S#Tx): Leaf[S, A] = tree.floor(time) match {
      case Some((_, seq)) => seq
      case _ => Vec.empty
    }

    def nearestEventAfter(time: Long)(implicit tx: S#Tx): Option[Long] = tree.ceil(time).map(_._1)

    def at     (time: Long)(implicit tx: S#Tx): Option[Elem] = intersect(time).headOption
    def valueAt(time: Long)(implicit tx: S#Tx): Option[A]    = intersect(time).headOption.map(_.magValue)
    def floor  (time: Long)(implicit tx: S#Tx): Option[Elem] = tree.floor(time).flatMap(_._2.headOption)
    def ceil   (time: Long)(implicit tx: S#Tx): Option[Elem] = tree.ceil (time).flatMap(_._2.headOption)

    /**
     * Adds a new value, and returns the dirty which corresponds to the new region holding `elem`.
     *
     * @param timeVal the time value at which the new element is inserted
     * @param elem    the element which is inserted
     * @return
     */
    private def addNoFire(timeVal: Long, elem: Elem)(implicit tx: S#Tx): Unit =
      tree.get(timeVal) match {
        case Some(oldLeaf) =>
          tree += timeVal -> (elem +: oldLeaf)
        case _ =>
          tree += timeVal -> Vec(elem)
      }

    def remove(elem: Elem)(implicit tx: S#Tx): Boolean = {
      val timeVal = elem.timeValue
      val (found, visible) = removeNoFire(timeVal, elem)
      if (visible && isConnected) {
        //            CollChanged -= time
        ElemChanged -= elem
        CollChanged(BiPin.Update(pin, Vec(BiPin.Removed(timeVal -> elem.magValue, elem))))
      }
      found
    }

    private def removeNoFire(timeVal: Long, elem: Elem)(implicit tx: S#Tx): (Boolean, Boolean) = {
      tree.get(timeVal) match {
        case Some(Vec(single)) =>
          val found = single == elem
          if (found) tree -= timeVal
          (found, found)

        case Some(seq) =>
          val i       = seq.indexOf(elem)
          val found   = i >= 0
          val visible = i == 0
          if (found) {
            val seqNew = seq.patch(i, Vec.empty[Elem], 1)
            tree += timeVal -> seqNew
          }
          (found, visible)

        case None => (false, false)
      }
    }

    def debugList()(implicit tx: S#Tx): List[(Long, A)] =
      tree.toList.flatMap {
        case (time, seq) => seq.map(time -> _.magValue)
      }

    def changed: EventLike[S, BiPin.Update[S, A]] = Changed
  }
}
