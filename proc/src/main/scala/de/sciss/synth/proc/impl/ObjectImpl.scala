/*
 *  ObjectImpl.scala
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

import de.sciss.lucre.{event => evt}
import evt.{Event, Sys}
import de.sciss.serial.{Serializer, DataOutput, ImmutableSerializer, DataInput}
import de.sciss.lucre.data.SkipList
import scala.collection.breakOut
import scala.annotation.switch
import de.sciss.lucre.synth.InMemory
import scala.language.higherKinds
import scala.reflect.ClassTag

object ObjectImpl {
  def apply[S <: Sys[S], E1 <: Elem[S]](elem: E1)(implicit tx: S#Tx): Obj[S] { type E = E1 } = {
    val targets = evt.Targets[S]
    val map     = SkipList.Map.empty[S, String, AttrEntry[S, E1#PeerUpdate]]
    new Impl[S, E1](targets, elem, map)
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    serializer[S].read(in, access)

  def readT[S <: Sys[S], E1[~ <: Sys[~]] <: Elem[~]](in: DataInput, access: S#Acc)
                                                    (implicit tx: S#Tx,
                                                     peer: Serializer[S#Tx, S#Acc, E1[S]]): Obj[S] { type E = E1[S] } =
    typedSerializer[S, E1[S]].read(in, access)

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Obj[S]] = anySer.asInstanceOf[Ser[S]]

  implicit def typedSerializer[S <: Sys[S], E1 <: Elem[S]](
    implicit peer: Serializer[S#Tx, S#Acc, E1]): evt.Serializer[S, Obj[S] { type E = E1 }] = new TypedSer[S, E1]

  // ---- implementation ----

  private type I = InMemory

  private type AttrEntry[S <: Sys[S], Upd] = KeyMapImpl.Entry[S, String, Elem[S], Elem.Update[S, Upd]]

  private val anySer = new Ser[I]

  private final class Ser[S <: Sys[S]] extends evt.NodeSerializer[S, Obj[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])
            (implicit tx: S#Tx): Obj[S] with evt.Node[S] = {
      val elem  = Elem.read(in, access)
      val map   = SkipList.Map.read[S, String, AttrEntry[S, Elem[S]#PeerUpdate]](in, access, SkipList.NoKeyObserver)
      new Impl[S, Elem[S]](targets, elem, map)
    }
  }

  private final class TypedSer[S <: Sys[S], E1 <: Elem[S]](implicit peer: Serializer[S#Tx, S#Acc, E1])
    extends evt.NodeSerializer[S, Obj[S] { type E = E1 }] {

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])
            (implicit tx: S#Tx): Obj[S] with evt.Node[S] { type E = E1 } = {
      val elem  = peer.read(in, access)
      val map   = SkipList.Map.read[S, String, AttrEntry[S, E1#PeerUpdate]](in, access, SkipList.NoKeyObserver)
      new Impl[S, E1](targets, elem, map)
    }
  }

  // XXX TODO: DRY - this is shared with ProcImpl
  implicit private def attributeEntryInfo[S <: Sys[S], Upd]: KeyMapImpl.ValueInfo[S, String, Elem[S], Elem.Update[S, Upd]] =
    anyAttrEntryInfo.asInstanceOf[KeyMapImpl.ValueInfo[S, String, Elem[S], Elem.Update[S, Upd]]]

  private val anyAttrEntryInfo = new KeyMapImpl.ValueInfo[I, String, Elem[I], Elem.Update[I, Any]] {
    def valueEvent(value: Elem[I]) = value.changed

    val keySerializer   = ImmutableSerializer.String
    val valueSerializer = Elem.serializer[I]
  }

  private final class Impl[S <: Sys[S], E1 <: Elem[S]](protected val targets: evt.Targets[S], val elem: E1,
                                                  attributeMap: SkipList.Map[S, String, AttrEntry[S, E1#PeerUpdate]])
    extends Obj[S] {
    obj =>

    type E = E1

    protected def disposeData()(implicit tx: S#Tx): Unit =
      attributeMap.dispose()

    protected def writeData(out: DataOutput): Unit = {
      elem        .write(out)
      attributeMap.write(out)
    }

    // ---- events ----

    sealed trait ObjectEvent {
      final protected def reader: evt.Reader[S, Obj[S]] = ObjectImpl.serializer
      final def node: Obj[S] with evt.Node[S] = obj
    }

    private object StateEvent
      extends evt.impl.TriggerImpl[S, Obj.UpdateT[S, E], Obj[S]]
      with evt.InvariantEvent     [S, Obj.UpdateT[S, E], Obj[S]]
      with evt.impl.Root          [S, Obj.UpdateT[S, E]]
      with ObjectEvent {

      final val slot = 2
    }

    object attr
      extends AttrMap.Modifiable[S]
      with evt.impl.EventImpl[S, Obj.UpdateT[S, E], Obj[S]]
      with evt.InvariantEvent[S, Obj.UpdateT[S, E], Obj[S]]
      with ObjectEvent
      with impl.KeyMapImpl[S, String, Elem[S], Elem.Update[S, E#PeerUpdate]] {

      final val slot = 0

      final protected def fire(added: Option[(String, Elem[S])], removed: Option[(String, Elem[S])])
                              (implicit tx: S#Tx): Unit = {
        val b = Vector.newBuilder[Obj.AttrUpdate[S]]
        // convention: first the removals, then the additions. thus, overwriting a key yields
        // successive removal and addition of the same key.
        removed.foreach { tup =>
          b += Obj.AttrRemoved(tup._1, tup._2)
        }
        added.foreach { tup =>
          b += Obj.AttrAdded(tup._1, tup._2)
        }
        StateEvent(Obj.UpdateT(obj, b.result()))
      }

      final protected def isConnected(implicit tx: S#Tx): Boolean = obj.targets.nonEmpty

      def put(key: String, value: Elem[S])(implicit tx: S#Tx): Unit = add(key, value)

      def contains(key: String)(implicit tx: S#Tx): Boolean = map.contains(key)

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Obj.UpdateT[S, E]] = {
        val changes = foldUpdate(pull)
        if (changes.isEmpty) None
        else Some(Obj.UpdateT(obj,
          changes.map({
            case (key, u) => Obj.AttrChange(key, u.element, u.change)
          })(breakOut)))
      }

      protected def map: SkipList.Map[S, String, Entry] = attributeMap

      protected def valueInfo = attributeEntryInfo[S, E#PeerUpdate]

      def apply[A[~ <: Sys[~]]](key: String)(implicit tx: S#Tx, tag: ClassTag[A[S]]): Option[A[S]] =
        get(key).flatMap { elem =>
          tag.unapply(elem.peer)
        }

//      def apply[A[~ <: Sys[~]] <: Elem[_]](key: String)(implicit tx: S#Tx,
//                                                        tag: reflect.ClassTag[A[S]]): Option[A[S]#Peer] =
//        get(key) match {
//          // cf. stackoverflow #16377741
//          case Some(attr) => tag.unapply(attr).map(_.peer) // Some(attr.peer)
//          case _          => None
//        }
    }

    private object ChangeEvent
      extends evt.impl.EventImpl[S, Obj.UpdateT[S, E], Obj[S]]
      with evt.InvariantEvent   [S, Obj.UpdateT[S, E], Obj[S]]
      with ObjectEvent {

      final val slot = 3

      def connect   ()(implicit tx: S#Tx): Unit = {
        attr         ---> this
        elem.changed ---> this
        StateEvent   ---> this
      }
      def disconnect()(implicit tx: S#Tx): Unit = {
        attr         -/-> this
        elem.changed -/-> this
        StateEvent   -/-> this
      }

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Obj.UpdateT[S, E]] = {
        val b = Vector.newBuilder[Obj.Change[S, E#PeerUpdate]]
        if (pull.contains(attr     )) pull(attr       ).foreach(e => b ++= e.changes)
        val elemCh = elem.changed
        if (pull.contains(elemCh))     pull(elemCh    ).foreach(e => b  += Obj.ElemChange(e.change))
        if (pull.contains(StateEvent)) pull(StateEvent).foreach(e => b ++= e.changes)

        val changes = b.result()
        if (changes.nonEmpty) Some(Obj.UpdateT(obj, changes)) else None
      }
    }

    def select(slot: Int): Event[S, Any, Any] = (slot: @switch) match {
      case ChangeEvent.slot => ChangeEvent
      case attr       .slot => attr
      case StateEvent .slot => StateEvent
    }

    def changed: Event[S, Obj.UpdateT[S, E], Obj[S]] = ChangeEvent
  }
}