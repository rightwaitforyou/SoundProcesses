package de.sciss.synth.proc
package impl

import de.sciss.lucre.{event => evt}
import evt.Sys
import de.sciss.serial.DataInput
import de.sciss.lucre.data.SkipList
import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.higherKinds

object ObjectImpl {
  def apply[S <: Sys[S]](elem: Elem[S])(implicit tx: S#Tx): Object[S] = ???

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Object[S] =
    serializer[S].read(in, access)

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Object[S]] = ???

  // ---- implementation ----

  private type AttrEntry[S <: Sys[S]] = KeyMapImpl.Entry[S, String, Elem[S], Elem.Update[S]]

  private sealed trait Impl[S <: Sys[S]]
    extends Object[S] {
    obj =>

    protected def attributeMap: SkipList.Map[S, String, AttrEntry[S]]

    sealed trait ObjectEvent {
      final protected def reader: evt.Reader[S, Object[S]] = ObjectImpl.serializer
      final def node: Object[S] with evt.Node[S] = obj
    }

    sealed trait KeyMap[Value, ValueUpd, OuterUpd]
      extends evt.impl.EventImpl [S, OuterUpd, Object[S]]
      with evt.InvariantEvent[S, OuterUpd, Object[S]]
      with ObjectEvent
      with impl.KeyMapImpl[S, String, Value, ValueUpd] {

      protected def wrapKey(key: String): Proc.AssociativeKey

      // ---- keymapimpl details ----

      final protected def fire(added: Set[String], removed: Set[String])(implicit tx: S#Tx): Unit = {
        val seqAdd: Vec[Proc.StateChange[S]] = added  .map(key => Proc.AssociationAdded  [S](wrapKey(key)))(breakOut)
        val seqRem: Vec[Proc.StateChange[S]] = removed.map(key => Proc.AssociationRemoved[S](wrapKey(key)))(breakOut)
        // convention: first the removals, then the additions. thus, overwriting a key yields
        // successive removal and addition of the same key.
        val seq = if (seqAdd.isEmpty) seqRem else if (seqRem.isEmpty) seqAdd else seqRem ++ seqAdd

        ??? // StateEvent(Object.Update(obj, seq))
      }

      final protected def isConnected(implicit tx: S#Tx): Boolean = obj.targets.nonEmpty
    }

    object attributes extends AttrMap.Modifiable[S] with KeyMap[Elem[S], Elem.Update[S], Object.Update[S]] {
      final val slot = 0

      def wrapKey(key: String): Proc.AssociativeKey = ???

      def put(key: String, value: Elem[S])(implicit tx: S#Tx): Unit = add(key, value)

      def contains(key: String)(implicit tx: S#Tx): Boolean = map.contains(key)

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Object.Update[S]] = {
        val changes = foldUpdate(pull)
        if (changes.isEmpty) None
        else Some(Object.Update(obj,
          changes.map({
            case (key, u) => Object.AttrChange(key, u.element, u.change)
          })(breakOut)))
      }

      protected def map: SkipList.Map[S, String, Entry] = attributeMap

      protected def valueInfo = ??? // attributeEntryInfo[S]

      def apply[A[~ <: Sys[~]] <: Elem[_]](key: String)(implicit tx: S#Tx,
                                                        tag: reflect.ClassTag[A[S]]): Option[A[S]#Peer] =
        get(key) match {
          // cf. stackoverflow #16377741
          case Some(attr) => tag.unapply(attr).map(_.peer) // Some(attr.peer)
          case _          => None
        }
    }
  }
}
