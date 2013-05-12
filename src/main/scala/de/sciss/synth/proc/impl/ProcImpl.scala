/*
 *  ProcImpl.scala
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

package de.sciss.synth
package proc
package impl

import de.sciss.lucre.{event => evt, expr, data, bitemp}
import de.sciss.synth.expr.Doubles
import evt.{Event, impl => evti, Sys}
import bitemp.BiType
import data.SkipList
import annotation.switch
import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.serial.{DataOutput, ImmutableSerializer, DataInput}
import language.higherKinds

object ProcImpl {
  private final val SER_VERSION = 0x5072  // "Pr"

  implicit val paramType: BiType[Param] = Doubles

  def apply[S <: Sys[S]](implicit tx: S#Tx): Proc[S] = new New[S](tx)

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Proc[S] =
    serializer[S].read(in, access)

  def serializer[S <: Sys[S]]: evt.NodeSerializer[S, Proc[S]] =
    anySer.asInstanceOf[evt.NodeSerializer[S, Proc[S]]]

  final val emptyGraph: SynthGraph = SynthGraph {}

  private val anySer = new Serializer[evt.InMemory]

  private class Serializer[S <: Sys[S]] extends evt.NodeSerializer[S, Proc[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Proc[S] =
      new Read(in, access, targets, tx)
  }

  // private def opNotSupported: Nothing = sys.error("Operation not supported")

  // private type GraphemeEntry [S <: Sys[S]] = KeyMapImpl.Entry[S, String, Grapheme [S], Grapheme .Update[S]]
  private type ScanEntry     [S <: Sys[S]] = KeyMapImpl.Entry[S, String, Scan     [S], Scan     .Update[S]]
  private type AttributeEntry[S <: Sys[S]] = KeyMapImpl.Entry[S, String, Attribute[S], Attribute.Update[S]]

  private type I = evt.InMemory

  // implicit def graphemeEntryInfo[S <: Sys[S]]: KeyMapImpl.ValueInfo[S, String, Grapheme[S], Grapheme.Update[S]] =
  //   anyGraphemeEntryInfo.asInstanceOf[KeyMapImpl.ValueInfo[S, String, Grapheme[S], Grapheme.Update[S]]]
  //
  //  private val anyGraphemeEntryInfo = new KeyMapImpl.ValueInfo[I, String, Grapheme[I], Grapheme.Update[I]] {
  //    def valueEvent(value: Grapheme[I]) = value.changed
  //
  //    val keySerializer   = ImmutableSerializer.String
  //    val valueSerializer = Grapheme.serializer[I]
  //  }

  implicit def scanEntryInfo[S <: Sys[S]]: KeyMapImpl.ValueInfo[S, String, Scan[S], Scan.Update[S]] =
    anyScanEntryInfo.asInstanceOf[KeyMapImpl.ValueInfo[S, String, Scan[S], Scan.Update[S]]]

  private val anyScanEntryInfo = new KeyMapImpl.ValueInfo[I, String, Scan[I], Scan.Update[I]] {
    def valueEvent(value: Scan[I]) = value.changed

    val keySerializer = ImmutableSerializer.String
    val valueSerializer = Scan.serializer[I]
  }

  implicit def attributeEntryInfo[S <: Sys[S]]: KeyMapImpl.ValueInfo[S, String, Attribute[S], Attribute.Update[S]] =
     anyAttributeEntryInfo.asInstanceOf[KeyMapImpl.ValueInfo[S, String, Attribute[S], Attribute.Update[S]]]

  private val anyAttributeEntryInfo = new KeyMapImpl.ValueInfo[I, String, Attribute[I], Attribute.Update[I]] {
    def valueEvent(value: Attribute[I]) = value.changed

    val keySerializer   = ImmutableSerializer.String
    val valueSerializer = Attribute.serializer[I]
  }

  private sealed trait Impl[S <: Sys[S]]
    extends Proc[S] {
    proc =>

    import Proc._

    protected def _graph    : S#Var[SynthGraph] // Code[SynthGraph]]
    // protected def name_#     : Expr.Var    [S, String]
    // protected def graphemeMap : SkipList.Map[S, String, GraphemeEntry[S]]
    protected def attributeMap: SkipList.Map[S, String, AttributeEntry[S]]
    protected def scanMap     : SkipList.Map[S, String, ScanEntry[S]]

    // final def name(implicit tx: S#Tx): Expr[S, String] = name_#()
    // final def name_=(s: Expr[S, String])(implicit tx: S#Tx) {
    //   name_#() = s
    // }

    final def graph(implicit tx: S#Tx): /*Code[*/ SynthGraph /*]*/ = _graph()

    final def graph_=(g: /* Code[*/ SynthGraph /*]*/)(implicit tx: S#Tx) {
      val old = _graph()
      if (old != g) {
        _graph() = g
        StateEvent(Proc.Update(proc, Vector(GraphChange(evt.Change(old /* .value */, g /* .value */ )))))
      }
    }

    // ---- key maps ----

    sealed trait ProcEvent {
      final protected def reader: evt.Reader[S, Proc[S]] = ProcImpl.serializer
      final def node: Proc[S] with evt.Node[S] = proc
    }

    sealed trait KeyMap[Value, ValueUpd, OuterUpd]
      extends evti.EventImpl [S, OuterUpd, Proc[S]]
      with evt.InvariantEvent[S, OuterUpd, Proc[S]]
      with ProcEvent
      with impl.KeyMapImpl[S, String, Value, ValueUpd] {
      protected def wrapKey(key: String): Proc.AssociativeKey

      // ---- keymapimpl details ----

      final protected def fire(added: Set[String], removed: Set[String])(implicit tx: S#Tx) {
        val seqAdd: IIdxSeq[Proc.StateChange] = added  .map(key => Proc.AssociationAdded  (wrapKey(key)))(breakOut)
        val seqRem: IIdxSeq[Proc.StateChange] = removed.map(key => Proc.AssociationRemoved(wrapKey(key)))(breakOut)
        // convention: first the removals, then the additions. thus, overwriting a key yields
        // successive removal and addition of the same key.
        val seq = if (seqAdd.isEmpty) seqRem else if (seqRem.isEmpty) seqAdd else seqRem ++ seqAdd

        StateEvent(Proc.Update(proc, seq))
      }

      final protected def isConnected(implicit tx: S#Tx): Boolean = proc.targets.nonEmpty
    }

    //    object graphemes extends Graphemes.Modifiable[S] with KeyMap[Grapheme[S], Grapheme.Update[S], Proc.Update[S]] {
    //      final val slot = 1
    //
    //      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Proc.Update[S]] = {
    //        val changes = foldUpdate(pull)
    //        if (changes.isEmpty) None
    //        else Some(Proc.Update(proc,
    //          changes.map({
    //            case (key, u) => Proc.GraphemeChange(key, u)
    //          })(breakOut)))
    //      }
    //
    //      protected def wrapKey(key: String) = GraphemeKey(key)
    //
    //      protected def map: SkipList.Map[S, String, Entry] = graphemeMap
    //
    //      protected def valueInfo = graphemeEntryInfo[S]
    //    }

    object attributes extends Attributes.Modifiable[S] with KeyMap[Attribute[S], Attribute.Update[S], Proc.Update[S]] {
      final val slot = 0

      protected def wrapKey(key: String) = AttributeKey(key)

      def put(key: String, value: Attribute[S])(implicit tx: S#Tx) {
        add(key, value)
      }

      def contains(key: String)(implicit tx: S#Tx): Boolean = map.contains(key)

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Proc.Update[S]] = {
        val changes = foldUpdate(pull)
        if (changes.isEmpty) None
        else Some(Proc.Update(proc,
          changes.map({
            case (key, u) => Proc.AttributeChange(key, u)
          })(breakOut)))
      }

      protected def map: SkipList.Map[S, String, Entry] = attributeMap

      protected def valueInfo = attributeEntryInfo[S]

      def apply[Attr[~ <: evt.Sys[~]] <: Attribute[_]](key: String)(implicit tx: S#Tx,
                                                      tag: reflect.ClassTag[Attr[S]]): Option[Attr[S]#Peer] =
        get(key) match {
          // cf. stackoverflow #16377741
          case Some(attr) => tag.unapply(attr).map(_.peer) // Some(attr.peer)
          case _          => None
        }
    }

    object scans extends Scans.Modifiable[S] with KeyMap[Scan[S], Scan.Update[S], Proc.Update[S]] {
      final val slot = 1

      protected def wrapKey(key: String) = ScanKey(key)

      def add(key: String)(implicit tx: S#Tx): Scan[S] = {
        val scan = Scan[S]
        add(key, scan)
        scan
      }

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Proc.Update[S]] = {
        val changes = foldUpdate(pull)
        if (changes.isEmpty) None
        else Some(Proc.Update(proc,
          changes.map({
            case (key, u) => Proc.ScanChange(key, u)
          })(breakOut)))
      }

      protected def map: SkipList.Map[S, String, Entry] = scanMap

      protected def valueInfo = scanEntryInfo[S]
    }

    private object StateEvent
      extends evti.TriggerImpl[S, Proc.Update[S], Proc[S]]
      with evt.InvariantEvent [S, Proc.Update[S], Proc[S]]
      with evti.Root          [S, Proc.Update[S]]
      with ProcEvent {
      final val slot = 2
    }

    private object ChangeEvent
      extends evt.impl.EventImpl[S, Proc.Update[S], Proc[S]]
      with evt.InvariantEvent   [S, Proc.Update[S], Proc[S]]
      with ProcEvent {

      final val slot = 3

      def connect   ()(implicit tx: S#Tx) {
        attributes ---> this
        scans      ---> this
        StateEvent ---> this
      }
      def disconnect()(implicit tx: S#Tx) {
        attributes -/-> this
        scans      -/-> this
        StateEvent -/-> this
      }

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Proc.Update[S]] = {
        // val graphOpt = if (graphemes .isSource(pull)) graphemes .pullUpdate(pull) else None
        val attrOpt  = if (pull.contains(attributes)) pull(attributes) else None
        val scansOpt = if (pull.contains(scans     )) pull(scans     ) else None
        val stateOpt = if (pull.contains(StateEvent)) pull(StateEvent) else None

        val seq0 =
        //        graphOpt match {
        //          case Some(u) => u.changes
        //          case _ => IIdxSeq.empty
        //        }
        attrOpt match {
          case Some(u) => u.changes
          case _ => Vector.empty
        }
        val seq1 = scansOpt match {
          case Some(u) => if (seq0.isEmpty) u.changes else seq0 ++ u.changes
          case _ => seq0
        }
        val seq2 = stateOpt match {
          case Some(u) => if (seq1.isEmpty) u.changes else seq1 ++ u.changes
          case _ => seq1
        }
        if (seq2.isEmpty) None else Some(Proc.Update(proc, seq2))
      }
    }

    final def select(slot: Int /*, invariant: Boolean */): Event[S, Any, Any] = (slot: @switch) match {
      case ChangeEvent.slot => ChangeEvent
      // case graphemes .slot => graphemes
      case attributes.slot => attributes
      case scans     .slot => scans
      case StateEvent.slot => StateEvent
    }

    //      final def stateChanged : evt.Event[ S, StateChange[ S ], Proc[ S ]] = StateEvent
    final def changed: evt.Event[S, Update[S], Proc[S]] = ChangeEvent

    final protected def writeData(out: DataOutput) {
      out.writeShort(SER_VERSION)
      // name_#     .write(out)
      _graph    .write(out)
      // graphemeMap .write(out)
      attributeMap.write(out)
      scanMap     .write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx) {
      // name_#     .dispose()
      _graph    .dispose()
      // graphemeMap.dispose()
      attributeMap.dispose()
      scanMap     .dispose()
    }

    override def toString() = "Proc" + id
  }

  private final class New[S <: Sys[S]](tx0: S#Tx) extends Impl[S] {
    protected val targets = evt.Targets[S](tx0)

    // protected val name_# = Strings.newVar[S](Strings.newConst("unnamed"))(tx0)
    protected val _graph = {
      implicit val peerSer = SynthGraphSerializer
      tx0.newVar[/*Code[ */ SynthGraph /*]*/ ](id, emptyGraph)
    }

    protected val scanMap = {
      implicit val tx = tx0
      //         implicit val _scanSer = implicitly[ stm.Serializer[ S#Tx, S#Acc, Scan[ S ]]]
      //         implicit val _scanSer : stm.Serializer[ S#Tx, S#Acc, Scan[ S ]] = Scan.serializer
      // implicit val _screwYou: serial.Serializer[S#Tx, S#Acc, ScanEntry[S]] = KeyMapImpl.entrySerializer
      SkipList.Map.empty[S, String, ScanEntry[S]]
    }

    //    protected val graphemeMap = {
    //      implicit val tx = tx0
    //      implicit val _screwYou: serial.Serializer[S#Tx, S#Acc, GraphemeEntry[S]] = KeyMapImpl.entrySerializer
    //      SkipList.Map.empty[S, String, GraphemeEntry[S]]
    //    }

    protected val attributeMap = {
      implicit val tx = tx0
      // implicit val _screwYou: serial.Serializer[S#Tx, S#Acc, GraphemeEntry[S]] = KeyMapImpl.entrySerializer
      SkipList.Map.empty[S, String, AttributeEntry[S]]
    }
  }

  private final class Read[S <: Sys[S]](in: DataInput, access: S#Acc, protected val targets: evt.Targets[S],
                                        tx0: S#Tx)
    extends Impl[S] {

    {
      val serVer = in.readShort()
      require(serVer == SER_VERSION, s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }

    // protected val name_# = Strings.readVar[S](in, access)(tx0)
    protected val _graph = {
      implicit val peerSer = SynthGraphSerializer
      tx0.readVar[/* Code[ */ SynthGraph /* ]*/ ](id, in)
    }

    protected val attributeMap = {
      implicit val tx = tx0
      // implicit val _screwYou: serial.Serializer[S#Tx, S#Acc, AttributeEntry[S]] = KeyMapImpl.entrySerializer
      SkipList.Map.read[S, String, AttributeEntry[S]](in, access)
    }

    protected val scanMap = {
      implicit val tx = tx0
      // implicit val _screwYou: serial.Serializer[S#Tx, S#Acc, ScanEntry[S]] = KeyMapImpl.entrySerializer
      SkipList.Map.read[S, String, ScanEntry[S]](in, access)
    }

    //    protected val graphemeMap = {
    //      implicit val tx = tx0
    //      implicit val _screwYou: serial.Serializer[S#Tx, S#Acc, GraphemeEntry[S]] = KeyMapImpl.entrySerializer
    //      SkipList.Map.read[S, String, GraphemeEntry[S]](in, access)
    //    }
  }
}