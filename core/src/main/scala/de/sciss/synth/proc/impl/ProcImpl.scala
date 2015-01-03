/*
 *  ProcImpl.scala
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

package de.sciss.synth
package proc
package impl

import de.sciss.lucre.{event => evt, data}
import evt.{Event, impl => evti, Sys}
import data.SkipList
import annotation.switch
import collection.breakOut
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.serial.{DataOutput, ImmutableSerializer, DataInput}
import language.higherKinds
import de.sciss.lucre.synth.InMemory

object ProcImpl {
  private final val SER_VERSION = 0x5073  // was "Pr"

  def apply[S <: Sys[S]](implicit tx: S#Tx): Proc[S] = new New[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Proc[S] =
    serializer[S].read(in, access)

  def serializer[S <: Sys[S]]: evt.NodeSerializer[S, Proc[S]] =
    anySer.asInstanceOf[evt.NodeSerializer[S, Proc[S]]]

  private val anySer = new Serializer[InMemory]

  private class Serializer[S <: Sys[S]] extends evt.NodeSerializer[S, Proc[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Proc[S] =
      new Read(in, access, targets)
  }

  private type ScanEntry[S <: Sys[S]] = KeyMapImpl.Entry[S, String, Scan[S], Scan.Update[S]]

  private type I = InMemory

  implicit def scanEntryInfo[S <: Sys[S]]: KeyMapImpl.ValueInfo[S, String, Scan[S], Scan.Update[S]] =
    anyScanEntryInfo.asInstanceOf[KeyMapImpl.ValueInfo[S, String, Scan[S], Scan.Update[S]]]

  private val anyScanEntryInfo = new KeyMapImpl.ValueInfo[I, String, Scan[I], Scan.Update[I]] {
    def valueEvent(value: Scan[I]) = value.changed

    val keySerializer   = ImmutableSerializer.String
    val valueSerializer = Scan.serializer[I]
  }

  private sealed trait Impl[S <: Sys[S]]
    extends Proc[S] {
    proc =>

    import Proc._

    protected def scanMap: SkipList.Map[S, String, ScanEntry[S]]

    // ---- key maps ----

    sealed trait ProcEvent {
      final protected def reader: evt.Reader[S, Proc[S]] = ProcImpl.serializer
      final def node: Proc[S] with evt.Node[S] = proc
    }

    object scans
      extends Scans.Modifiable[S]
      with evti.EventImpl [S, Proc.Update[S], Proc[S]]
      with evt.InvariantEvent[S, Proc.Update[S], Proc[S]]
      with ProcEvent
      with impl.KeyMapImpl[S, String, Scan[S], Scan.Update[S]] {

      // ---- key-map-impl details ----

      final protected def fire(added: Option[(String, Scan[S])], removed: Option[(String, Scan[S])])
                              (implicit tx: S#Tx): Unit = {
        val b = Vector.newBuilder[Proc.ScanMapChange[S]]
        b.sizeHint(2)
        // convention: first the removals, then the additions. thus, overwriting a key yields
        // successive removal and addition of the same key.
        removed.foreach { tup =>
          b += Proc.ScanRemoved[S](tup._1, tup._2)
        }
        added.foreach {  tup =>
          b += Proc.ScanAdded[S](tup._1, tup._2)
        }

        StateEvent(Proc.Update(proc, b.result()))
      }

      final protected def isConnected(implicit tx: S#Tx): Boolean = proc.targets.nonEmpty

      final val slot = 1

      def add(key: String)(implicit tx: S#Tx): Scan[S] =
        get(key).getOrElse {
          val res = Scan[S]
          add(key, res)
          res
        }

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Proc.Update[S]] = {
        val changes = foldUpdate(pull)
        if (changes.isEmpty) None
        else Some(Proc.Update(proc,
          changes.map({
            case (key, u) => Proc.ScanChange(key, u.scan, u.changes)
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

      def connect   ()(implicit tx: S#Tx): Unit = {
        graph.changed ---> this
        scans         ---> this
        StateEvent    ---> this
      }
      def disconnect()(implicit tx: S#Tx): Unit = {
        graph.changed -/-> this
        scans         -/-> this
        StateEvent    -/-> this
      }

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Proc.Update[S]] = {
        // val graphOpt = if (graphemes .isSource(pull)) graphemes .pullUpdate(pull) else None
        val graphCh  = graph.changed
        val graphOpt = if (pull.contains(graphCh   )) pull(graphCh   ) else None
        val scansOpt = if (pull.contains(scans     )) pull(scans     ) else None
        val stateOpt = if (pull.contains(StateEvent)) pull(StateEvent) else None

        val seq0 = graphOpt.fold(Vec.empty[Change[S]]) { u =>
          Vector(GraphChange(u))
        }
        val seq2 = scansOpt.fold(seq0) { u =>
          if (seq0.isEmpty) u.changes else seq0 ++ u.changes
        }
        val seq3 = stateOpt.fold(seq2) { u =>
          if (seq2.isEmpty) u.changes else seq2 ++ u.changes
        }
        if (seq3.isEmpty) None else Some(Proc.Update(proc, seq3))
      }
    }

    final def select(slot: Int /*, invariant: Boolean */): Event[S, Any, Any] = (slot: @switch) match {
      case ChangeEvent.slot => ChangeEvent
      case scans      .slot => scans
      case StateEvent .slot => StateEvent
    }

    //      final def stateChanged : evt.Event[ S, StateChange[ S ], Proc[ S ]] = StateEvent
    final def changed: evt.Event[S, Update[S], Proc[S]] = ChangeEvent

    final protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      graph       .write(out)
      scanMap     .write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      graph       .dispose()
      scanMap     .dispose()
    }

    override def toString() = s"Proc$id"
  }

  private final class New[S <: Sys[S]](implicit tx0: S#Tx) extends Impl[S] {
    protected val targets       = evt.Targets[S](tx0)
    val graph                   = SynthGraphs.newVar(SynthGraphs.empty)
    protected val scanMap       = SkipList.Map.empty[S, String, ScanEntry[S]]
  }

  private final class Read[S <: Sys[S]](in: DataInput, access: S#Acc, protected val targets: evt.Targets[S])
                                       (implicit tx0: S#Tx)
    extends Impl[S] {

    {
      val serVer = in.readShort()
      require(serVer == SER_VERSION, s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }

    val graph                   = SynthGraphs.readVar(in, access)
    protected val scanMap       = SkipList.Map.read[S, String, ScanEntry[S]](in, access)
  }
}