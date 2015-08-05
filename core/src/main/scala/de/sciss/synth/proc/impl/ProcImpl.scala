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
  private final val SER_VERSION = 0x5074  // was "Pr"

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

  final class ScansImpl[S <: Sys[S]](proc: Impl[S], val slot: Int, isInput: Boolean)
    extends Scans.Modifiable[S]
    with evti.EventImpl [S, Proc.Update[S], Proc[S]]
    with evt.InvariantEvent[S, Proc.Update[S], Proc[S]]
    with impl.KeyMapImpl[S, String, Scan[S], Scan.Update[S]] {

    // ---- key-map-impl details ----

    protected def map = if (isInput) proc.scanInMap else proc.scanOutMap

    protected def fire(added: Option[(String, Scan[S])], removed: Option[(String, Scan[S])])
                      (implicit tx: S#Tx): Unit = {
      val b = Vector.newBuilder[Proc.ScanMapChange[S]]
      b.sizeHint(2)
      // convention: first the removals, then the additions. thus, overwriting a key yields
      // successive removal and addition of the same key.
      removed.foreach { tup =>
        b += (if (isInput) Proc.InputRemoved[S](tup._1, tup._2) else Proc.OutputRemoved[S](tup._1, tup._2))
      }
      added.foreach {  tup =>
        b += (if (isInput) Proc.InputAdded[S](tup._1, tup._2) else Proc.OutputAdded[S](tup._1, tup._2))
      }

      proc.StateEvent(Proc.Update(proc, b.result()))
    }

    protected def reader: evt.Reader[S, Proc[S]] = ProcImpl.serializer
    def node: Proc[S] with evt.Node[S] = proc

    protected def isConnected(implicit tx: S#Tx): Boolean = proc.isConnected

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
          case (key, u) =>
            if (isInput) Proc.InputChange (key, u.scan, u.changes)
            else         Proc.OutputChange(key, u.scan, u.changes)
        })(breakOut)))
    }

    protected def valueInfo = scanEntryInfo[S]
  }

  private sealed trait Impl[S <: Sys[S]]
    extends Proc[S] {
    proc =>

    import Proc._

    def scanInMap : SkipList.Map[S, String, ScanEntry[S]]
    def scanOutMap: SkipList.Map[S, String, ScanEntry[S]]

    // ---- key maps ----

    def isConnected(implicit tx: S#Tx): Boolean = targets.nonEmpty

    sealed trait ProcEvent {
      final protected def reader: evt.Reader[S, Proc[S]] = ProcImpl.serializer
      final def node: Proc[S] with evt.Node[S] = proc
    }

    final val inputs  = new ScansImpl(this, 1, isInput = true )
    final val outputs = new ScansImpl(this, 2, isInput = false)

    object StateEvent
      extends evti.TriggerImpl[S, Proc.Update[S], Proc[S]]
      with evt.InvariantEvent [S, Proc.Update[S], Proc[S]]
      with evti.Root          [S, Proc.Update[S]]
      with ProcEvent {

      final val slot = 3
    }

    private object ChangeEvent
      extends evt.impl.EventImpl[S, Proc.Update[S], Proc[S]]
      with evt.InvariantEvent   [S, Proc.Update[S], Proc[S]]
      with ProcEvent {

      final val slot = 4

      def connect   ()(implicit tx: S#Tx): Unit = {
        graph.changed ---> this
        inputs        ---> this
        outputs       ---> this
        StateEvent    ---> this
      }
      def disconnect()(implicit tx: S#Tx): Unit = {
        graph.changed -/-> this
        inputs        -/-> this
        outputs       -/-> this
        StateEvent    -/-> this
      }

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Proc.Update[S]] = {
        // val graphOpt = if (graphemes .isSource(pull)) graphemes .pullUpdate(pull) else None
        val graphCh     = graph.changed
        val graphOpt    = if (pull.contains(graphCh   )) pull(graphCh   ) else None
        val scanInsOpt  = if (pull.contains(inputs    )) pull(inputs    ) else None
        val scanOutsOpt = if (pull.contains(outputs   )) pull(outputs   ) else None
        val stateOpt    = if (pull.contains(StateEvent)) pull(StateEvent) else None

        val seq0 = graphOpt.fold(Vec.empty[Change[S]]) { u =>
          Vector(GraphChange(u))
        }
        val seq1 = scanInsOpt.fold(seq0) { u =>
          if (seq0.isEmpty) u.changes else seq0 ++ u.changes
        }
        val seq2 = scanOutsOpt.fold(seq1) { u =>
          if (seq1.isEmpty) u.changes else seq1 ++ u.changes
        }
        val seq3 = stateOpt.fold(seq2) { u =>
          if (seq2.isEmpty) u.changes else seq2 ++ u.changes
        }
        if (seq3.isEmpty) None else Some(Proc.Update(proc, seq3))
      }
    }

    final def select(slot: Int /*, invariant: Boolean */): Event[S, Any, Any] = (slot: @switch) match {
      case ChangeEvent.slot => ChangeEvent
      case 1 /* inputs .slot */ => inputs
      case 2 /* outputs.slot */ => outputs
      case StateEvent .slot => StateEvent
    }

    //      final def stateChanged : evt.Event[ S, StateChange[ S ], Proc[ S ]] = StateEvent
    final def changed: evt.Event[S, Update[S], Proc[S]] = ChangeEvent

    final protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      graph       .write(out)
      scanInMap   .write(out)
      scanOutMap  .write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      graph       .dispose()
      scanInMap   .dispose()
      scanOutMap  .dispose()
    }

    override def toString() = s"Proc$id"
  }

  private final class New[S <: Sys[S]](implicit tx0: S#Tx) extends Impl[S] {
    protected val targets   = evt.Targets[S](tx0)
    val graph               = SynthGraphs.newVar(SynthGraphs.empty)
    val scanInMap           = SkipList.Map.empty[S, String, ScanEntry[S]]
    val scanOutMap          = SkipList.Map.empty[S, String, ScanEntry[S]]
  }

  private final class Read[S <: Sys[S]](in: DataInput, access: S#Acc, protected val targets: evt.Targets[S])
                                       (implicit tx0: S#Tx)
    extends Impl[S] {

    {
      val serVer = in.readShort()
      if (serVer != SER_VERSION) sys.error(s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }

    val graph         = SynthGraphs.readVar(in, access)
    val scanInMap     = SkipList.Map.read[S, String, ScanEntry[S]](in, access)
    val scanOutMap    = SkipList.Map.read[S, String, ScanEntry[S]](in, access)
  }
}