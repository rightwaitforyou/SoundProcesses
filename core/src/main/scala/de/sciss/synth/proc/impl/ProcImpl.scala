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

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.event.{impl => evti, Targets}
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Elem, Copy, NoSys, Obj, Sys}
import de.sciss.lucre.synth.InMemory
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer, Serializer}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.higherKinds

object ProcImpl {
  private final val SER_VERSION = 0x5074  // was "Pr"

  def apply[S <: Sys[S]](implicit tx: S#Tx): Proc[S] = new New[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Proc[S] =
    serializer[S].read(in, access)

  def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Proc[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  private class Ser[S <: Sys[S]] extends ObjSerializer[S, Proc[S]] {
    def tpe: Obj.Type = Proc
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Proc[S] = {
    val targets = Targets.read(in, access)
    new Read(in, access, targets)
  }

  private type ScanEntry[S <: Sys[S]] = KeyMapImpl.Entry[S, String, Scan[S]]

  private type I = InMemory

  implicit def scanEntryInfo[S <: Sys[S]]: KeyMapImpl.ValueInfo[S, String, Scan[S]] =
    anyScanEntryInfo.asInstanceOf[KeyMapImpl.ValueInfo[S, String, Scan[S]]]

  private val anyScanEntryInfo = new KeyMapImpl.ValueInfo[NoSys, String, Scan[NoSys]] {
    // def valueEvent(value: Scan[NoSys]) = value.changed

    val keySerializer   = ImmutableSerializer.String
    val valueSerializer = Scan.serializer[NoSys]
  }

  final class ScansImpl[S <: Sys[S]](proc: Impl[S], val slot: Int, isInput: Boolean)
    extends Scans.Modifiable[S]
    // with evti.EventImpl [S, Proc.Update[S], Proc[S]]
    with impl.KeyMapImpl[S, String, Scan[S]] {

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
      added.foreach { tup =>
        b += (if (isInput) Proc.InputAdded[S](tup._1, tup._2) else Proc.OutputAdded[S](tup._1, tup._2))
      }

      proc.changed.fire(Proc.Update(proc, b.result()))
    }

    // def node: Proc[S] with evt.Node[S] = proc

    // protected def isConnected(implicit tx: S#Tx): Boolean = proc.isConnected

    def add(key: String)(implicit tx: S#Tx): Scan[S] =
      get(key).getOrElse {
        val res = Scan[S](proc, key)
        add(key, res)
        res
      }

//    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Proc.Update[S]] = {
//      val changes = foldUpdate(pull)
//      if (changes.isEmpty) None
//      else Some(Proc.Update(proc,
//        changes.map({
//          case (key, u) =>
//            if (isInput) Proc.InputChange (key, u.scan, u.changes)
//            else         Proc.OutputChange(key, u.scan, u.changes)
//        })(breakOut)))
//    }

    protected def valueInfo = scanEntryInfo[S]
  }

  private sealed trait Impl[S <: Sys[S]]
    extends Proc[S] with evt.impl.SingleNode[S, Proc.Update[S]] {
    proc =>

    final def tpe: Obj.Type = Proc

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl[Out] { out =>
        protected val targets   = Targets[Out]
        val graph               = context(proc.graph)
        val scanInMap           = SkipList.Map.empty[Out, String, ScanEntry[Out]]
        val scanOutMap          = SkipList.Map.empty[Out, String, ScanEntry[Out]]
        context.defer(proc, out) {
          def copyMap(in : SkipList.Map[S  , String, ScanEntry[S  ]],
                      out: SkipList.Map[Out, String, ScanEntry[Out]]): Unit =
          in.iterator.foreach { case (key, eIn) =>
            val eOut = new ScanEntry(key, context(eIn.value))
            out.add(key -> eOut)
          }

          copyMap(proc.scanInMap , out.scanInMap)
          copyMap(proc.scanOutMap, out.scanOutMap)
        }
        connect()
      }

    import Proc._

    def scanInMap : SkipList.Map[S, String, ScanEntry[S]]
    def scanOutMap: SkipList.Map[S, String, ScanEntry[S]]

    // ---- key maps ----

    def isConnected(implicit tx: S#Tx): Boolean = targets.nonEmpty

//    sealed trait ProcEvent {
//      final def node: Proc[S] with evt.Node[S] = proc
//    }

    final val inputs  = new ScansImpl(this, 1, isInput = true )
    final val outputs = new ScansImpl(this, 2, isInput = false)

//    object StateEvent
//      extends evti.TriggerImpl[S, Proc.Update[S], Proc[S]]
//      with evti.Root          [S, Proc.Update[S]]
//      with ProcEvent {
//
//      final val slot = 3
//    }

    final def connect()(implicit tx: S#Tx): this.type = {
      graph.changed ---> changed
      // inputs        ---> changed
      // outputs       ---> changed
      // StateEvent    ---> this
      this
    }

    private[this] def disconnect()(implicit tx: S#Tx): Unit = {
      graph.changed -/-> changed
      // inputs        -/-> changed
      // outputs       -/-> changed
      // StateEvent    -/-> this
    }

    object changed extends Changed
      with evt.impl.Generator[S, Proc.Update[S]]
      // with evt.impl.Root[S, Proc.Update[S]]
      // extends evt.impl.EventImpl[S, Proc.Update[S], Proc[S]]
       {

      // final val slot = 4

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Proc.Update[S]] = {
        val graphCh     = graph.changed
        val graphOpt    = if (pull.contains(graphCh)) pull(graphCh) else None
        val stateOpt    = if (pull.isOrigin(this)) Some(pull.resolve[Proc.Update[S]]) else None

        val seq0 = graphOpt.fold(Vec.empty[Change[S]]) { u =>
          Vector(GraphChange(u))
        }
//        val seq1 = seq0
//          scanInsOpt.fold(seq0) { u =>
//          if (seq0.isEmpty) u.changes else seq0 ++ u.changes
//        }
//        val seq2 = seq1
//        scanOutsOpt.fold(seq1) { u =>
//          if (seq1.isEmpty) u.changes else seq1 ++ u.changes
//        }
        val seq3 = stateOpt.fold(seq0 /* seq2 */) { u =>
          if (seq0 /* seq2 */.isEmpty) u.changes else seq0 /* seq2 */ ++ u.changes
        }
        if (seq3.isEmpty) None else Some(Proc.Update(proc, seq3))
      }
    }

//    final def event(slot: Int /*, invariant: Boolean */): Event[S, Any] = (slot: @switch) match {
//      case ChangeEvent.slot => ChangeEvent
//      case 1 /* inputs .slot */ => inputs
//      case 2 /* outputs.slot */ => outputs
//      case StateEvent .slot => StateEvent
//    }

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

    override def toString: String = s"Proc$id"
  }

  private final class New[S <: Sys[S]](implicit tx0: S#Tx) extends Impl[S] {
    protected val targets   = evt.Targets[S](tx0)
    val graph               = SynthGraphObj.newVar(SynthGraphObj.empty)
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

    val graph         = SynthGraphObj.readVar(in, access)
    val scanInMap     = SkipList.Map.read[S, String, ScanEntry[S]](in, access)
    val scanOutMap    = SkipList.Map.read[S, String, ScanEntry[S]](in, access)
  }
}