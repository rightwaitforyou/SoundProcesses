/*
 *  Proc.scala
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

import de.sciss.lucre.event.Publisher
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.model
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc.impl.{ProcImpl => Impl}

import scala.collection.immutable.{IndexedSeq => Vec}

object Proc extends Obj.Type {
  final val typeID = 0x10005

  // ---- implementation forwards ----

  def apply[S <: Sys[S]](implicit tx: S#Tx): Proc[S] = Impl[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Proc[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Proc[S]] = Impl.serializer[S]

  // ---- event types ----

  /** An update is a sequence of changes */
  final case class Update[S <: Sys[S]](proc: Proc[S], changes: Vec[Change[S]])

  /** A change is either a state change, or a scan or a grapheme change */
  sealed trait Change[S <: Sys[S]]

  final case class GraphChange[S <: Sys[S]](change: model.Change[SynthGraph]) extends Change[S]

  /** An associative change is either adding or removing an association */
  sealed trait ScanMapChange[S <: Sys[S]] extends Change[S] {
    def key: String
    def scan: Scan[S]
  }
  final case class InputAdded   [S <: Sys[S]](key: String, scan: Scan[S]) extends ScanMapChange[S]
  final case class InputRemoved [S <: Sys[S]](key: String, scan: Scan[S]) extends ScanMapChange[S]
  final case class OutputAdded  [S <: Sys[S]](key: String, scan: Scan[S]) extends ScanMapChange[S]
  final case class OutputRemoved[S <: Sys[S]](key: String, scan: Scan[S]) extends ScanMapChange[S]

  final case class InputChange[S <: Sys[S]](key: String, scan: Scan[S], changes: Vec[Scan.Change[S]])
    extends Change[S] {
    override def toString = s"InputChange($key, $scan, $changes)"
  }

  final case class OutputChange[S <: Sys[S]](key: String, scan: Scan[S], changes: Vec[Scan.Change[S]])
    extends Change[S] {
    override def toString = s"OutputChange($key, $scan, $changes)"
  }

  /** Source code of the graph function. */
  final val attrSource = "graph-source"

  final val scanMainIn  = "in"
  final val scanMainOut = "out"

  /** Audio input file (tape) grapheme. */
  final val graphAudio  = "sig"

  override def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)
}
/** The `Proc` trait is the basic entity representing a sound process. */
trait Proc[S <: Sys[S]] extends Obj[S] with Publisher[S, Proc.Update[S]] {
  /** The variable synth graph function of the process. */
  def graph: Expr.Var[S, SynthGraph]

  //  /** The real-time inputs and outputs of the process. */
  //  def scans: Scans.Modifiable[S]

  /** The real-time inputs of the process. */
  def inputs: Scans.Modifiable[S]

  /** The real-time outputs of the process. */
  def outputs: Scans.Modifiable[S]
}