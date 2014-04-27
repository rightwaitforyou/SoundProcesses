/*
 *  Proc.scala
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

package de.sciss.synth
package proc

import de.sciss.lucre.{event => evt, expr}
import expr.Expr
import impl.{ProcImpl => Impl}
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.serial.DataInput
import de.sciss.model
import evt.Sys
import de.sciss.lucre.event.Publisher

object Proc {
  final val typeID = 0x10005

  // ---- implementation forwards ----

  def apply[S <: Sys[S]](implicit tx: S#Tx): Proc[S] = Impl[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Proc[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: evt.NodeSerializer[S, Proc[S]] = Impl.serializer[S]

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
  final case class ScanAdded  [S <: Sys[S]](key: String, scan: Scan[S]) extends ScanMapChange[S]
  final case class ScanRemoved[S <: Sys[S]](key: String, scan: Scan[S]) extends ScanMapChange[S]

  final case class ScanChange[S <: Sys[S]](key: String, scan: Scan[S], changes: Vec[Scan.Change[S]])
    extends Change[S] {
    override def toString = s"ScanChange($key, $scan, $changes)"
  }
}
/** The `Proc` trait is the basic entity representing a sound process. */
trait Proc[S <: Sys[S]] extends evt.Node[S] with Publisher[S, Proc.Update[S]] {
  /** The variable synth graph function of the process. */
  def graph: Expr.Var[S, SynthGraph]

  /** The real-time inputs and outputs of the process. */
  def scans: Scans.Modifiable[S]
}