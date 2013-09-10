/*
 *  Proc.scala
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

import de.sciss.lucre.{event => evt, expr}
import expr.Expr
import impl.{ProcImpl => Impl}
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.serial.DataInput
import de.sciss.model
import de.sciss.lucre.synth.Sys

object Proc {
  // ---- implementation forwards ----

  def apply[S <: Sys[S]](implicit tx: S#Tx): Proc[S] = Impl[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Proc[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: evt.NodeSerializer[S, Proc[S]] = Impl.serializer[S]

  // ---- event types ----

  /** An update is a sequence of changes */
  final case class Update[S <: Sys[S]](proc: Proc[S], changes: Vec[Change[S]])

  /** A change is either a state change, or a scan or a grapheme change */
  sealed trait Change[S <: Sys[S]]

  /** A state change is either a renaming, a change of graph, or a change of association (map) */
  sealed trait StateChange[S <: Sys[S]] extends Change[S]
  // final case class Rename     (change: evt.Change[String    ]) extends StateChange
  final case class GraphChange[S <: Sys[S]](change: model.Change[SynthGraph]) extends StateChange[S]

  /** An associative change is either adding or removing an association */
  sealed trait AssociativeChange[S <: Sys[S]] extends StateChange[S] {
    def key: AssociativeKey
  }
  final case class AssociationAdded  [S <: Sys[S]](key: AssociativeKey) extends AssociativeChange[S]
  final case class AssociationRemoved[S <: Sys[S]](key: AssociativeKey) extends AssociativeChange[S]

  /** An associative key is either a grapheme or a scan key */
  sealed trait AssociativeKey { def name: String }
  final case class ScanKey(name: String) extends AssociativeKey {
    override def toString = s"[scan: $name]"
  }

  final case class AttributeKey(name: String) extends AssociativeKey {
    override def toString = s"[attribute: $name]"
  }

  final case class ScanChange[S <: Sys[S]](key: String, scan: Scan[S], changes: Vec[Scan.Change[S]])
    extends Change[S] {
    override def toString = s"ScanChange($key, $scan, $changes)"
  }

  final case class AttributeChange[S <: Sys[S]](key: String, attribute: Attribute[S], change: Any)
    extends Change[S] {
    override def toString = s"AttributeChange($key, $attribute, $change)"
  }
}
/** The `Proc` trait is the basic entity representing a sound process. */
trait Proc[S <: Sys[S]] extends evt.Node[S] {
  /** The variable synth graph function of the process. */
  def graph: Expr.Var[S, SynthGraph]

  /** The real-time inputs and outputs of the process. */
  def scans     : Scans     .Modifiable[S]

  /** The scalar attributes of the process. */
  def attributes: Attributes.Modifiable[S]

  def changed: evt.Event[S, Proc.Update[S], Proc[S]]
}