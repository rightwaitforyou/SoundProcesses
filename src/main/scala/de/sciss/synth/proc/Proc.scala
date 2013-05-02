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
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.serial.DataInput

object Proc {
  // ---- implementation forwards ----

  def apply[S <: evt.Sys[S]](implicit tx: S#Tx): Proc[S] = Impl[S]

  def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Proc[S] = Impl.read(in, access)

  implicit def serializer[S <: evt.Sys[S]]: evt.NodeSerializer[S, Proc[S]] = Impl.serializer[S]

  // ---- event types ----

  /** An update is a sequence of changes */
  final case class Update[S <: evt.Sys[S]](proc: Proc[S], changes: IIdxSeq[Change[S]])

  /** A change is either a state change, or a scan or a grapheme change */
  sealed trait Change[+S]

  /** A state change is either a renaming, a change of graph, or a change of association (map) */
  sealed trait StateChange extends Change[ Nothing ]
  // final case class Rename     (change: evt.Change[String    ]) extends StateChange
  final case class GraphChange(change: evt.Change[SynthGraph]) extends StateChange

  /** An associative change is either adding or removing an association */
  sealed trait AssociativeChange extends StateChange {
    def key: AssociativeKey
  }
  final case class AssociationAdded  (key: AssociativeKey) extends AssociativeChange
  final case class AssociationRemoved(key: AssociativeKey) extends AssociativeChange

  /** An associative key is either a grapheme or a scan key */
  sealed trait AssociativeKey { def name: String }
  final case class ScanKey(name: String) extends AssociativeKey {
    override def toString = "[scan: " + name + "]"
  }
  final case class GraphemeKey(name: String) extends AssociativeKey {
    override def toString = "[grapheme: " + name + "]"
  }

  final case class ScanChange[S <: evt.Sys[S]](key: String, scanUpdate: Scan.Update[S]) extends Change[S] {
    override def toString = "ScanChange(" + key + ", " + scanUpdate + ")"
  }

  final case class GraphemeChange[S <: evt.Sys[S]](key: String, graphemeUpdate: Grapheme.Update[S]) extends Change[S] {
    override def toString = "GraphemeChange(" + key + ", " + graphemeUpdate + ")"
  }
}

trait Proc[S <: evt.Sys[S]] extends evt.Node[S] {
  import Proc._

  // def name(implicit tx: S#Tx): Expr[S, String]
  // def name_=(expr: Expr[S, String])(implicit tx: S#Tx): Unit

  def graph(implicit tx: S#Tx): Code[SynthGraph]
  def graph_=(g: Code[SynthGraph])(implicit tx: S#Tx): Unit

  // ---- controls preview demo ----

  def scans    : Scans    .Modifiable[S]
  // def graphemes: Graphemes.Modifiable[S]

  def changed: evt.Event[S, Update[S], Proc[S]]
}