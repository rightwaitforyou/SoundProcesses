/*
 *  Scan.scala
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

import de.sciss.lucre.{event => evt, data}
import impl.{ScanImpl => Impl}
import evt.Event
import language.implicitConversions
import de.sciss.serial.DataInput
import de.sciss.lucre.stm.Identifiable
import collection.immutable.{IndexedSeq => Vec}

object Scan {
  object Link {
    implicit def grapheme[S <: evt.Sys[S]](peer: proc.Grapheme[S]): Grapheme[S] = Grapheme(peer)
    implicit def scan    [S <: evt.Sys[S]](peer: proc.Scan    [S]): Scan    [S] = Scan    (peer)

    /** A link to a grapheme (random access). */
    final case class Grapheme[S <: evt.Sys[S]](peer: proc.Grapheme[S]) extends Link[S] {
      def id = peer.id
      override def toString = peer.toString()
    }

    /** A link to another scan (real-time). */
    final case class Scan[S <: evt.Sys[S]](peer: proc.Scan[S]) extends Link[S] {
      def id = peer.id
      override def toString = peer.toString()
    }
  }

  /** This trait describes a source or sink link to/from a sink. */
  sealed trait Link[S <: evt.Sys[S]] extends Identifiable[S#ID]

  /** Constructs a new unconnected scan. */
  def apply[S <: evt.Sys[S]](implicit tx: S#Tx): Scan[S] = Impl.apply

  def read[S <: evt.Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Scan[S] = Impl.read(in, access)

  implicit def serializer[ S <: evt.Sys[ S ]] : evt.Serializer[ S, Scan[ S ]] = Impl.serializer

  /** A scan's event fires updates of this type. */
  final case class Update[S <: evt.Sys[S]](scan: Scan[S], changes: Vec[Change[S]])
  sealed trait Change      [S <: evt.Sys[S]]
  sealed trait LinkChange  [S <: evt.Sys[S]] extends Change[S]      { def link  : Link[S] }
  sealed trait SinkChange  [S <: evt.Sys[S]] extends LinkChange[S]  { def sink  : Link[S] }
  sealed trait SourceChange[S <: evt.Sys[S]] extends LinkChange[S]  { def source: Link[S] }

  final case class SinkAdded[S <: evt.Sys[S]](sink: Link[S]) extends SinkChange[S] {
    // override def toString = s"[$scan ---> $sink]"
    def link = sink
  }

  final case class SinkRemoved[S <: evt.Sys[S]](sink: Link[S]) extends SinkChange[S] {
    // override def toString = s"[$scan -/-> $sink]"
    def link = sink
  }

  final case class SourceAdded[S <: evt.Sys[S]](source: Link[S]) extends SourceChange[S] {
    // override def toString = s"[$scan <--- $source]"
    def link = source
  }

  final case class SourceRemoved[S <: evt.Sys[S]](source: Link[S]) extends SourceChange[S] {
    // override def toString = s"[$scan <-/- $source]"
    def link = source
  }

  final case class GraphemeChange[S <: evt.Sys[S]](source: Grapheme.Update[S]) extends Change[S]
}

/**
 * A `Scan` represents a real-time signal which can either function as a reader linked to another scan
 * which functions as its source or a grapheme, or it functions as a writer sinking into a grapheme
 * or another scan. Scans are situated with a process (`Proc`) and identified by a unique name, also
 * known as key. A scan can write to any number of targets, and be synchronised to one or multiple
 * source (if the number of sources is greater than one, they are mixed together).
 *
 * If not synchronised to a source, the owner process' graph may feed a signal into it, using
 * `graph.scan.Out`.
 *
 * A scan's event forwards updates from any of its sources, but does not observe its sinks.
 */
trait Scan[S <: evt.Sys[S]] extends evt.Node[S] {
  import Scan._

  /** Returns an iterator over all currently connected sinks. */
  def sinks(implicit tx: S#Tx): data.Iterator[S#Tx, Link[S]]

  // for now, links are not in t_p; this is probably fine, because
  // we have graphemes for such a 'declarative' view, and the scan as needle is really
  // more the 'procedural' approach

  /** Adds a new sink to this scan.
   *
   *  @param  sink the link to add
   *  @return `true` if the link was new in the list of sinks for this scan, `false` it was already connected
   */
  def addSink   (sink: Link[S])(implicit tx: S#Tx): Boolean

  /** Removes a new sink from this scan.
   *
   *  @param  sink the link to remove
   *  @return `true` if the link was found in the list of sinks for this scan, `false` otherwise
   */
  def removeSink(sink: Link[S])(implicit tx: S#Tx): Boolean

  /** Returns an iterator over all currently connected sources. */
  def sources(implicit tx: S#Tx): data.Iterator[S#Tx, Link[S]]

  /** Adds a new source to this scan.
   *
   *  @param  source the link to add
   *  @return `true` if the link was new in the list of sources for this scan, `false` it was already connected
   */
  def addSource(source: Link[S])(implicit tx: S#Tx): Boolean

  /** Removes a new source from this scan.
   *
   *  @param  source the link to remove
   *  @return `true` if the link was found in the list of source for this scan, `false` otherwise
   */
  def removeSource(source: Link[S])(implicit tx: S#Tx): Boolean

  /** An event that is invoked when sinks or sources are connected or disconnected. */
  def changed: Event[S, Scan.Update[S], Scan[S]]

  // called in the implementation from addSink( Link.Scan( _ )). the difference
  // to source_= is that this method should not establish the opposite connection
  // by calling addSink on the source, as this would result in an infinite feedback.
  // still, this method should fire an Scan.SourceChanged event.

  // private[proc] def addScanSource(source: Scan[S])(implicit tx: S#Tx): Unit
  // private[proc] def addScanSink  (sink  : Scan[S])(implicit tx: S#Tx): Unit
}
