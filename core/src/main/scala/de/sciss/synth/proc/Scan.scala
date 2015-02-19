/*
 *  Scan.scala
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

import de.sciss.lucre.{event => evt, data}
import impl.{ScanImpl => Impl}
import de.sciss.lucre.event.Publisher
import language.implicitConversions
import de.sciss.serial.{Serializer, DataInput}
import de.sciss.lucre.stm.Identifiable
import collection.immutable.{IndexedSeq => Vec}
import evt.Sys

object Scan {
  final val typeID = 0x10009

  object Link {
    implicit def grapheme[S <: Sys[S]](peer: proc.Grapheme[S]): Grapheme[S] = Grapheme(peer)
    implicit def scan    [S <: Sys[S]](peer: proc.Scan    [S]): Scan    [S] = Scan    (peer)

    /** A link to a grapheme (random access). */
    final case class Grapheme[S <: Sys[S]](peer: proc.Grapheme[S]) extends Link[S] {
      def id = peer.id
      override def toString = peer.toString()
    }

    /** A link to another scan (real-time). */
    final case class Scan[S <: Sys[S]](peer: proc.Scan[S]) extends Link[S] {
      def id = peer.id
      override def toString = peer.toString()
    }
  }

  /** This trait describes a source or sink link to/from a sink. */
  sealed trait Link[S <: Sys[S]] extends Identifiable[S#ID]

  /** Constructs a new unconnected scan. */
  def apply[S <: Sys[S]](implicit tx: S#Tx): Scan[S] = Impl.apply

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Scan[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Scan[S]] = Impl.serializer

  /** A scan event fires updates of this type. */
  final case class Update[S <: Sys[S]](scan: Scan[S], changes: Vec[Change[S]])
  sealed trait Change      [S <: Sys[S]]
  sealed trait LinkChange  [S <: Sys[S]] extends Change[S]      { def link  : Link[S] }
  sealed trait SinkChange  [S <: Sys[S]] extends LinkChange[S]  { def sink  : Link[S] }
  sealed trait SourceChange[S <: Sys[S]] extends LinkChange[S]  { def source: Link[S] }

  final case class SinkAdded[S <: Sys[S]](sink: Link[S]) extends SinkChange[S] {
    // override def toString = s"[$scan ---> $sink]"
    def link = sink
  }

  final case class SinkRemoved[S <: Sys[S]](sink: Link[S]) extends SinkChange[S] {
    // override def toString = s"[$scan -/-> $sink]"
    def link = sink
  }

  final case class SourceAdded[S <: Sys[S]](source: Link[S]) extends SourceChange[S] {
    // override def toString = s"[$scan <--- $source]"
    def link = source
  }

  final case class SourceRemoved[S <: Sys[S]](source: Link[S]) extends SourceChange[S] {
    // override def toString = s"[$scan <-/- $source]"
    def link = source
  }

  final case class GraphemeChange[S <: Sys[S]](grapheme: Grapheme[S],
                                               changes: Vec[Grapheme.Segment]) extends Change[S]

  // ---- Elem ----

  implicit object Elem extends proc.Elem.Companion[Scan.Elem] {
    def typeID: Int = Scan.typeID

    def apply[S <: Sys[S]](peer: Scan[S])(implicit tx: S#Tx): Scan.Elem[S] =  proc.impl.ElemImpl.Scan(peer)

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Scan.Elem[S]] =
      proc.impl.ElemImpl.Scan.serializer[S]
  }
  trait Elem[S <: Sys[S]] extends proc.Elem[S] {
    type Peer       = Scan[S]
    type PeerUpdate = Scan.Update[S]
    type This       = Elem[S]
  }

  /** Convenient short-cut */

  object Obj {
    def unapply[S <: Sys[S]](obj: proc.Obj[S]): Option[Scan.Obj[S]] =
      if (obj.elem.isInstanceOf[Scan.Elem[S]]) Some(obj.asInstanceOf[Scan.Obj[S]])
      else None
  }
  type Obj[S <: Sys[S]] = proc.Obj.T[S, Scan.Elem]
}

/** A `Scan` represents a real-time signal which can either function as a reader linked to another scan
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
trait Scan[S <: Sys[S]] extends evt.Node[S] with Publisher[S, Scan.Update[S]] {
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
}