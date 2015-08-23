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

import de.sciss.lucre.event.Publisher
import de.sciss.lucre.stm.{Elem, Obj, Identifiable, Sys}
import de.sciss.lucre.{event => evt, stm, data}
import de.sciss.serial.{DataOutput, Serializer, DataInput}
import de.sciss.synth.proc.impl.{ScanImpl => Impl}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

object Scan extends Obj.Type {
  final val typeID = 0x10009

  override def init(): Unit = {
    super.init()
    Link .init()
  }

  object Link extends Elem.Type {
    final val typeID = 0x1000B

    implicit def grapheme[S <: Sys[S]](peer: proc.Grapheme[S]): Grapheme[S] = Grapheme(peer)
    implicit def scan    [S <: Sys[S]](peer: proc.Scan    [S]): Scan    [S] = Scan    (peer)

    /** A link to a grapheme (random access). */
    final case class Grapheme[S <: Sys[S]](peer: proc.Grapheme[S])
      extends Link[S] with stm.impl.ConstElemImpl[S] {

      def tpe: Elem.Type = Link

      def id = peer.id
      override def toString = peer.toString

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(0)
        peer.write(out)
      }
    }

    /** A link to another scan (real-time). */
    final case class Scan[S <: Sys[S]](peer: proc.Scan[S])
      extends Link[S] with stm.impl.ConstElemImpl[S] {
      def tpe: Elem.Type = Link

      def id = peer.id
      override def toString = peer.toString

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(1)
        peer.write(out)
      }
    }

    def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S] = ???  // RRR
  }

  /** This trait describes a source or sink link to/from a sink. */
  sealed trait Link[S <: Sys[S]] extends Elem[S] with Identifiable[S#ID]

  /** Constructs a new unconnected scan. */
  def apply[S <: Sys[S]](implicit tx: S#Tx): Scan[S] = Impl.apply

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Scan[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Scan[S]] = Impl.serializer

  /** A scan event fires updates of this type. */
  final case class Update[S <: Sys[S]](scan: Scan[S], changes: Vec[Change[S]])
  sealed trait Change      [S <: Sys[S]]
  sealed trait LinkChange  [S <: Sys[S]] extends Change[S]      { def link  : Link[S] }
//  sealed trait SinkChange  [S <: Sys[S]] extends LinkChange[S]  { def sink  : Link[S] }
//  sealed trait SourceChange[S <: Sys[S]] extends LinkChange[S]  { def source: Link[S] }

  final case class Added[S <: Sys[S]](link: Link[S]) extends LinkChange[S] {
    // override def toString = s"[$scan ---> $sink]"
//    def link = sink
  }

  final case class Removed[S <: Sys[S]](link: Link[S]) extends LinkChange[S] {
    // override def toString = s"[$scan -/-> $sink]"
//    def link = sink
  }

//  final case class SourceAdded[S <: Sys[S]](source: Link[S]) extends SourceChange[S] {
//    // override def toString = s"[$scan <--- $source]"
//    def link = source
//  }
//
//  final case class SourceRemoved[S <: Sys[S]](source: Link[S]) extends SourceChange[S] {
//    // override def toString = s"[$scan <-/- $source]"
//    def link = source
//  }

//  final case class GraphemeChange[S <: Sys[S]](grapheme: Grapheme[S],
//                                               changes: Vec[Grapheme.Segment]) extends Change[S]
  override def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)
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
trait Scan[S <: Sys[S]] extends Obj[S] with Publisher[S, Scan.Update[S]] {
  import Scan._

  /** Returns an iterator over all currently connected nodes. */
  def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Link[S]]

  def isEmpty (implicit tx: S#Tx): Boolean
  def nonEmpty(implicit tx: S#Tx): Boolean

  /** Adds a new link to this scan.
   *
   *  @param  link the link to add
   *  @return `true` if the link was new in the list for this scan, `false` it was already connected
   */
  def add(link: Link[S])(implicit tx: S#Tx): Boolean

  /** Removes a new link from this scan.
   *
   *  @param  link the link to remove
   *  @return `true` if the link was found in the list for this scan, `false` otherwise
   */
  def remove(link: Link[S])(implicit tx: S#Tx): Boolean
}