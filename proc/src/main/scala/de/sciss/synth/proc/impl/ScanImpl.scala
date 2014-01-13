/*
 *  ScanImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
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
package impl

import de.sciss.lucre.{event => evt, data, expr}
import evt.{impl => evti, Event}
import annotation.switch
import expr.List
import proc.Scan
import de.sciss.serial.{DataOutput, Serializer, DataInput}
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.lucre.synth.{InMemory, Sys}

object ScanImpl {
  import Scan.Link

  private final val SER_VERSION = 0x536E  // "Sn"

  sealed trait Update[S]

  def apply[S <: Sys[S]](implicit tx: S#Tx): Scan[S] = {
    val targets           = evt.Targets[S] // XXX TODO: partial?
    val scanSourceList    = List.Modifiable[S, Scan    [S]]
    val graphemeSourceList= List.Modifiable[S, Grapheme[S], Grapheme.Update[S]]
    val sinkList          = List.Modifiable[S, Link[S]]
    new Impl(targets, scanSourceList, graphemeSourceList, sinkList)
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Scan[S] = {
    serializer[S].read(in, access)
  }

  implicit def serializer[S <: Sys[S]]: evt.NodeSerializer[S, Scan[S]] =
    anySer.asInstanceOf[evt.NodeSerializer[S, Scan[S]]]

  private type I = InMemory

  private val anySer: evt.NodeSerializer[I, Scan[I]] = new Ser[I]

  private final class Ser[S <: Sys[S]] extends evt.NodeSerializer[S, Scan[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Scan[S] = {
      val serVer    = in.readShort()
      require(serVer == SER_VERSION, s"Incompatible serialized (found $serVer, required $SER_VERSION)")

      val scanSourceList      = List.Modifiable.read[S, Scan[S]]                        (in, access)
      val graphemeSourceList  = List.Modifiable.read[S, Grapheme[S], Grapheme.Update[S]](in, access)
      val sinkList            = List.Modifiable.read[S, Link[S]](in, access)
      new Impl(targets, scanSourceList, graphemeSourceList, sinkList)
    }
  }

  implicit def linkSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Link[S]] =
    anyLinkSer.asInstanceOf[Serializer[S#Tx, S#Acc, Link[S]]]

  private val anyLinkSer: Serializer[I#Tx, I#Acc, Link[I]] = new LinkSer[I]

  private final class LinkSer[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Link[S]] {
    def write(link: Link[S], out: DataOutput): Unit =
      link match {
        case Link.Grapheme(peer) =>
          out.writeByte(0)
          peer.write(out)
        case Link.Scan(peer) =>
          out.writeByte(1)
          peer.write(out)
      }

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Link[S] = {
      (in.readByte(): @switch) match {
        case 0 =>
          val peer = Grapheme.read[S](in, access)
          Link.Grapheme(peer)
        case 1 =>
          val peer = Scan.read[S](in, access)
          Link.Scan(peer)
        case cookie => sys.error("Unexpected cookie " + cookie)
      }
    }
  }

  // TODO: the crappy sinkList is only needed because the id map does not have an iterator...
  // we should really figure out how to add iterator functionality to the id map!!!
  private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                        protected val scanSourceList    : List.Modifiable[S, Scan[S], Unit],
                                        protected val graphemeSourceList: List.Modifiable[S, Grapheme[S], Grapheme.Update[S]],
                                        protected val sinkList          : List.Modifiable[S, Link[S], Unit])
    extends Scan[S]
    with evti.StandaloneLike[S, Scan.Update[S], Scan[S]]
    with evti.Generator[S, Scan.Update[S], Scan[S]] {
    override def toString() = "Scan" + id

    def sinks  (implicit tx: S#Tx): data.Iterator[S#Tx, Link[S]] = sinkList.iterator
    def sources(implicit tx: S#Tx): data.Iterator[S#Tx, Link[S]] = new data.Iterator[S#Tx, Link[S]] {
      val scanIt  = scanSourceList.iterator
      val graphIt = graphemeSourceList.iterator

      def hasNext(implicit tx: S#Tx) = scanIt.hasNext || graphIt.hasNext

      def next()(implicit tx: S#Tx): Link[S] = if (scanIt.hasNext)
        Link.Scan(scanIt.next())
      else
        Link.Grapheme(graphIt.next())
    }

    def addSink(sink: Link[S])(implicit tx: S#Tx): Boolean = {
      // val sinkID = sink.id
      // if (sinkMap.contains(sinkID)) return false
      if (sinkList.indexOf(sink) >= 0) return false

      // sinkMap.put(sinkID, sink)
      sinkList.addLast(sink) // addHead faster than addLast; but perhaps we should use addLast to have a better iterator order?
      sink match {
        case Link.Scan(peer) => peer.addSource(Link.Scan(this))
        case _ =>
      }
      fire(Scan.Update(this, Vec(Scan.SinkAdded(sink))))
      true
    }

    def removeSink(sink: Link[S])(implicit tx: S#Tx): Boolean = {
      // val sinkID = sink.id
      // if (!sinkMap.contains(sinkID)) return false
      if (sinkList.indexOf(sink) < 0) return false
      // sinkMap.remove(sinkID)
      sinkList.remove(sink)
      sink match {
        case Link.Scan(peer) => peer.removeSource(Link.Scan(this))
        case _ =>
      }
      fire(Scan.Update(this, Vec(Scan.SinkRemoved(sink))))
      true
    }

    def addSource(source: Link[S])(implicit tx: S#Tx): Boolean = {
      val res = source match {
        case Link.Scan    (peer) => addScanSource    (peer)
        case Link.Grapheme(peer) => addGraphemeSource(peer)
      }
      if (res) fire(Scan.Update(this, Vec(Scan.SourceAdded(source))))
      res
    }

    private def addScanSource(source: Scan[S])(implicit tx: S#Tx): Boolean = {
      if (scanSourceList.indexOf(source) >= 0) return false

      scanSourceList.addLast(source)
      source.addSink(Link.Scan(this))
      true
    }

    private def addGraphemeSource(source: Grapheme[S])(implicit tx: S#Tx): Boolean = {
      if (graphemeSourceList.indexOf(source) >= 0) return false

      graphemeSourceList.addLast(source)
      true
    }

    def removeSource(source: Link[S])(implicit tx: S#Tx): Boolean = {
      val res = source match {
        case Link.Scan    (peer) => removeScanSource    (peer)
        case Link.Grapheme(peer) => removeGraphemeSource(peer)
      }
      if (res) fire(Scan.Update(this, Vec(Scan.SourceRemoved(source))))
      res
    }

    private def removeScanSource(source: Scan[S])(implicit tx: S#Tx): Boolean = {
      val res = scanSourceList.remove(source)
      if (res) {
        source.removeSink(Link.Scan(this))
      }
      res
    }

    private def removeGraphemeSource(source: Grapheme[S])(implicit tx: S#Tx): Boolean =
      graphemeSourceList.remove(source)

    def changed: Event[S, Scan.Update[S], Scan[S]] = this

    def connect   ()(implicit tx: S#Tx): Unit = graphemeSourceList.changed ---> this
    def disconnect()(implicit tx: S#Tx): Unit = graphemeSourceList.changed -/-> this

    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Scan.Update[S]] = {
      val u1 = if (pull.parents(this).isEmpty)
        pull.resolve[Scan.Update[S]].changes // fold(Vec.empty[Scan.Change[S]])(_.changes)
      else
        Vec.empty

      val u2 = pull(graphemeSourceList.changed).fold(u1) { ll =>
        val gcs = ll.changes.collect {
          case List.Element(_, gc) => Scan.GraphemeChange(gc.grapheme, gc.changes)
        }
        if (u1.isEmpty) gcs else if (gcs.isEmpty) u1 else u1 ++ gcs
      }
      if (u2.isEmpty) None else Some(Scan.Update(this, u2))
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      scanSourceList    .write(out)
      graphemeSourceList.write(out)
      sinkList          .write(out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      scanSourceList    .dispose()
      graphemeSourceList.dispose()
      sinkList          .dispose()
    }

    protected def reader: evt.Reader[S, Scan[S]] = serializer
  }
}