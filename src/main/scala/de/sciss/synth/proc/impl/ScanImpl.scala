/*
 *  ScanImpl.scala
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
package impl

import de.sciss.lucre.{event => evt, stm, data, expr}
import stm.IdentifierMap
import evt.{impl => evti, Event, Sys}
import annotation.switch
import expr.LinkedList
import proc.Scan
import de.sciss.serial.{DataOutput, Serializer, DataInput}

object ScanImpl {
  import Scan.Link

  private final val SER_VERSION = 0x536E  // "Sn"

  sealed trait Update[S]

  def apply[S <: Sys[S]](implicit tx: S#Tx): Scan[S] = {
    val targets   = evt.Targets[S] // XXX TODO: partial?
    // val id        = targets.id
    val sourceMap = tx.newDurableIDMap[Link[S]]
    val sourceList= LinkedList.Modifiable[S, Link[S]]
    val sinkMap   = tx.newDurableIDMap[Link[S]]
    val sinkList  = LinkedList.Modifiable[S, Link[S]]
    new Impl(targets, sourceMap, sourceList, sinkMap, sinkList)
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Scan[S] = {
    serializer[S].read(in, access)
  }

  implicit def serializer[S <: Sys[S]]: evt.NodeSerializer[S, Scan[S]] =
    anySer.asInstanceOf[evt.NodeSerializer[S, Scan[S]]]

  private type I = evt.InMemory

  private val anySer: evt.NodeSerializer[I, Scan[I]] = new Ser[I]

  private final class Ser[S <: Sys[S]] extends evt.NodeSerializer[S, Scan[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Scan[S] = {
      val serVer    = in.readShort()
      require(serVer == SER_VERSION, s"Incompatible serialized (found $serVer, required $SER_VERSION)")
      // val id        = targets.id
      val sourceMap = tx.readDurableIDMap[Link[S]](in)
      val sourceList= LinkedList.Modifiable.read[S, Link[S]](in, access)
      val sinkMap   = tx.readDurableIDMap[Link[S]](in)
      val sinkList  = LinkedList.Modifiable.read[S, Link[S]](in, access)
      new Impl(targets, sourceMap, sourceList, sinkMap, sinkList)
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
                                        protected val sourceMap: IdentifierMap[S#ID, S#Tx, Link[S]],
                                        protected val sourceList: LinkedList.Modifiable[S, Link[S], Unit],
                                        protected val sinkMap: IdentifierMap[S#ID, S#Tx, Link[S]],
                                        protected val sinkList: LinkedList.Modifiable[S, Link[S], Unit])
    extends Scan[S]
    with evti.StandaloneLike[S, Scan.Update[S], Scan[S]]
    with evti.Generator[S, Scan.Update[S], Scan[S]] {
    override def toString() = "Scan" + id

    def sinks  (implicit tx: S#Tx): data.Iterator[S#Tx, Link[S]] = sinkList  .iterator
    def sources(implicit tx: S#Tx): data.Iterator[S#Tx, Link[S]] = sourceList.iterator

    def addSink(sink: Link[S])(implicit tx: S#Tx): Boolean = {
      val sinkID = sink.id
      if (sinkMap.contains(sinkID)) return false

      sinkMap.put(sinkID, sink)
      sinkList.addLast(sink) // addHead faster than addLast; but perhaps we should use addLast to have a better iterator order?
      sink match {
        case Link.Scan(peer) => peer.addSource(Link.Scan(this))
        case _ =>
      }
      fire(Scan.SinkAdded(this, sink))
      true
    }

    def removeSink(sink: Link[S])(implicit tx: S#Tx): Boolean = {
      val sinkID = sink.id
      if (!sinkMap.contains(sinkID)) return false
      sinkMap.remove(sinkID)
      sinkList.remove(sink)
      sink match {
        case Link.Scan(peer) => peer.removeSource(Link.Scan(this))
        case _ =>
      }
      fire(Scan.SinkRemoved(this, sink))
      true
    }

    def addSource(source: Link[S])(implicit tx: S#Tx): Boolean = {
      val sourceID = source.id
      if (sourceMap.contains(sourceID)) return false

      sourceMap.put(sourceID, source)
      sourceList.addLast(source)
      source match {
        case Link.Scan(peer) =>
          peer.addSink(Link.Scan(this))
        case Link.Grapheme(peer) if isConnected =>
          peer.changed ---> this
        case _ =>
      }
      fire(Scan.SourceAdded(this, source))
      true
    }

    @inline private def isConnected(implicit tx: S#Tx): Boolean = targets.nonEmpty

    def removeSource(source: Link[S])(implicit tx: S#Tx): Boolean = {
      val sourceID = source.id
      if (!sourceMap.contains(sourceID)) return false
      sourceMap.remove(sourceID)
      sourceList.remove(source)
      source match {
        case Link.Scan(peer) =>
          peer.removeSink(Link.Scan(this))
        case Link.Grapheme(peer) if isConnected =>
          peer.changed -/-> this
      }
      fire(Scan.SourceRemoved(this, source))
      true
    }

    def changed: Event[S, Scan.Update[S], Scan[S]] = this

    def connect()(implicit tx: S#Tx): Unit =
      sources.foreach {
        case Scan.Link.Grapheme(peer) => peer.changed ---> this
        case _ =>
      }

    def disconnect()(implicit tx: S#Tx): Unit =
      sources.foreach {
        case Scan.Link.Grapheme(peer) => peer.changed -/-> this
        case _ =>
      }

    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Scan.Update[S]] = {
      if (pull.parents(this).isEmpty) {
        pull.resolve[Scan.Update[S]]
      } else {
        ???
        //        source.flatMap {
        //          case Scan.Link.Grapheme(peer) =>
        //            pull(peer.changed).map(Scan.SourceUpdate(this, _))
        //          case _ => None
        //        }
      }
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      sourceMap .write(out)
      sourceList.write(out)
      sinkMap   .write(out)
      sinkList  .write(out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      sourceMap .dispose()
      sourceList.dispose()
      sinkMap   .dispose()
      sinkList  .dispose()
    }

    protected def reader: evt.Reader[S, Scan[S]] = serializer
  }
}