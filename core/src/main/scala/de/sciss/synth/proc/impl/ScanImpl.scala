/*
 *  ScanImpl.scala
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

import de.sciss.lucre.{event => evt, data, expr}
import evt.{impl => evti, Event, Sys}
import annotation.switch
import expr.List
import de.sciss.serial.{DataOutput, Serializer, DataInput}
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.lucre.synth.InMemory

object ScanImpl {
  import Scan.Link

  private final val SER_VERSION = 0x536F  // was "Sn"

  sealed trait Update[S]

  def apply[S <: Sys[S]](implicit tx: S#Tx): Scan[S] = {
    val targets = evt.Targets[S] // XXX TODO: partial?
    val list    = List.Modifiable[S, Link[S]]
    new Impl(targets, list)
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
      val serVer = in.readShort()
      if (serVer != SER_VERSION) sys.error(s"Incompatible serialized (found $serVer, required $SER_VERSION)")

      val list = List.Modifiable.read[S, Link[S]](in, access)
      new Impl(targets, list)
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

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Link[S] =
      (in.readByte(): @switch) match {
        case 0 =>
          val peer = Grapheme.read[S](in, access)
          Link.Grapheme(peer)
        case 1 =>
          val peer = Scan.read[S](in, access)
          Link.Scan(peer)
        case cookie => sys.error(s"Unexpected cookie $cookie")
      }
  }

  // TODO: the crappy sinkList is only needed because the id map does not have an iterator...
  // we should really figure out how to add iterator functionality to the id map!!!
  private final class Impl[S <: Sys[S]](protected val targets : evt.Targets[S],
                                        protected val list    : List.Modifiable[S, Link[S], Unit])
    extends Scan[S]
    with evti.StandaloneLike[S, Scan.Update[S], Scan[S]]
    with evti.Generator[S, Scan.Update[S], Scan[S]]
    with evti.Root[S, Scan.Update[S]] {

    override def toString() = s"Scan$id"

    def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Link[S]] = list.iterator

    def add(link: Link[S])(implicit tx: S#Tx): Boolean = {
      if (list.indexOf(link) >= 0) return false
      list.addLast(link) // addHead faster than addLast; but perhaps we should use addLast to have a better iterator order?
      link match {
        case Link.Scan(peer) => peer.add(Link.Scan(this))
        case _ =>
      }
      fire(Scan.Update(this, Vec(Scan.Added(link))))
      true
    }

    def remove(link: Link[S])(implicit tx: S#Tx): Boolean = {
      if (list.indexOf(link) < 0) return false
      list.remove(link)
      link match {
        case Link.Scan(peer) => peer.remove(Link.Scan(this))
        case _ =>
      }
      fire(Scan.Update(this, Vec(Scan.Removed(link))))
      true
    }

    def changed: Event[S, Scan.Update[S], Scan[S]] = this

//    def connect   ()(implicit tx: S#Tx): Unit = list.changed ---> this
//    def disconnect()(implicit tx: S#Tx): Unit = list.changed -/-> this

//    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Scan.Update[S]] = {
//      val gen = pull.isOrigin(this)
//      val u1 = if (!gen) Vec.empty else pull.resolve[Scan.Update[S]].changes
//      val u2 = u1
////      val u2 = if ( gen) u1        else pull(graphemeSourceList.changed).fold(u1) { ll =>
////        val gcs = ll.changes.collect {
////          case List.Element(_, gc) => Scan.GraphemeChange(gc.grapheme, gc.changes)
////        }
////        if (u1.isEmpty) gcs else if (gcs.isEmpty) u1 else u1 ++ gcs
////      }
//      if (u2.isEmpty) None else Some(Scan.Update(this, u2))
//    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      list.write(out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = list.dispose()

    protected def reader: evt.Reader[S, Scan[S]] = serializer
  }
}