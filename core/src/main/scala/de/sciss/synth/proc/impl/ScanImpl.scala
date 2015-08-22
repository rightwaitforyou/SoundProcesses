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

import de.sciss.lucre.event.{impl => evti}
import de.sciss.lucre.expr.List
import de.sciss.lucre.stm.{NoSys, Obj, Sys}
import de.sciss.lucre.{data, event => evt}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.annotation.switch
import scala.collection.immutable.{IndexedSeq => Vec}

object ScanImpl {
  import Scan.Link

  private final val SER_VERSION = 0x536F  // was "Sn"

  sealed trait Update[S]

  def apply[S <: Sys[S]](implicit tx: S#Tx): Scan[S] = {
    val targets = evt.Targets[S]
    val list    = ??? // RRR List.Modifiable[S, Link[S]]
    new Impl(targets, list)
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Scan[S] = {
    serializer[S].read(in, access)
  }

  def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Scan[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  private final class Ser[S <: Sys[S]] extends Obj.Serializer[S, Scan[S]] {
    def typeID: Int = Scan.typeID

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Scan[S] = {
      val serVer = in.readShort()
      if (serVer != SER_VERSION) sys.error(s"Incompatible serialized version (found $serVer, required $SER_VERSION)")

      val list = ??? // RRR List.Modifiable.read[S, Link[S]](in, access)
      new Impl(targets, list)
    }
  }

  implicit def linkSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Link[S]] = anyLinkSer.asInstanceOf[LinkSer[S]]

  private val anyLinkSer = new LinkSer[NoSys]

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

  private final class Impl[S <: Sys[S]](protected val targets : evt.Targets[S],
                                        protected val list    : List.Modifiable[S, Link[S]])
    extends Scan[S]
    with evti.SingleNode[S, Scan.Update[S]] {

    def typeID: Int = Scan.typeID

    override def toString: String = s"Scan$id"

    def iterator(implicit tx: S#Tx): data.Iterator[S#Tx, Link[S]] = list.iterator

    def isEmpty (implicit tx: S#Tx): Boolean = list.isEmpty
    def nonEmpty(implicit tx: S#Tx): Boolean = list.nonEmpty

    def add(link: Link[S])(implicit tx: S#Tx): Boolean = {
      if (list.indexOf(link) >= 0) return false
      list.addLast(link) // addHead faster than addLast; but perhaps we should use addLast to have a better iterator order?
      link match {
        case Link.Scan(peer) => peer.add(Link.Scan(this))
        case _ =>
      }
      changed.fire(Scan.Update(this, Vec(Scan.Added(link))))
      true
    }

    def remove(link: Link[S])(implicit tx: S#Tx): Boolean = {
      if (list.indexOf(link) < 0) return false
      list.remove(link)
      link match {
        case Link.Scan(peer) => peer.remove(Link.Scan(this))
        case _ =>
      }
      changed.fire(Scan.Update(this, Vec(Scan.Removed(link))))
      true
    }

    object changed extends Changed
      with evti.Generator[S, Scan.Update[S]]
      with evti.Root[S, Scan.Update[S]]

    protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      list.write(out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      list.dispose()
      // disconnect()
    }
  }
}