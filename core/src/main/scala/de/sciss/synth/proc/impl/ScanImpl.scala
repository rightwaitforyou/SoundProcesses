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

import de.sciss.lucre.event.{Targets, impl => evti}
import de.sciss.lucre.expr.List
import de.sciss.lucre.stm.impl.{ElemSerializer, ObjSerializer}
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.annotation.switch
import scala.collection.immutable.{IndexedSeq => Vec}

object ScanImpl {
  import Scan.Link

  private final val SER_VERSION = 0x5370  // was "Sn"

  sealed trait Update[S]

  def apply[S <: Sys[S]](proc: Proc[S], key: String)(implicit tx: S#Tx): Scan[S] = {
    val targets = evt.Targets[S]
    val list    = List.Modifiable[S, Link]
    new Impl(targets, proc, key, list)
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Scan[S] =
    serializer[S].read(in, access)

  def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Scan[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  private final class Ser[S <: Sys[S]] extends ObjSerializer[S, Scan[S]] {
    def tpe: Obj.Type = Scan
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Scan[S] = {
    val targets = Targets.read(in, access)
    val serVer = in.readShort()
    if (serVer != SER_VERSION) sys.error(s"Incompatible serialized version (found $serVer, required $SER_VERSION)")

    val proc  = Proc.read(in, access)
    val key   = in.readUTF()
    val list  = List.Modifiable.read[S, Link[S]](in, access)
    new Impl(targets, proc, key, list)
  }

  implicit def linkSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Link[S]] = anyLinkSer.asInstanceOf[LinkSer[S]]

  private val anyLinkSer = new LinkSer[NoSys]

  private final class LinkSer[S <: Sys[S]] extends ElemSerializer[S, Link[S]] {
    def tpe: Elem.Type = Link
  }

  def readIdentifiedLink[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Link[S] = {
    val constCookie = in.readByte()
    if (constCookie != 3) sys.error(s"Unexpected cookie, found $constCookie, expected 3")
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

  private final val filterAll: Any => Boolean = _ => true

  private final class Impl[S <: Sys[S]](protected val targets : evt.Targets[S],
                                        _proc: Proc[S], val key: String,
                                        protected val list    : List.Modifiable[S, Link[S]])
    extends Scan[S]
    with evti.SingleNode[S, Scan.Update[S]] { in =>

    def tpe: Obj.Type = Scan

    override def toString: String = s"Scan$id"

    def proc(implicit tx: S#Tx): Proc[S] = _proc

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      val outList = List.Modifiable[Out, Link]
      val out     = new Impl(Targets[Out], context(_proc), key, outList)
      context.defer(in, out) {
        val filter  = context.getHint(proc, Proc.hintFilterLinks).asInstanceOf[Option[Proc[S] => Boolean]]
          .getOrElse(filterAll)
        in.list.iterator.foreach {
          case Scan.Link.Scan(peer) if !filter(peer.proc) =>
          case link => out.add(context(link))
        }
      }
      out // .connect()
    }

    def iterator(implicit tx: S#Tx): Iterator[Link[S]] = list.iterator

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
      _proc.write(out)
      out.writeUTF(key)
      list.write(out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      list.dispose()
      // disconnect()
    }
  }
}