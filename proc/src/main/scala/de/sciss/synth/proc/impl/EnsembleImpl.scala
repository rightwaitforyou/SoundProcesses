/*
 *  Ensemble.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.{event => evt}
import de.sciss.lucre.event.{InMemory, EventLike, Sys}
import de.sciss.lucre.expr.{Expr, Long => LongEx, Boolean => BooleanEx}
import de.sciss.serial.{DataInput, DataOutput}
import de.sciss.synth.proc
import de.sciss.synth.proc.Ensemble.Update

object EnsembleImpl {
  private final val COOKIE = 0x456E00  // "En\0"

  def apply[S <: Sys[S]](folder: FolderElem.Obj[S], offset: Expr[S, Long], playing: Expr[S, Boolean])
                        (implicit tx: S#Tx): Ensemble[S] = {
    val targets = evt.Targets[S]
    new Impl(targets, folder, offset, playing)
  }

  def serializer[S <: Sys[S]]: evt.NodeSerializer[S, Ensemble[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[InMemory]

  private final class Ser[S <: Sys[S]] extends evt.NodeSerializer[S, Ensemble[S]] {
    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Ensemble[S] with evt.Node[S] = {
      val cookie  = in.readInt()
      if (cookie != COOKIE) sys.error(s"Unexpected cookie (found $cookie, expected $COOKIE)")
      // val folder  = Folder   .read(in, access)
      val folder  = Obj.readT[S, FolderElem](in, access)
      val offset  = LongEx   .read(in, access)
      val playing = BooleanEx.read(in, access)
      new Impl(targets, folder, offset, playing)
    }
  }

  // ---- Elem ----

  object ElemImpl extends proc.impl.ElemImpl.Companion[Ensemble.Elem] {
    val typeID = Ensemble.typeID

    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                   (implicit tx: S#Tx): Ensemble.Elem[S] with evt.Node[S] = {
      val peer = Ensemble.read(in, access)
      new ElemImpl(targets, peer)
    }

    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Ensemble.Elem[S] =
      sys.error("Constant ProcGroup not supported")

    def apply[S <: Sys[S]](peer: Ensemble[S])(implicit tx: S#Tx): Ensemble.Elem[S] =
      new ElemImpl(evt.Targets[S], peer)
  }

  private final class ElemImpl[S <: Sys[S]](val targets: evt.Targets[S],
                                            val peer: Ensemble[S])
    extends proc.impl.ElemImpl.Active[S] with Ensemble.Elem[S] {

    def typeID = Ensemble.typeID
    def prefix = "Ensemble"

    protected def peerEvent = peer.changed

    def mkCopy()(implicit tx: S#Tx): Ensemble.Elem[S] = {
      val folderOrig  = peer.folder
      val folderCopy: FolderElem.Obj[S] = Obj.copyT[S, FolderElem](folderOrig, folderOrig.elem)  // P.I.T.A.
      val offsetCopy  = peer.offset
      val playingCopy = peer.playing
      val copy = Ensemble(folderCopy, offsetCopy, playingCopy)
      Ensemble.Elem(copy)
//
//      val newPeer     = _Proc[S]
//      newPeer.graph() = peer.graph()
//      // peer.scans.keys.foreach(newPeer.scans.add)
//      peer.scans.iterator.foreach { case (key, scan) =>
//        val scanNew = newPeer.scans.add(key)
//        scan.sources.foreach { link =>
//          scanNew.addSource(link)
//        }
//        scan.sinks.foreach { link =>
//          scanNew.addSink(link)
//        }
//      }
//      Proc(newPeer)
    }
  }

  // ---- impl ----

  private final class Impl[S <: Sys[S]](val targets: evt.Targets[S], folderObj: FolderElem.Obj[S],
                                        offsetEx: Expr[S, Long], playingEx: Expr[S, Boolean])
    extends Ensemble[S]
    with evt.impl.StandaloneLike[S, Ensemble.Update[S], Ensemble[S]] {

    override def toString() = s"Ensemble$id"

    def folder (implicit tx: S#Tx): FolderElem.Obj[S] = folderObj
    def offset (implicit tx: S#Tx): Expr[S, Long]     = offsetEx
    def playing(implicit tx: S#Tx): Expr[S, Boolean]  = playingEx

    def changed: EventLike[S, Update[S]] = this

    protected def writeData(out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      folderObj.write(out)
      offsetEx .write(out)
      playingEx.write(out)
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = ()

    // ---- event ----

    protected def reader: evt.Reader[S, Ensemble[S]] = serializer[S]

    def connect   ()(implicit tx: S#Tx): Unit = {
      folderObj .changed ---> this
      offsetEx .changed ---> this
      playingEx.changed ---> this
    }

    def disconnect()(implicit tx: S#Tx): Unit = {
      folderObj .changed -/-> this
      offsetEx .changed -/-> this
      playingEx.changed -/-> this
    }

    def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Ensemble.Update[S]] = {
      val folderEvt = folderObj.elem.peer.changed
      val l1 = if (pull.contains(folderEvt))
        pull(folderEvt).fold(List.empty[Ensemble.Change[S]])(Ensemble.Folder(_) :: Nil)
      else Nil

      val offsetEvt = offsetEx.changed
      val l2 = if (pull.contains(offsetEvt))
        pull(offsetEvt).fold(l1)(Ensemble.Offset[S](_) :: l1)
      else l1

      val playingEvt = playingEx.changed
      val l3 = if (pull.contains(playingEvt))
        pull(playingEvt).fold(l2)(Ensemble.Playing[S](_) :: l2)
      else l2

      if (l3.isEmpty) None else Some(Ensemble.Update(this, l3))
    }
  }
}
