/*
 *  FolderImpl.scala
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

import de.sciss.lucre.{event => evt, expr}
import evt.Sys
import de.sciss.serial.DataInput

object FolderElemImpl extends ElemCompanionImpl[FolderElem] {
  // final val typeID = 0x10000
  def typeID = FolderElem.typeID

  //  def empty[S <: Sys[S]]()(implicit tx: S#Tx): FolderElem[S] =
  //    apply(expr.List.Modifiable[S, Obj[S], Obj.Update[S]])

  def apply[S <: Sys[S]](peer: Folder[S])(implicit tx: S#Tx): FolderElem[S] = {
    val targets = evt.Targets[S]
    new Impl[S](targets, peer)
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): FolderElem[S] =
    serializer[S].read(in, access)

  // ---- Elem.Extension ----
  /** Read identified active element */
  def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                 (implicit tx: S#Tx): FolderElem[S] with evt.Node[S] = {
    val peer = expr.List.Modifiable.read[S, Obj[S], Obj.Update[S]](in, access)
    new Impl[S](targets, peer)
  }

  /** Read identified constant element */
  def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): FolderElem[S] =
    sys.error("Constant Folder not supported")

  // ---- implementation ----

  private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                        val peer: Folder[S])
    extends FolderElem[S]
    with ActiveElemImpl[S] {

    def typeID = FolderElemImpl.typeID
    def prefix = "Folder"

    // XXX TODO: not nice
    override def toString() = s"$prefix$id"

    def mkCopy()(implicit tx: S#Tx): FolderElem[S] = FolderElemImpl(peer) // XXX TODO - copy list?
  }
}
