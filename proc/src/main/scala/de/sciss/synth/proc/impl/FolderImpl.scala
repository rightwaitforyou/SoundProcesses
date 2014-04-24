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

object FolderImpl extends ElemImpl.Companion[Folder] {
  final val typeID = 0x10000

  def empty[S <: Sys[S]]()(implicit tx: S#Tx): Folder[S] =
    apply(expr.List.Modifiable[S, Object[S], Object.Update[S]])

  def apply[S <: Sys[S]](peer: Folder.Peer[S])(implicit tx: S#Tx): Folder[S] = {
    val targets = evt.Targets[S]
    new Impl[S](targets, peer)
  }

  // ---- Elem.Extension ----
  /** Read identified active element */
  def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                 (implicit tx: S#Tx): Folder[S] with evt.Node[S] = {
    val peer = expr.List.Modifiable.read[S, Object[S], Object.Update[S]](in, access)
    new Impl[S](targets, peer)
  }

  /** Read identified constant element */
  def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Folder[S] =
    sys.error("Constant Folder not supported")

  // ---- implementation ----

  private final class Impl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                        val peer: Folder.Peer[S])
    extends Folder[S]
    with ElemImpl.Active[S] {

    def typeID = FolderImpl.typeID
    def prefix = "Folder"

    // XXX TODO: not nice
    override def toString() = s"$prefix$id"

    def mkCopy()(implicit tx: S#Tx): Folder[S] = FolderImpl(peer) // XXX TODO - copy list?
  }
}
