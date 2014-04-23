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
import de.sciss.serial
import de.sciss.serial.DataInput

object FolderImpl extends Elem.Extension {
  final val typeID = 0x10000

  def empty[S <: Sys[S]]()(implicit tx: S#Tx): Folder[S] =
    apply(expr.List.Modifiable[S, Object[S], Object.Update[S]])

  def apply[S <: Sys[S]](peer: Folder.Peer[S])(implicit tx: S#Tx): Folder[S] = ???

  implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Folder[S]] = ???

  // ---- Elem.Extension ----
  /** Read identified active element */
  def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                 (implicit tx: S#Tx): Elem[S] with evt.Node[S] = {
    ???
  }

  /** Read identified constant element */
  def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Elem[S] = {
    ???
  }

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
