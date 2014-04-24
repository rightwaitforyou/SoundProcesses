/*
 *  Folder.scala
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

import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr
import impl.{FolderImpl => Impl}
import de.sciss.serial

object Folder {
  type Peer[S <: Sys[S]] = expr.List.Modifiable[S, Object[S], Object.Update[S]]

  def empty[S <: Sys[S]]()(implicit tx: S#Tx): Folder[S] = Impl.empty[S]()

  def apply[S <: Sys[S]](peer: Peer[S])(implicit tx: S#Tx): Folder[S] =
    Impl(peer)

  implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Folder[S]] =
    Impl.serializer[S]
}
trait Folder[S <: Sys[S]] extends Elem[S] { type Peer = Folder.Peer[S] }
