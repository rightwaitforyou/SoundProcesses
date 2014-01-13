/*
 *  package.scala
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

package de.sciss.lucre.synth

import de.sciss.serial.{Serializer, DataInput, DataOutput}
import de.sciss.lucre.stm

package object expr {
  // this is plain stupid... another reason why the scan should reproduce the proc and key (problem though: proc -> timed-proc)
  implicit def IdentifierSerializer[S <: stm.Sys[S]]: Serializer[S#Tx, S#Acc, S#ID] =
    anyIDSer.asInstanceOf[Serializer[S#Tx, S#Acc, S#ID]]

  private val anyIDSer = new IDSer[stm.InMemory]

  private final class IDSer[S <: stm.Sys[S]] extends Serializer[S#Tx, S#Acc, S#ID] {
    def write(id: S#ID, out: DataOutput): Unit = id.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): S#ID = tx.readID(in, access)
  }
}
