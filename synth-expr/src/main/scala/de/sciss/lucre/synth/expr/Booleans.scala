/*
 *  Booleans.scala
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

package de.sciss.lucre.synth
package expr

import de.sciss.lucre.{event => evt}
import evt.{Sys, Targets}
import de.sciss.serial.{DataOutput, DataInput}

object Booleans extends BiTypeImpl[Boolean] {
  final val typeID = 6

  def readValue(in: DataInput): Boolean = in.readBoolean()

  def writeValue(value: Boolean, out: DataOutput): Unit = out.writeBoolean(value)

  final class Ops[S <: Sys[S]](ex: Ex[S]) {

  }

  // ---- protected ----

  def readTuple[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])(implicit tx: S#Tx): ExN[S] =
  //   case 3 =>
  //      readCursor[ S ]( in, access, targets )
    sys.error("Invalid cookie " + cookie)
}