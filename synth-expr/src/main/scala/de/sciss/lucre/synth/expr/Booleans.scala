/*
 *  Booleans.scala
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