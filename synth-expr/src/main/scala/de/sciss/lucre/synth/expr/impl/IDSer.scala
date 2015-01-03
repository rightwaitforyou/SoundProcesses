/*
 *  IDSer.scala
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

package de.sciss.lucre.synth.expr
package impl

import de.sciss.lucre.stm
import de.sciss.serial.{DataInput, DataOutput, Serializer}

final class IDSer[S <: stm.Sys[S]] extends Serializer[S#Tx, S#Acc, S#ID] {
  def write(id: S#ID, out: DataOutput): Unit = id.write(out)

  def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): S#ID = tx.readID(in, access)
}
