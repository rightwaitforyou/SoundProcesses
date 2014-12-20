package de.sciss.lucre.synth.expr
package impl

import de.sciss.lucre.stm
import de.sciss.serial.{DataInput, DataOutput, Serializer}

final class IDSer[S <: stm.Sys[S]] extends Serializer[S#Tx, S#Acc, S#ID] {
  def write(id: S#ID, out: DataOutput): Unit = id.write(out)

  def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): S#ID = tx.readID(in, access)
}
