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
