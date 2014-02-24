/*
 *  package.scala
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

  /** Initializes types and thus installs some type extensions. */
  def initTypes(): Unit = {
    Booleans  .install
    Curves    .install
    Doubles   .install
    Ints      .install
    Longs     .install
    SpanLikes .install
    Spans     .install
    Strings   .install

    DoubleVec .install
    IntVec    .install
    LongVec   .install
  }
}