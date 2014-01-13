/*
 *  DummySerializerFactory.scala
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

package de.sciss.synth.proc.impl

import de.sciss.lucre.stm
import de.sciss.serial.{DataOutput, DataInput, Serializer}

object DummySerializerFactory {
  def apply[I <: stm.Sys[I]]: DummySerializerFactory[I] = anySer.asInstanceOf[DummySerializerFactory[I]]

  private val anySer = new Impl[stm.InMemory, Nothing]

  private class Impl[I <: stm.Sys[I], A]
    extends Serializer[I#Tx, I#Acc, A] with DummySerializerFactory[I] {

    implicit def dummySerializer[A1]: Serializer[I#Tx, I#Acc, A1] =
      this.asInstanceOf[Serializer[I#Tx, I#Acc, A1]]

    def write(v: A, out: DataOutput) = ()

    def read(in: DataInput, access: I#Acc)(implicit tx: I#Tx): A =
      sys.error("Operation not supported")
  }
}

trait DummySerializerFactory[I <: stm.Sys[I]] {
  implicit def dummySerializer[A]: Serializer[I#Tx, I#Acc, A]
}