///*
// *  CommonSerializers.scala
// *  (SoundProcesses)
// *
// *  Copyright (c) 2010-2013 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is free software; you can redistribute it and/or
// *  modify it under the terms of the GNU General Public License
// *  as published by the Free Software Foundation; either
// *  version 2, june 1991 of the License, or (at your option) any later version.
// *
// *  This software is distributed in the hope that it will be useful,
// *  but WITHOUT ANY WARRANTY; without even the implied warranty of
// *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// *  General Public License for more details.
// *
// *  You should have received a copy of the GNU General Public
// *  License (gpl.txt) along with this software; if not, write to the Free Software
// *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.lucre.synth
//package expr
//
//import annotation.switch
//import de.sciss.serial.{Serializer, DataInput, DataOutput, ImmutableSerializer}
//import de.sciss.synth
//import de.sciss.lucre.stm
//import de.sciss.synth.Curve._
//
//object CommonSerializers {
//  implicit object Curve extends ImmutableSerializer[synth.Curve] {
//    def write(shape: synth.Curve, out: DataOutput): Unit = {
//      out.writeInt(shape.id)
//      shape match {
//        case parametric(c)  => out.writeFloat(c)
//        case _              =>
//      }
//    }
//
//    def read(in: DataInput): synth.Curve = {
//      (in.readInt(): @switch) match {
//        case step       .id => step
//        case linear     .id => linear
//        case exponential.id => exponential
//        case sine       .id => sine
//        case welch      .id => welch
//        case parametric .id => parametric(in.readFloat())
//        case squared    .id => squared
//        case cubed      .id => cubed
//        case other          => sys.error("Unexpected envelope shape ID " + other)
//      }
//    }
//  }
//
//  // this is plain stupid... another reason why the scan should reproduce the proc and key (problem though: proc -> timed-proc)
//  implicit def Identifier[S <: stm.Sys[S]]: Serializer[S#Tx, S#Acc, S#ID] = anyIDSer.asInstanceOf[Serializer[S#Tx, S#Acc, S#ID]]
//
//  private val anyIDSer = new IDSer[stm.InMemory]
//
//  private final class IDSer[S <: stm.Sys[S]] extends Serializer[S#Tx, S#Acc, S#ID] {
//    def write(id: S#ID, out: DataOutput): Unit = id.write(out)
//
//    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): S#ID = tx.readID(in, access)
//  }
//}
