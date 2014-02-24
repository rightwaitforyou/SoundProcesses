/*
 *  VecLikeType.scala
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

package de.sciss.lucre.synth.expr

import scala.collection.immutable.{IndexedSeq => Vec}
import de.sciss.serial.{ImmutableSerializer, DataOutput, DataInput}
import de.sciss.lucre.event.{Targets, Sys}
import de.sciss.lucre.bitemp.BiType

trait VecLikeType[A] extends BiTypeImpl[Vec[A]] {
  def element: BiType[A]

  private[this] lazy val valueSer = ImmutableSerializer.indexedSeq[A](element.ValueSer)

  final def readValue (               in : DataInput ): Vec[A]  = valueSer.read(in)
  final def writeValue(value: Vec[A], out: DataOutput): Unit    = valueSer.write(value, out)

  lazy val install: Unit = ()
}

object IntVec extends VecLikeType[Int] {
  final val element = Ints

  final val typeID = 0x2000 | Ints.typeID
}

object LongVec extends VecLikeType[Long] {
  final val element = Longs

  final val typeID = 0x2000 | Longs.typeID
}

object DoubleVec extends VecLikeType[Double] {
  final val element = Doubles

  final val typeID = 0x2000 | Doubles.typeID
}
