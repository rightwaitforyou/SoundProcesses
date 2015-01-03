/*
 *  VecLikeType.scala
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

import scala.collection.immutable.{IndexedSeq => Vec}
import de.sciss.serial.{ImmutableSerializer, DataOutput, DataInput}
import de.sciss.lucre.event.{Targets, Sys}
import de.sciss.lucre.expr.ExprType
import de.sciss.lucre.expr
import de.sciss.lucre

trait VecLikeType[A] extends expr.impl.ExprTypeImplA[Vec[A]] {
  def element: ExprType[A]

  // XXX TODO: this should be provided by ExprTypeImpl
  private[this] object ElemValueSer extends ImmutableSerializer[A] {
    def read (      in : DataInput ): A     = element.readValue (   in )
    def write(v: A, out: DataOutput): Unit  = element.writeValue(v, out)
  }
  private[this] lazy val valueSer = ImmutableSerializer.indexedSeq[A](ElemValueSer) // (element.ValueSer)

  final def readValue (               in : DataInput ): Vec[A]  = valueSer.read(in)
  final def writeValue(value: Vec[A], out: DataOutput): Unit    = valueSer.write(value, out)

  // lazy val install: Unit = ()
}

object IntVec extends VecLikeType[Int] {
  final val element = lucre.expr.Int

  final val typeID = 0x2000 | lucre.expr.Int.typeID
}

object LongVec extends VecLikeType[Long] {
  final val element = lucre.expr.Long

  final val typeID = 0x2000 | lucre.expr.Long .typeID
}

object DoubleVec extends VecLikeType[Double] {
  final val element = lucre.expr.Double

  final val typeID = 0x2000 | lucre.expr.Double.typeID
}
