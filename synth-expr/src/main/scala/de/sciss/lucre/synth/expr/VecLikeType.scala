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

  protected def readTuple[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                      (implicit tx: S#Tx): ReprNode[S] = sys.error("Invalid cookie " + cookie)

  private val valueSer = ImmutableSerializer.indexedSeq[A](element.ValueSer)

  def writeValue(value: Vec[A], out: DataOutput): Unit = valueSer.write(value, out)
  def readValue(in: DataInput): Vec[A] = valueSer.read(in)

  // def typeID: Int = ...

  // def newConst[S <: Sys[S]](vec: Vec[A])         : Expr.Const[S, Vec[A]]

  // def wrap[S <: Sys[S]](vec: Vec[Expr[S, A]]): Expr[S, Vec[A]]

  // def newVar  [S <: Sys[S]](vec: Vec[Expr[S, A]]): Expr.Var  [S, Vec[A]]
}
//trait VecExpr[S <: Sys[S], +A] extends Expr[S, Vec[A]] {
//  def size: Expr[S, Int]
//  def get(index: Expr[S, Int]): Expr[S, Option[A]] // OptionExpr[S, A]
//}

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
