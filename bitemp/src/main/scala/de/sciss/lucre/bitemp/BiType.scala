/*
 *  BiType.scala
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

package de.sciss.lucre
package bitemp

import expr.Type
import de.sciss.span.SpanLike
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.lucre.{event => evt}

object BiType {
  trait TupleReader[+A] {
    def name: String

    /** Arity of the operator */
    val arity: Int
    /** Lowest id of handled operators */
    val opLo : Int
    /** Highest id of handled operators. Note: This value is _inclusive_ */
    val opHi : Int

    def readTuple[S <: evt.Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                  (implicit tx: S#Tx): expr.Expr.Node[S, A]

    override def toString = s"$name [$arity-ary, lo = $opLo, hi = $opHi]"
  }
}
/** Extends `Type` with a an expression form which acts as a cursor on a bi-temporal object. */
trait BiType[A] extends Type[A] {
  def typeID: Int

  implicit object ValueSer extends ImmutableSerializer[A] {
    def write(v: A, out: DataOutput): Unit = writeValue(v, out)

    def read(in: DataInput): A = readValue(in)
  }

  def longType: BiType[Long]

  def spanLikeType: BiType[SpanLike]
}
