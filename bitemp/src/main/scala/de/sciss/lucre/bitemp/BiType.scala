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

/**
 * Extends `Type` with a an expression form which acts as a cursor on a bi-temporal object.
 */
trait BiType[A] extends Type[A] {
  def typeID: Int

  implicit object ValueSer extends ImmutableSerializer[A] {
    def write(v: A, out: DataOutput): Unit = writeValue(v, out)

    def read(in: DataInput): A = readValue(in)
  }

  def longType: BiType[Long]

  def spanLikeType: BiType[SpanLike]
}
