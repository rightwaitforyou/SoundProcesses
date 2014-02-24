/*
 *  BiTypeImpl.scala
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
package expr

import de.sciss.lucre.bitemp.BiType
import de.sciss.span.SpanLike
import de.sciss.lucre.{event => evt}
import de.sciss.serial.DataInput

/**
  * typeIDs : 0 = Byte, 1 = Short, 2 = Int, 3 = Long, 4 = Float, 5 = Double, 6 = Boolean, 7 = Char,
  *           8 = String, 9 = SpanLike, 10 = Span, 11 = Grapheme.Value
  */
trait BiTypeImpl[A] extends BiType[A] {
  //   final protected def readLongExpr[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Expr[ S, Long ] =
  //      Longs.readExpr( in, access )

  final def longType    : BiType[Long    ] = Longs
  final def spanLikeType: BiType[SpanLike] = SpanLikes

  private[this] var extUnary = Array.empty[BiType.TupleReader[A]]

  /** This method is not thread-safe. We assume extensions are registered upon application start only! */
  final def registerUnaryOp(reader: BiType.TupleReader[A]): Unit = {
    val opLo = reader.opLo
    val opHi = reader.opHi
    require (opLo < opHi, s"Lo ($opLo) must be less than hi ($opHi)")
    val idx0  = extUnary.indexWhere(_.opLo >= opHi)
    val idx   = if (idx0 < 0) extUnary.length else idx0
    if (idx > 0) {
      val pred = extUnary(idx - 1)
      require(pred.opHi <= opLo, s"Extension overlap for $pred versus $reader")
    }
    extUnary = extUnary.patch(idx, reader :: Nil, 0)
  }

  private[this] def findExt(coll: Array[BiType.TupleReader[A]], op: Int): Int = {
    var index = 0
    var low   = 0
    var high  = coll.size - 1
    while ({
      index = (high + low) >> 1
      low  <= high
    }) {
      val ext = coll(index)
      if (ext.opLo <= op) {
        if (ext.opHi > op) return index
        low = index + 1
      } else {
        high = index - 1
      }
    }
    -1
  }

  final protected def readUnaryOpExtension[S <: evt.Sys[S]](op: Int, in: DataInput, access: S#Acc,
                                                            targets: evt.Targets[S])(implicit tx: S#Tx): ExN[S] = {
    val idx = findExt(extUnary, op)
    require(idx >= 0, s"Unknown extension unary operator $op")
    val ext = extUnary(idx)
    ext.readTuple(op, in, access, targets)
  }
}
