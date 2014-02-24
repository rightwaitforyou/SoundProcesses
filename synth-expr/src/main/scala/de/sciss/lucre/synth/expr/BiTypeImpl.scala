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

  private[this] val exts = new Array[Array[BiType.TupleReader[A]]](2)
  exts(0) = new Array[BiType.TupleReader[A]](0)
  exts(1) = new Array[BiType.TupleReader[A]](0)

  /** This method is not thread-safe. We assume extensions are registered upon application start only! */
  final def registerOp(reader: BiType.TupleReader[A]): Unit = {
    val arity = reader.arity
    require (arity > 0 && arity <= exts.length, s"Unsupported arity $arity")
    val opLo = reader.opLo
    val opHi = reader.opHi
    require (opLo <= opHi, s"Lo ($opLo) must be less than or equal hi ($opHi)")
    val extsA = exts(arity - 1)
    val idx0  = extsA.indexWhere(_.opLo > opHi)
    val idx   = if (idx0 < 0) extsA.length else idx0
    if (idx > 0) {
      val pred = extsA(idx - 1)
      require(pred.opHi < opLo, s"Extension overlap for $pred versus $reader")
    }
    exts(arity - 1) = extsA.patch(idx, reader :: Nil, 0)
  }

  /** The default implementation reads a type `Int` requiring to match `typeID`, followed by an operator id `Int`
    * which will be resolved using `readOpExtension`.
    */
  protected def readTuple[S <: evt.Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: evt.Targets[S])
                            (implicit tx: S#Tx): ExN[S] = {
    val tpe  = in.readInt()
    require(tpe == typeID, s"Invalid type id (found $tpe, required $typeID)")
    val opID = in.readInt()
    readOpExtension(cookie, opID, in, access, targets)
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
        if (ext.opHi >= op) return index
        low = index + 1
      } else {
        high = index - 1
      }
    }
    -1
  }

  final protected def readOpExtension[S <: evt.Sys[S]](arity: Int, op: Int, in: DataInput, access: S#Acc,
                                                       targets: evt.Targets[S])(implicit tx: S#Tx): ExN[S] = {
    if (arity > exts.length) sys.error(s"Unknown extension $arity-ary operator $op")
    val extA  = exts(arity - 1)
    val idx   = findExt(extA, op)
    require(idx >= 0, s"Unknown extension $arity-ary operator $op")
    val ext = extA(idx)
    ext.readTuple(op, in, access, targets)
  }
}