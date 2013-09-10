package de.sciss.lucre.synth
package expr

import de.sciss.lucre.bitemp.BiType
import de.sciss.span.SpanLike

/**
  * typeIDs : 0 = byte, 1 = short, 2 = int, 3 = long, 4 = float, 5 = double, 6 = boolean, 7 = char,
  *           8 = string, 9 = spanlike, 10 = span, 11 = Grapheme.Value
  */
trait BiTypeImpl[A] extends BiType[A] {
  //   final protected def readLongExpr[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Expr[ S, Long ] =
  //      Longs.readExpr( in, access )

  final def longType    : BiType[Long    ] = Longs
  final def spanLikeType: BiType[SpanLike] = SpanLikes
}
