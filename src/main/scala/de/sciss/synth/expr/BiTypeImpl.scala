package de.sciss.synth.expr

import de.sciss.lucre.bitemp.{SpanLike, BiType}

trait BiTypeImpl[ A ] extends BiType[ A ] {
//   final protected def readLongExpr[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Expr[ S, Long ] =
//      Longs.readExpr( in, access )

   final def longType : BiType[ Long ] = Longs
   final def spanLikeType: BiType[ SpanLike ] = SpanLikes
}
