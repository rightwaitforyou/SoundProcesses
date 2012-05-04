package de.sciss.synth.expr

import de.sciss.lucre.expr.BiType

trait BiTypeImpl[ A ] extends BiType[ A ] {
//   final protected def readLongExpr[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Expr[ S, Long ] =
//      Longs.readExpr( in, access )

   final def longType : BiType[ Long ] = Longs
}
