//package de.sciss.synth.proc
//
//import de.sciss.lucre.bitemp.BiPin
//import de.sciss.lucre.stm.Sys
//
//object ParamMap {
////   trait Var[ S <: Sys[ S ]] extends ParamMap[ S ] {
////      override def apply( key: String ) : BiPin.ExprVar[ S, Param ]
////      override def get( key: String ) : Option[ BiPin.ExprVar[ S, Param ]]
////   }
//}
//trait ParamMap[ S <: Sys[ S ]] {
//   def apply( key: String )( implicit tx: S#Tx ) : BiPin.Expr[ S, Param ]
//   def get( key: String )( implicit tx: S#Tx ) : Option[ BiPin.Expr[ S, Param ]]
//   def keys( implicit tx: S#Tx ): Set[ String ] // txn.Iterator[ S#Tx, String ]
//   def entriesAt( time: Long )( implicit tx: S#Tx ) : Map[ String, Param ]
//}