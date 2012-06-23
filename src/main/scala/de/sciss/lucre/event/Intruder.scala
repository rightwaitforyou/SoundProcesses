//package de.sciss.lucre.event
//
//import de.sciss.lucre.stm.Sys
//
//object Intruder {
////   def --->[ S <: Sys[ S ]]( e: EventLike[ S, _, _ ], r: ExpandedSelector[ S ])( implicit tx: S#Tx ) {
////      e ---> r
////   }
////
////   def -/->[ S <: Sys[ S ]]( e: EventLike[ S, _, _ ], r: ExpandedSelector[ S ])( implicit tx: S#Tx ) {
////      e -/-> r
////   }
//   def devirtualize[ S <: Sys[ S ]]( sel: VirtualNodeSelector[ S ], reader: Reader[ S, Node[ S ]])( implicit tx: S#Tx ) : NodeSelector[ S, _ ] =
//      sel.devirtualize( reader )
//
//   def devirtualizeNode[ S <: Sys[ S ]]( sel: VirtualNodeSelector[ S ], reader: Reader[ S, Node[ S ]])( implicit tx: S#Tx ) : Node[ S ] =
//      sel.devirtualize( reader ).node
//}
