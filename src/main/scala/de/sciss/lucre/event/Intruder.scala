//package de.sciss.lucre.event
//
//import de.sciss.lucre.stm.Sys
//
//object Intruder {
//   def --->[ S <: Sys[ S ]]( e: EventLike[ S, _, _ ], r: ExpandedSelector[ S ])( implicit tx: S#Tx ) {
//      e ---> r
//   }
//
//   def -/->[ S <: Sys[ S ]]( e: EventLike[ S, _, _ ], r: ExpandedSelector[ S ])( implicit tx: S#Tx ) {
//      e -/-> r
//   }
//}
