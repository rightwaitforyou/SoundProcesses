//package de.sciss.lucre
//package bitemp
//
//import de.sciss.lucre.{event => evt}
//import stm.Sys
//import evt.Event
//import collection.immutable.{IndexedSeq => IIdxSeq}
//import expr.{Expr => Ex}
//
//object BiPinLike {
//   type Region[ Elem ] = (SpanLike, Elem)
//
//   type TimedElem[ S <: Sys[ S ], Elem ] = (Ex[ S, Long ], Elem)
//   type Leaf[      S <: Sys[ S ], Elem ] = /* (Long, */ IIdxSeq[ TimedElem[ S, Elem ]] /* ) */
//
//   trait Companion {
//      trait Update[ S <: Sys[ S ], Elem, U, Repr ] {
//         def source: Repr
//      }
//      trait Collection[ S <: Sys[ S ], Elem, U, Repr ] extends Update[ S, Elem, U, Repr ] {
//         def changes: IIdxSeq[ Region[ Elem ]]
//      }
//      trait Element[ S <: Sys[ S ], Elem, U, Repr ] extends Update[ S, Elem, U, Repr ] {
//         def changes: IIdxSeq[ (Elem, U) ]
//      }
//
//      trait Modifiable[ S <: Sys[ S ], Elem ] /* extends BiPinLike[ S, Elem, U ] */ {
//         def add(    time: Ex[ S, Long ], elem: Elem )( implicit tx: S#Tx ) : Unit
//         def remove( time: Ex[ S, Long ], elem: Elem )( implicit tx: S#Tx ) : Boolean
//         def clear()( implicit tx: S#Tx ) : Unit
//      }
//   }
//
////   trait Modifiable[ S <: Sys[ S ], Elem, U ] extends BiPinLike[ S, Elem, U ] {
////      def add(    time: Ex[ S, Long ], elem: Elem )( implicit tx: S#Tx ) : Unit
////      def remove( time: Ex[ S, Long ], elem: Elem )( implicit tx: S#Tx ) : Boolean
////      def clear()( implicit tx: S#Tx ) : Unit
////   }
//}
//trait BiPinLike[ S <: Sys[ S ], Elem, U, Repr, Companion <: BiPinLike.Companion ] extends evt.Node[ S ] {
//   import BiPinLike.Leaf
//
////   def modifiableOption : Option[ BiPinLike.Modifiable[ S, Elem, U ]]
//   def modifiableOption : Option[ Companion#Modifiable[ S, Elem ]]
//
//   /**
//    * Queries the element valid for the given point in time.
//    * Unlike, `intersect`, if there are multiple elements sharing
//    * the same point in time, this returns the most recently added element.
//    *
//    * We propose that this should be the unambiguous way to evaluate
//    * the `BiPin` for a given moment in time.
//    *
//    * @param time the query time point
//    * @return  an element for the given time point, if it exists, otherwise `None`
//    */
//   def at( time: Long )( implicit tx: S#Tx ) : Option[ Elem ]
//
//   def floor( time: Long )( implicit tx: S#Tx ) : Option[ (Long, Elem) ]
//   def ceil(  time: Long )( implicit tx: S#Tx ) : Option[ (Long, Elem) ]
//
//   /**
//    * Queries all elements which are found at a given point in time.
//    * There may be multiple time expressions which are not equal but
//    * evaluate to the same moment in time. It is thus possible that
//    * for a given point, multiple elements are found.
//    *
//    * @param time the query point
//    * @return  the sequence of elements found along with their time expressions
//    */
//   def intersect( time: Long )( implicit tx: S#Tx ) : Leaf[ S, Elem ]
//
//   def collectionChanged:  Event[ S, Companion#Collection[ S, Elem, U, Repr ], Repr ]
//   def elementChanged:     Event[ S, Companion#Element[    S, Elem, U, Repr ], Repr ]
//   def changed :           Event[ S, Companion#Update[     S, Elem, U, Repr ], Repr ]
//
//   def nearestEventAfter( time: Long )( implicit tx: S#Tx ) : Option[ Long ]
//}
