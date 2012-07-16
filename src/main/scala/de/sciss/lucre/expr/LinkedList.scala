package de.sciss.lucre.expr

import de.sciss.lucre.{event => evt}
import evt.{Event, EventLike}
import de.sciss.lucre.stm.{TxnSerializer, Sys}
import de.sciss.collection.txn
import impl.LinkedListImpl

object LinkedList {
   sealed trait Update[ S <: Sys[ S ], Elem, U ] {
      def list: LinkedList[ S, Elem, U ]
   }
   sealed trait Collection[ S <: Sys[ S ], Elem, U ] extends Update[ S, Elem, U ] {
      def index: Int
      def elem: Elem
   }
   final case class Added[ S <: Sys[ S ], Elem, U ]( list: LinkedList[ S, Elem, U ], index: Int, elem: Elem )
   extends Collection[ S, Elem, U ]

   final case class Removed[ S <: Sys[ S ], Elem, U ]( list: LinkedList[ S, Elem, U ], index: Int, elem: Elem )
   extends Collection[ S, Elem, U ]

   final case class Element[ S <: Sys[ S ], Elem, U ]( list: LinkedList[ S, Elem, U ], elem: Elem, update: U )
   extends Update[ S, Elem, U ]

   trait Var[ S <: Sys[ S ], Elem, U ] extends LinkedList[ S, Elem, U ] {
      def addLast( elem: Elem )( implicit tx: S#Tx ) : Unit
      def addHead( elem: Elem )( implicit tx: S#Tx ) : Unit
      def remove( elem: Elem )( implicit tx: S#Tx ) : Boolean
      def removeLast()( implicit tx: S#Tx ) : Unit
      def removeHead()( implicit tx: S#Tx ) : Unit
      def clear()( implicit tx: S#Tx ) : Unit
   }

   def newVar[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
                                       ( implicit tx: S#Tx, elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ]) : Var[ S, Elem, U ] =
      LinkedListImpl.newVar( eventView )
}
trait LinkedList[ S <: Sys[ S ], Elem, U ] extends evt.Node[ S ] {
   def isEmpty( implicit tx: S#Tx ) : Boolean
   def nonEmpty( implicit tx: S#Tx ) : Boolean
   def size( implicit tx: S#Tx ) : Int
   
   def headOption( implicit tx: S#Tx ) : Option[ Elem ]
   def lastOption( implicit tx: S#Tx ) : Option[ Elem ]
   def head( implicit tx: S#Tx ) : Elem
   def last( implicit tx: S#Tx ) : Elem
   def iterator( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, Elem ]
   
   def collectionChanged:  Event[ S, LinkedList.Collection[ S, Elem, U ], LinkedList[ S, Elem, U ]]
   def elementChanged:     Event[ S, LinkedList.Element[    S, Elem, U ], LinkedList[ S, Elem, U ]]
   def changed:            Event[ S, LinkedList.Update[     S, Elem, U ], LinkedList[ S, Elem, U ]]

   def debugList()( implicit tx: S#Tx ) : List[ Elem ]
}
