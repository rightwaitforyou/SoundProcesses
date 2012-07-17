/*
 *  LinkedList.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2012 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr

import de.sciss.lucre.{event => evt}
import evt.{Event, EventLike}
import de.sciss.lucre.stm.{TxnSerializer, Sys}
import de.sciss.collection.txn
import impl.{LinkedListImpl => Impl}
import collection.immutable.{IndexedSeq => IIdxSeq}

object LinkedList {
   import de.sciss.lucre.expr.{Expr => Ex}

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

   final case class Element[ S <: Sys[ S ], Elem, U ]( list: LinkedList[ S, Elem, U ], updates: IIdxSeq[ (Elem, U) ])
   extends Update[ S, Elem, U ]

   trait Var[ S <: Sys[ S ], Elem, U ] extends LinkedList[ S, Elem, U ] {
      def addLast( elem: Elem )( implicit tx: S#Tx ) : Unit
      def addHead( elem: Elem )( implicit tx: S#Tx ) : Unit
      def remove( elem: Elem )( implicit tx: S#Tx ) : Boolean
      def removeLast()( implicit tx: S#Tx ) : Unit
      def removeHead()( implicit tx: S#Tx ) : Unit
      def clear()( implicit tx: S#Tx ) : Unit
   }

   type Expr[    S <: Sys[ S ], A ] = LinkedList[ S, Ex[ S, A ], evt.Change[ A ]]
   type ExprVar[ S <: Sys[ S ], A ] = Var[        S, Ex[ S, A ], evt.Change[ A ]]

   def newVar[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
                                       ( implicit tx: S#Tx, elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ]) : Var[ S, Elem, U ] =
      Impl.newVar( eventView )

   def newExprVar[ S <: Sys[ S ], A ]( implicit tx: S#Tx, peerType: Type[ A ]) : ExprVar[ S, A ] =
      newVar[ S, Ex[ S, A ], evt.Change[ A ]]( _.changed )( tx, peerType.serializer[ S ])

   def exprSerializer[ S <: Sys[ S ], A ]( implicit elemType: Type[ A ]) : TxnSerializer[ S#Tx, S#Acc, Expr[ S, A ]] =
      Impl.serializer[ S, Ex[ S, A ], evt.Change[ A ]]( _.changed )( elemType.serializer[ S ])

   def serializer[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])(
      implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ]) : TxnSerializer[ S#Tx, S#Acc, LinkedList[ S, Elem, U ]] =
      Impl.serializer[ S, Elem, U ]( eventView )

   def exprVarSerializer[ S <: Sys[ S ], A ]( implicit elemType: Type[ A ]) : TxnSerializer[ S#Tx, S#Acc, ExprVar[ S, A ]] =
      Impl.varSerializer[ S, Ex[ S, A ], evt.Change[ A ]]( _.changed )( elemType.serializer[ S ])

   def varSerializer[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])(
      implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ]) : TxnSerializer[ S#Tx, S#Acc, Var[ S, Elem, U ]] =
      Impl.varSerializer[ S, Elem, U ]( eventView )
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
