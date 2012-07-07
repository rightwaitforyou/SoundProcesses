/*
 *  BiPin.scala
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

import impl.BiPinImpl
import de.sciss.lucre.{event => evt, DataInput}
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.lucre.stm.{TxnSerializer, Sys}
import de.sciss.collection.txn
import evt.{Event, EventLike}

object BiPin {
   type ExprUpdate[ S <: Sys[ S ], A ] = Update[ S, Expr[ S, A ], evt.Change[ A ]]

   sealed trait Update[ S <: Sys[ S ], Elem, U ] {
      def pin: BiPin[ S, Elem, U ]
   }
   final case class Collection[ S <: Sys[ S ], Elem, U ]( pin: BiPin[ S, Elem, U ], changes: IIdxSeq[ Region[ Elem ]]) extends Update[ S, Elem, U ]
   final case class Element[ S <: Sys[ S ], Elem, U ]( pin: BiPin[ S, Elem, U ], changes: IIdxSeq[ (Elem, U) ]) extends Update[ S, Elem, U ]

   type Region[ Elem ] = (SpanLike, Elem)

   type TimedElem[ S <: Sys[ S ], Elem ] = (Expr[ S, Long ], Elem)
   type Leaf[      S <: Sys[ S ], Elem ] = /* (Long, */ IIdxSeq[ TimedElem[ S, Elem ]] /* ) */

   type ExprVar[ S <: Sys[ S ], A ] = Var[ S, Expr[ S, A ], evt.Change[ A ]]

   trait Var[ S <: Sys[ S ], Elem, U ] extends BiPin[ S, Elem, U ] {
//      def get( implicit tx: S#Tx, time: Chronos[ S ]) : Expr[ S, A ]
//      def getAt( time: Long )( implicit tx: S#Tx ) : Expr[ S, A ]
//      def set( value: Expr[ S, A ])( implicit tx: S#Tx ) : Unit
//      def add( time: Expr[ S, Long ], value: Expr[ S, A ])( implicit tx: S#Tx ) : Option[ Expr[ S, A ]]
//      def remove( time: Expr[ S, Long ])( implicit tx: S#Tx ) : Boolean
//      def removeAt( time: Long )( implicit tx: S#Tx ) : Option[ Expr[ S, Long ]]
//      def removeAll( span: SpanLike )( implicit tx: S#Tx ) : Unit

      def add(    time: Expr[ S, Long ], elem: Elem )( implicit tx: S#Tx ) : Unit
      def remove( time: Expr[ S, Long ], elem: Elem )( implicit tx: S#Tx ) : Boolean
      def clear()( implicit tx: S#Tx ) : Unit
   }

//   def newVar[ S <: Sys[ S ], A ]( implicit tx: S#Tx, elemType: BiType[ A ]) : Var[ S, Expr[ S, A ], evt.Change[ A ]] =
//      BiGroupImpl.newGenericVar[ S, Expr[ S, A ], evt.Change[ A ]]( _.changed )( tx, elemType.serializer[ S ], elemType.spanLikeType )
//
//   def newGenericVar[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
//      ( implicit tx: S#Tx, elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
//        spanType: Type[ SpanLike ]) : Var[ S, Elem, U ] = BiGroupImpl.newGenericVar( eventView )
//
//   def readGenericVar[ S <: Sys[ S ], Elem, U ]( in: DataInput, access: S#Acc, eventView: Elem => EventLike[ S, U, Elem ])
//         ( implicit tx: S#Tx, elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
//           spanType: Type[ SpanLike ]) : Var[ S, Elem, U ] = BiGroupImpl.readGenericVar( in, access, eventView )

//   def newVar[ S <: Sys[ S ], A ]( init: Expr[ S, A ])
//                                 ( implicit tx: S#Tx, peerType: BiType[ A ]) : Var[ S, Expr[ S, A ], evt.Change[ A ]] =
//      BiPinImpl.newVar( init )

   def newVar[ S <: Sys[ S ], A ]( implicit tx: S#Tx, elemType: BiType[ A ]) : Var[ S, Expr[ S, A ], evt.Change[ A ]] =
      BiPinImpl.newGenericVar[ S, Expr[ S, A ], evt.Change[ A ]]( _.changed )( tx, elemType.serializer[ S ], elemType.longType )

      def newConfluentVar[ S <: Sys[ S ], A ]( init: Expr[ S, A ])
                                          ( implicit tx: S#Tx,
                                            peerType: BiType[ A ]) : Var[ S, Expr[ S, A ], evt.Change[ A ]] =
      sys.error( "TODO" ) // BiPinImpl.newConfluentVar( init )

   def readVar[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc )
                                  ( implicit tx: S#Tx, peerType: BiType[ A ]) : Var[ S, Expr[ S, A ], evt.Change[ A ]] =
      BiPinImpl.readVar( in, access )

   def serializer[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
                                           ( implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                             timeType: Type[ Long ]) : TxnSerializer[ S#Tx, S#Acc, BiPin[ S, Elem, U ]] =
      BiPinImpl.serializer[ S, Elem, U ]( eventView )

   def varSerializer[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
                                              ( implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                                timeType: Type[ Long ]) : TxnSerializer[ S#Tx, S#Acc, BiPin.Var[ S, Elem, U ]] =
      BiPinImpl.varSerializer[ S, Elem, U ]( eventView )

//   implicit def serializer[ S <: Sys[ S ], A ]( implicit peerType: BiType[ A ]) :
//      evt.Reader[ S, BiPin[ S, A ]] with TxnSerializer[ S#Tx, S#Acc, BiPin[ S, A ]] = BiPinImpl.serializer[ S, A ]
//
//   implicit def varSerializer[ S <: Sys[ S ], A ]( implicit peerType: BiType[ A ]) :
//      evt.Reader[ S, Var[ S, A ]] with TxnSerializer[ S#Tx, S#Acc, Var[ S, A ]] = BiPinImpl.varSerializer[ S, A ]
}
sealed trait BiPin[ S <: Sys[ S ], Elem, U ] extends evt.Node[ S ] {
   import BiPin.Leaf

//   def value( implicit tx: S#Tx, time: Chronos[ S ]) : A
   def at( time: Long )( implicit tx: S#Tx ) : Elem
   def intersect( time: Long )( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, Leaf[ S, Elem ]]
//   def projection( implicit tx: S#Tx, time: Chronos[ S ]) : Expr[ S, A ]

   def collectionChanged:  Event[ S, BiPin.Collection[ S, Elem, U ], BiPin[ S, Elem, U ]]
   def elementChanged:     Event[ S, BiPin.Element[    S, Elem, U ], BiPin[ S, Elem, U ]]
   def changed :           Event[ S, BiPin.Update[     S, Elem, U ], BiPin[ S, Elem, U ]]

   def debugList()( implicit tx: S#Tx ) : List[ (Long, Elem) ]
}
