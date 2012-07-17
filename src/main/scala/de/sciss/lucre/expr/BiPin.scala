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

package de.sciss.lucre
package expr

import impl.BiPinImpl
import de.sciss.lucre.{event => evt, DataInput}
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.lucre.stm.{TxnSerializer, Sys}
import evt.{Event, EventLike}

object BiPin {
   import expr.{Expr => Ex}

   object Expr {
      type Update[ S <: Sys[ S ], A ] = BiPin.Update[ S, Ex[ S, A ], evt.Change[ A ]]
      type Modifiable[ S <: Sys[ S ], A ] = BiPin.Modifiable[ S, Ex[ S, A ], evt.Change[ A ]]

      def read[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc )( implicit tx: S#Tx, elemType: BiType[ A ]) : Expr[ S, A ] = {
         BiPinImpl.read[ S, Ex[ S, A ], evt.Change[ A ]]( in, access, _.changed )( tx, elemType.serializer[ S ], elemType.longType )
      }

      def serializer[ S <: Sys[ S ], A ]( implicit elemType: BiType[ A ]) : TxnSerializer[ S#Tx, S#Acc, Expr[ S, A ]] = {
         import elemType.serializer
         implicit val timeType = elemType.longType
         BiPinImpl.serializer[ S, Ex[ S, A ], evt.Change[ A ]]( _.changed )
      }

      object Modifiable {
         def serializer[ S <: Sys[ S ], A ]( implicit elemType: BiType[ A ]) : TxnSerializer[ S#Tx, S#Acc, BiPin.Expr.Modifiable[ S, A ]] = {
            import elemType.serializer
            implicit val timeType = elemType.longType
            BiPinImpl.modifiableSerializer[ S, Ex[ S, A ], evt.Change[ A ]]( _.changed )
         }

         def read[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc )( implicit tx: S#Tx, elemType: BiType[ A ]) : Expr.Modifiable[ S, A ] = {
            BiPinImpl.readModifiable[ S, Ex[ S, A ], evt.Change[ A ]]( in, access, _.changed )( tx, elemType.serializer[ S ], elemType.longType )
         }

         def apply[ S <: Sys[ S ], A ]( default: Ex[ S, A ])( implicit tx: S#Tx, elemType: BiType[ A ]) : Expr.Modifiable[ S, A ] =
            BiPinImpl.newModifiable[ S, Ex[ S, A ], evt.Change[ A ]]( default, _.changed )( tx, elemType.serializer[ S ], elemType.longType )

         def partial[ S <: Sys[ S ], A ]( default: Ex[ S, A ])
                                        ( implicit tx: S#Tx, elemType: BiType[ A ]) : Expr.Modifiable[ S, A ] =
            BiPinImpl.newPartialModifiable[ S, Ex[ S, A ], evt.Change[ A ]]( default, _.changed )( tx, elemType.serializer[ S ], elemType.longType )
      }
   }
   type Expr[ S <: Sys[ S ], A ]    = BiPin[ S, Ex[ S, A ], evt.Change[ A ]]

   sealed trait Update[ S <: Sys[ S ], Elem, U ] {
      def pin: BiPin[ S, Elem, U ]
   }
   final case class Collection[ S <: Sys[ S ], Elem, U ]( pin: BiPin[ S, Elem, U ], changes: IIdxSeq[ Region[ Elem ]]) extends Update[ S, Elem, U ]
   final case class Element[ S <: Sys[ S ], Elem, U ]( pin: BiPin[ S, Elem, U ], changes: IIdxSeq[ (Elem, U) ]) extends Update[ S, Elem, U ]

   type Region[ Elem ] = (SpanLike, Elem)

   type TimedElem[ S <: Sys[ S ], Elem ] = (Ex[ S, Long ], Elem)
   type Leaf[      S <: Sys[ S ], Elem ] = /* (Long, */ IIdxSeq[ TimedElem[ S, Elem ]] /* ) */

   object Modifiable {
      /**
       * Extractor to check if a `BiPin` is actually a `BiPin.Modifiable`
       */
      def unapply[ S <: Sys[ S ], Elem, U ]( v: BiPin[ S, Elem, U ]) : Option[ Modifiable[ S, Elem, U ]] = {
         if( v.isInstanceOf[ Modifiable[ _, _, _ ]]) Some( v.asInstanceOf[ Modifiable[ S, Elem, U ]]) else None
      }

      def read[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])( in: DataInput, access: S#Acc )
                                        ( implicit tx: S#Tx,
                                          elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                          timeType: Type[ Long ]) : Modifiable[ S, Elem, U ] = {
         BiPinImpl.readModifiable[ S, Elem, U ]( in, access, eventView )
      }

      def apply[ S <: Sys[ S ], Elem, U ]( default: Elem )( eventView: Elem => EventLike[ S, U, Elem ])
                                         ( implicit tx: S#Tx,
                                           elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                           timeType: Type[ Long ]) : Modifiable[ S, Elem, U ] =
         BiPinImpl.newModifiable[ S, Elem, U ]( default, eventView )

      def serializer[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
                                              ( implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                                timeType: Type[ Long ]) : TxnSerializer[ S#Tx, S#Acc, BiPin.Modifiable[ S, Elem, U ]] =
         BiPinImpl.modifiableSerializer[ S, Elem, U ]( eventView )
   }
   trait Modifiable[ S <: Sys[ S ], Elem, U ] extends BiPin[ S, Elem, U ] {
      def add(    time: Ex[ S, Long ], elem: Elem )( implicit tx: S#Tx ) : Unit
      def remove( time: Ex[ S, Long ], elem: Elem )( implicit tx: S#Tx ) : Boolean
      def clear()( implicit tx: S#Tx ) : Unit
   }

   def read[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])( in: DataInput, access: S#Acc )
                                     ( implicit tx: S#Tx,
                                       elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                       timeType: Type[ Long ]) : BiPin[ S, Elem, U ] = {
      BiPinImpl.read[ S, Elem, U ]( in, access, eventView )
   }

   def serializer[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
                                           ( implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                             timeType: Type[ Long ]) : TxnSerializer[ S#Tx, S#Acc, BiPin[ S, Elem, U ]] =
      BiPinImpl.serializer[ S, Elem, U ]( eventView )
}
sealed trait BiPin[ S <: Sys[ S ], Elem, U ] extends evt.Node[ S ] {
   import BiPin.Leaf

//   def value( implicit tx: S#Tx, time: Chronos[ S ]) : A

   /**
    * Queries the element valid for the given point in time.
    * Unlike, `intersect`, if there are multiple elements sharing
    * the same point in time, this returns the most recently added element.
    *
    * We propose that this should be the unambiguous way to evaluate
    * the `BiPin` for a given moment in time.
    *
    * @param time the query time point
    * @return  an element for the given time point
    */
   def at( time: Long )( implicit tx: S#Tx ) : Elem

   /**
    * Queries all elements which are found at a given point in time.
    * There may be multiple time expressions which are not equal but
    * evaluate to the same moment in time. It is thus possible that
    * for a given point, multiple elements are found.
    *
    * @param time the query point
    * @return  the sequence of elements found along with their time expressions
    */
   def intersect( time: Long )( implicit tx: S#Tx ) : Leaf[ S, Elem ]
//   def projection( implicit tx: S#Tx, time: Chronos[ S ]) : Expr[ S, A ]

   def collectionChanged:  Event[ S, BiPin.Collection[ S, Elem, U ], BiPin[ S, Elem, U ]]
   def elementChanged:     Event[ S, BiPin.Element[    S, Elem, U ], BiPin[ S, Elem, U ]]
   def changed :           Event[ S, BiPin.Update[     S, Elem, U ], BiPin[ S, Elem, U ]]

   def nearestEventAfter( time: Long )( implicit tx: S#Tx ) : Option[ Long ]

   def debugList()( implicit tx: S#Tx ) : List[ (Long, Elem) ]
}