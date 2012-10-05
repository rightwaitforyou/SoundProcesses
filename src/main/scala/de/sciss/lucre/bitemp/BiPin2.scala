/*
 *  BiPin2.scala
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
package bitemp

//import impl.{BiPin2Impl => Impl}
import de.sciss.lucre.{event => evt}
import collection.immutable.{IndexedSeq => IIdxSeq}
import evt.{EventLike, Sys}
import expr.Expr

object BiPin2 {
   def ??? : Nothing = sys.error( "TODO" )

   sealed trait Update[ S <: Sys[ S ], A ] {
      def pin: BiPin2[ S, A ]
   }
   sealed trait Collection[ S <: Sys[ S ], A ] extends Update[ S, A ] {
      def value: (Long, A)
      def elem: BiExpr[ S, A ]
   }
   final case class Added[   S <: Sys[ S ], A ]( pin: BiPin2[ S, A ], value: (Long, A), elem: BiExpr[ S, A ]) extends Collection[ S, A ]
   final case class Removed[ S <: Sys[ S ], A ]( pin: BiPin2[ S, A ], value: (Long, A), elem: BiExpr[ S, A ]) extends Collection[ S, A ]

   final case class Element[ S <: Sys[ S ], A ]( pin: BiPin2[ S, A ], changes: IIdxSeq[ (BiExpr[ S, A ], evt.Change[ (Long, A) ])])
   extends Update[ S, A ]

   type Leaf[ S <: Sys[ S ], A ] = IIdxSeq[ BiExpr[ S, A ]]

   object Modifiable {
      /**
       * Extractor to check if a `BiPin2` is actually a `BiPin2.Modifiable`
       */
      def unapply[ S <: Sys[ S ], A ]( v: BiPin2[ S, A ]) : Option[ Modifiable[ S, A ]] = {
         if( v.isInstanceOf[ Modifiable[ _, _ ]]) Some( v.asInstanceOf[ Modifiable[ S, A ]]) else None
      }

      def read[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc )
                                  ( implicit tx: S#Tx, biType: BiType[ A ]) : Modifiable[ S, A ] = {
         ??? // Impl.readModifiable[ S, Elem, U ]( in, access, eventView )
      }

      def apply[ S <: Sys[ S ], A ]( implicit tx: S#Tx, biType: BiType[ A ]) : Modifiable[ S, A ] =
         ??? // Impl.newModifiable[ S, Elem, U ]( /* default, */ eventView )

      def serializer[ S <: Sys[ S ], A ]( implicit biType: BiType[ A ]) : stm.Serializer[ S#Tx, S#Acc, BiPin2.Modifiable[ S, A ]] =
         ??? // Impl.modifiableSerializer[ S, Elem, U ]( eventView )
   }
   trait Modifiable[ S <: Sys[ S ], A ] extends BiPin2[ S, A ] {
      def add(    elem: BiExpr[ S, A ])( implicit tx: S#Tx ) : Unit
      def remove( elem: BiExpr[ S, A ])( implicit tx: S#Tx ) : Boolean
      def clear()( implicit tx: S#Tx ) : Unit
   }

   def read[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc )
                               ( implicit tx: S#Tx, biType: BiType[ A ]) : BiPin2[ S, A ] = {
      ??? // Impl.read[ S, Elem, U ]( in, access, eventView )
   }

   def serializer[ S <: Sys[ S ], A ]( implicit biType: BiType[ A ]) : stm.Serializer[ S#Tx, S#Acc, BiPin2[ S, A ]] =
      ??? // Impl.serializer[ S, Elem, U ]( eventView )
}
sealed trait BiPin2[ S <: Sys[ S ], A ] extends Writable {
   import BiPin2.Leaf

   final protected type Elem = BiExpr[ S, A ]

//   def value( implicit tx: S#Tx, time: Chronos[ S ]) : A

   def modifiableOption : Option[ BiPin2.Modifiable[ S, A ]]

   /**
    * Queries the element valid for the given point in time.
    * Unlike, `intersect`, if there are multiple elements sharing
    * the same point in time, this returns the most recently added element.
    *
    * We propose that this should be the unambiguous way to evaluate
    * the `BiPin2` for a given moment in time.
    *
    * @param time the query time point
    * @return  an element for the given time point, if it exists, otherwise `None`
    */
   def at( time: Long )( implicit tx: S#Tx ) : Option[ Elem ]

   def valueAt( time: Long )( implicit tx: S#Tx ) : Option[ A ]

   /**
    * Finds the entry at the given time, or the closest entry before the given time.
    *
    * @param time the query time
    * @return     the entry nearest in time to the query time, but not later than the
    *             query time, or `None` if there is no entry at such time
    */
   def floor( time: Long )( implicit tx: S#Tx ) : Option[ Elem ]

   /**
    * Finds the entry at the given time, or the closest entry after the given time.
    *
    * @param time the query time
    * @return     the entry nearest in time to the query time, but not earlier than the
    *             query time, or `None` if there is no entry at such time
    */
   def ceil( time: Long )( implicit tx: S#Tx ) : Option[ Elem ]

   /**
    * Queries all elements which are found at a given point in time.
    * There may be multiple time expressions which are not equal but
    * evaluate to the same moment in time. It is thus possible that
    * for a given point, multiple elements are found.
    *
    * @param time the query point
    * @return  the sequence of elements found along with their time expressions
    */
   def intersect( time: Long )( implicit tx: S#Tx ) : Option[ (Long, Leaf[ S, A ])]

//   def projection( implicit tx: S#Tx, time: Chronos[ S ]) : Expr[ S, A ]

//   def collectionChanged:  EventLike[ S, BiPin2.Collection[ S, A ], BiPin2[ S, A ]]
//   def elementChanged:     EventLike[ S, BiPin2.Element[    S, A ], BiPin2[ S, A ]]
   def changed :           EventLike[ S, BiPin2.Update[     S, A ], BiPin2[ S, A ]]

   /**
    * Finds the entry with the smallest time which is greater than or equal to the query time.
    *
    * @param time the query time
    * @return     the time corresponding to the next entry, or `None` if there is no entry
    *             at or later than the given time
    */
   def nearestEventAfter( time: Long )( implicit tx: S#Tx ) : Option[ Long ]

   def debugList()( implicit tx: S#Tx ) : List[ (Long, A) ]
}