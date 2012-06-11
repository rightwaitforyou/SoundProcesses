/*
 *  BiGroup.scala
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
import de.sciss.collection.txn
import de.sciss.lucre.stm.{TxnSerializer, Sys}
import impl.BiGroupImpl

object BiGroup {
   sealed trait Update[ S <: Sys[ S ], Elem, U ] {
      def group: BiGroup[ S, Elem, U ]
      def elem: Elem
   }
   final case class Added[   S <: Sys[ S ], Elem, U ]( group: BiGroup[ S, Elem, U ], elem: Elem ) extends Update[ S, Elem, U ]
   final case class Removed[ S <: Sys[ S ], Elem, U ]( group: BiGroup[ S, Elem, U ], elem: Elem ) extends Update[ S, Elem, U ]
   final case class Moved[   S <: Sys[ S ], Elem, U ]( group: BiGroup[ S, Elem, U ], elem: Elem ) extends Update[ S, Elem, U ]
   final case class Element[ S <: Sys[ S ], Elem, U ]( group: BiGroup[ S, Elem, U ], elem: Elem, elemUpdate: U ) extends Update[ S, Elem, U ]

   def newVar[ S <: Sys[ S ], A ]( implicit tx: S#Tx, elemType: BiType[ A ]) : Var[ S, Expr[ S, A ], evt.Change[ A ]] =
      BiGroupImpl.newGenericVar[ S, Expr[ S, A ], evt.Change[ A ]]( _.changed )( tx, elemType.serializer[ S ], elemType.spanLikeType )

   def newGenericVar[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
      ( implicit tx: S#Tx, elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ],
        spanType: Type[ SpanLike ]) : Var[ S, Elem, U ] = BiGroupImpl.newGenericVar( eventView )

   trait Var[ S <: Sys[ S ], Elem, U ] extends BiGroup[ S, Elem, U ] {
      def add( span: Expr[ S, SpanLike ], elem: Elem )( implicit tx: S#Tx ) : Unit
      def remove( span: Expr[ S, SpanLike ], elem: Elem )( implicit tx: S#Tx ) : Boolean
   }
}
trait BiGroup[ S <: Sys[ S ], Elem, U ] {
   def iterator( implicit tx: S#Tx, time: Chronos[ S ]) : txn.Iterator[ S#Tx, (SpanLike, Elem) ]
   def iteratorAt( time: Long )( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, (SpanLike, Elem) ]
   def iteratorWithin( span: SpanLike )( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, (SpanLike, Elem) ]

//   def projection( implicit tx: S#Tx, time: Chronos[ S ]) : Expr[ S, A ]

   def changed : Event[ S, BiGroup.Update[ S, Elem, U ], BiGroup[ S, Elem, U ]]

   def debugList()( implicit tx: S#Tx ) : List[ (SpanLike, Elem) ]
}