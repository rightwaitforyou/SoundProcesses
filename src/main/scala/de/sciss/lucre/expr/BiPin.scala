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
import de.sciss.lucre.stm.{Disposable, TxnSerializer, Writer, Sys}
import evt.Event

object BiPin {
   type Update[ A ] = IIdxSeq[ Region[ A ]]

   final case class Region[ A ]( span: SpanLike, value: A )

   trait Var[ S <: Sys[ S ], A ] extends BiPin[ S, A ] with Disposable[ S#Tx ] /* with Source[ S, A ] with Sink[ S, A ] */ {
      def get( implicit tx: S#Tx, time: Chronos[ S ]) : Expr[ S, A ]
      def getAt( time: Long )( implicit tx: S#Tx ) : Expr[ S, A ]
      def set( value: Expr[ S, A ])( implicit tx: S#Tx ) : Unit
      def add( time: Expr[ S, Long ], value: Expr[ S, A ])( implicit tx: S#Tx ) : Option[ Expr[ S, A ]]
      def remove( time: Expr[ S, Long ])( implicit tx: S#Tx ) : Boolean
      def removeAt( time: Long )( implicit tx: S#Tx ) : Option[ Expr[ S, Long ]]
      def removeAll( span: SpanLike )( implicit tx: S#Tx ) : Unit
   }

   def newVar[ S <: Sys[ S ], A ]( init: Expr[ S, A ])( implicit tx: S#Tx,
                                                        peerType: BiType[ A ]) : Var[ S, A ] =
      BiPinImpl.newVar( init )

   def newConfluentVar[ S <: Sys[ S ], A ]( init: Expr[ S, A ])( implicit tx: S#Tx,
                                                                 peerType: BiType[ A ]) : Var[ S, A ] =
      BiPinImpl.newConfluentVar( init )

   def readVar[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc )
                                  ( implicit tx: S#Tx, peerType: BiType[ A ]) : Var[ S, A ] =
      BiPinImpl.readVar( in, access )

   implicit def serializer[ S <: Sys[ S ], A ]( implicit peerType: BiType[ A ]) :
      evt.Reader[ S, BiPin[ S, A ]] with TxnSerializer[ S#Tx, S#Acc, BiPin[ S, A ]] = BiPinImpl.serializer[ S, A ]

   implicit def varSerializer[ S <: Sys[ S ], A ]( implicit peerType: BiType[ A ]) :
      evt.Reader[ S, Var[ S, A ]] with TxnSerializer[ S#Tx, S#Acc, Var[ S, A ]] = BiPinImpl.varSerializer[ S, A ]
}
sealed trait BiPin[ S <: Sys[ S ], A ] extends /* BiSource[ S#Tx, Chronos[ S ], Expr[ S, A ]] with */ Writer {
   def value( implicit tx: S#Tx, time: Chronos[ S ]) : A
   def valueAt( time: Long )( implicit tx: S#Tx ) : A
//   def projection( implicit tx: S#Tx, time: Chronos[ S ]) : Expr[ S, A ]

   def changed : Event[ S, BiPin.Update[ A ], BiPin[ S, A ]]

   def debugList()( implicit tx: S#Tx ) : List[ (Long, A) ]
}
