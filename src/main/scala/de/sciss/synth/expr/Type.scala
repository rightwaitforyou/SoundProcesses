/*
 *  Type.scala
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

package de.sciss.synth.expr

import de.sciss.lucre.expr.Expr
import de.sciss.lucre.{DataInput, DataOutput, event}
import event.{EventLikeSerializer, Targets, Observer}
import de.sciss.lucre.stm.{InMemory, Sys}

trait Type[ A ] {
   final protected type Ex[ S <: Sys[ S ]] = Expr[ S, A ]
   final protected type Change[ S <: Sys[ S ]] = event.Change[ A ]

   // ---- abstract ----

   protected def readValue( in: DataInput ) : A
   protected def writeValue( value: A, out: DataOutput ) : Unit
//   protected implicit def serializer[ S <: Sys[ S ]] : TxnSerializer[ S#Tx, S#Acc, A ]

   // ---- public ----

   final def newConst[ S <: Sys[ S ]]( value: A ) : Ex[ S ] = new Const( value )

   final def newVar[ S <: Sys[ S ]]( init: Ex[ S ]) : Expr.Var[ S, A ] = sys.error( "TODO" )

   implicit def serializer[ S <: Sys[ S ]] : EventLikeSerializer[ S, Ex[ S ]] = anySer.asInstanceOf[ Ser[ S ]] // new Ser[ S ]

   private val anySer = new Ser[ InMemory ]

   private final class Ser[ S <: Sys[ S ]] extends EventLikeSerializer[ S, Ex[ S ]] {
      def read( in: DataInput, access: S#Acc, targets: Targets[ S ])( implicit tx: S#Tx ) : Ex[ S ] = {
         // 0 = var, 1 = op
         (in.readUnsignedByte() /*: @switch */) match {
            case 0      => sys.error( "TODO" ) // new VarRead( in, access, targets, tx )
            case arity  => sys.error( "TODO" )
//               val clazz   = in.readInt()
//               val opID    = in.readInt()
//               if( clazz == tpe.id ) {
//                  readTuple( arity, opID, in, access, targets )
//               } else {
//                  getExtension( clazz )( tx.peer ).readTuple( arity, opID, in, access, targets )
//               }
         }
      }

      def readConstant( in: DataInput )( implicit tx: S#Tx ) : Ex[ S ] = newConst( readValue( in ))
   }

   // ---- private ----

   private final case class Const[ S <: Sys[ S ]]( constValue: A ) extends Expr.Const[ S, A ] {
      def react( fun: S#Tx => Change[ S ] => Unit )( implicit tx: S#Tx ) : Observer[ S, Change[ S ], Ex[ S ]] = {
         Observer( serializer[ S ], fun )
      }

      protected def writeData( out: DataOutput ) {
         writeValue( constValue, out )
      }
   }

   private final class Var[ S <: Sys[ S ]]( protected val ref: S#Var[ Ex[ S ]], protected val targets: Targets[ S ])
   extends Expr.Var[ S, A ] {
      def reader : event.Reader[ S, Ex[ S ]] = serializer[ S ]
   }
}