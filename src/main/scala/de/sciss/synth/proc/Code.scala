/*
 *  Code.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.{stm, DataOutput, DataInput, event => evt}
import stm.Serializer
import annotation.switch

object Code {
   implicit def serializer[ S <: evt.Sys[ S ], A ]( implicit peerSer: Serializer[ S#Tx, S#Acc, A ]) : Serializer[ S#Tx, S#Acc, Code[ A ]] =
      new Ser[ S, A ]( peerSer )

   implicit def withoutSource[ A ]( value: A ) : Code[ A ] = Code( value, None )

   private final class Ser[ S <: evt.Sys[ S ], A ]( peerSer: Serializer[ S#Tx, S#Acc, A ])
   extends Serializer[ S#Tx, S#Acc, Code[ A ]] {
      def write( code: Code[ A ], out: DataOutput ) {
//         code.write( out )
         peerSer.write( code.value, out )
         code.source match {
            case Some( text ) => out.writeUnsignedByte( 1 ); out.writeString( text )
            case _            => out.writeUnsignedByte( 0 )
         }
      }

      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Code[ A ] = {
         val value   = peerSer.read( in, access )
         val source  = (in.readUnsignedByte(): @switch) match {
            case 1 => Some( in.readString() )
            case 0 => None
         }
         Code( value, source )
      }
   }
}
final case class Code[ A ]( value: A, source: Option[ String ]) {
   override def toString = "Code(" + value + ", source? " + source.isDefined + ")"

//   def write( out: DataOutput ) {
//
//   }
}
