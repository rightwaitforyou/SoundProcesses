package de.sciss.synth.proc

import de.sciss.lucre.stm.{Serializer, Sys}
import de.sciss.lucre.{Writable, DataOutput, DataInput}
import annotation.switch

object Code {
   implicit def serializer[ S <: Sys[ S ], A ]( implicit peerSer: Serializer[ S#Tx, S#Acc, A ]) : Serializer[ S#Tx, S#Acc, Code[ A ]] =
      new Ser[ S, A ]( peerSer )

   private final class Ser[ S <: Sys[ S ], A ]( peerSer: Serializer[ S#Tx, S#Acc, A ])
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
