package de.sciss.synth.proc
package impl

import de.sciss.lucre.{DataInput, DataOutput, stm}
import de.sciss.synth.Synth
import concurrent.stm.TxnLocal
import stm.{Durable, Writer}

object AuralProc {
//   implicit object Serializer extends stm.Serializer[ AuralProc ] {
//      def write( v: AuralProc, out: DataOutput ) { v.write( out )}
//      def read( in: DataInput ) : AuralProc = {
//         val name = in.readString()
//         new Impl( name )
//      }
//   }

   def apply( name: String ) : AuralProc = new Impl( name )

   private final class Impl( val name: String ) extends AuralProc {
//      def write( out: DataOutput ) {
//         out.writeString( name )
//      }
   }
}
sealed trait AuralProc /* extends Writer */ {
   def name : String
//   def synth: Durable
}
