package de.sciss.synth.expr

import de.sciss.lucre.stm.{TxnSerializer, Sys}
import de.sciss.lucre.{DataInput, event, DataOutput}

object Strings extends Type[ String ] {
   protected def readValue( in: DataInput ) : String = in.readString()
   protected def writeValue( value: String, out: DataOutput ) { out.writeString( value )}

   final class Ops[ S <: Sys[ S ]]( ex: Ex[ S ]) {

   }

//   protected implicit def serializer[ S <: Sys[ S ]] : TxnSerializer[ S#Tx, S#Acc, String ] = sys.error( "TODO" )

   // ---- private ----
}
