package de.sciss.synth.proc

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.expr.Expr
import de.sciss.synth.Env
import de.sciss.lucre.bitemp.{BiType, BiPin}
import de.sciss.synth.expr.BiTypeImpl
import de.sciss.lucre.{DataOutput, DataInput}
import de.sciss.lucre.event.Targets

object Scan_ {
   sealed trait Elem[ S <: Sys[ S ]]
   final case class Mono[ S <: Sys[ S ]]( targetLevel: Expr[ S, Double ], shape: Env.Shape ) extends Elem[ S ]
//   final case class AudioFile[ S <: Sys[ S ]]( f: File, offset: Expr[ S, Long ]) extends Elem[ S ]
//   final case class Graph[ S <: Sys[ S ]]( func: Expr[ S, SynthGraph ]) extends Elem[ S ]
   final case class Synthesis[ S <: Sys[ S ]]() extends Elem[ S ]
   final case class Embedded[ S <: Sys[ S ]]( ref: Scan[ S ], offset: Expr[ S, Long ]) extends Elem[ S ]

//   type Modifiable[ S <: Sys[ S ]] = BiPin.Expr.Modifiable[ S, Elem[ S ]]
   type Modifiable[ S <: Sys[ S ]] = BiPin.Modifiable[ S, Elem[ S ], Unit ]

   object Modifiable {
      /**
       * Extractor to check if a `Scan` is actually a `Scan.Modifiable`
       */
      def unapply[ S <: Sys[ S ], Elem, U ]( v: Scan[ S ]) : Option[ Modifiable[ S ]] = {
//         if( v.isInstanceOf[ Modifiable[ _ ]]) Some( v.asInstanceOf[ Modifiable[ S ]]) else None
         if( v.isInstanceOf[ BiPin.Modifiable[ _ , _ , _ ]]) Some( v.asInstanceOf[ Modifiable[ S ]]) else None
      }
   }

//   def Elems[ S <: Sys[ S ]] : BiType[ Elem[ S ]] = anyElems.asInstanceOf[ BiType[ Elem[ S ]]]
//
//   private val anyElems = new ElemsImpl[ I ]
//
//   private final class ElemsImpl[ S <: Sys[ S ]] extends BiTypeImpl[ Elem[ S ]] {
//      private val typeID = 1000
//
//      /* protected */ def readValue( in: DataInput ) : Elem[ S ] = ??? // SpanLike.read( in )
//      /* protected */ def writeValue( value: Elem[ S ], out: DataOutput ) { ??? } // value.write( out )}
//
//      def readTuple[ S1 <: Sys[ S1 ]]( cookie: Int, in: DataInput, access: S1#Acc, targets: Targets[ S1 ])( implicit tx: S1#Tx ) : Ex[ S1 ] =
//         (cookie /*: @switch */) match {
//            case _ => sys.error( "Invalid cookie " + cookie )
//         }
//   }
}