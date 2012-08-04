package de.sciss.synth
package proc

import de.sciss.lucre.stm.ImmutableSerializer
import de.sciss.lucre.{DataOutput, DataInput}
import impl.SynthGraphSerializer

object ProcGraph {
   implicit def serializer: ImmutableSerializer[ ProcGraph ] = Ser

   implicit def withoutSource( synthGraph: SynthGraph ) : ProcGraph = apply( synthGraph, "" )

   private object Ser extends ImmutableSerializer[ ProcGraph ] {
      def write( d: ProcGraph, out: DataOutput ) {
         SynthGraphSerializer.write( d.synthGraph, out )
         out.writeString( d.sourceCode )
      }

      def read( in: DataInput ) : ProcGraph = {
         val graph   = SynthGraphSerializer.read( in )
         val source  = in.readString()
         apply( graph, source )
      }
   }
}
final case class ProcGraph( synthGraph: SynthGraph, sourceCode: String )
