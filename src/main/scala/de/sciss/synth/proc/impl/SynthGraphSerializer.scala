package de.sciss.synth.proc.impl

import de.sciss.synth.SynthGraph
import java.io.{ObjectInputStream, ObjectOutputStream}
import de.sciss.lucre.{DataInput, DataOutput}
import de.sciss.lucre.stm.ImmutableSerializer

/**
 * A serializer using plain old java (object output) serialization
 */
object SynthGraphSerializer extends ImmutableSerializer[ SynthGraph ] {
   def write( v: SynthGraph, out: DataOutput ) {
      val oos = new ObjectOutputStream( out )
      oos.writeObject( v )
   }

   def read( in: DataInput ) : SynthGraph = {
      val ois = new ObjectInputStream( in )
      ois.readObject().asInstanceOf[ SynthGraph ]
   }
}
