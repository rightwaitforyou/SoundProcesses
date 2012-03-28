package de.sciss.synth.proc.impl

import de.sciss.synth.SynthGraph
import de.sciss.lucre.stm.Serializer
import java.io.{ObjectInputStream, ObjectOutputStream}
import de.sciss.lucre.{DataInput, DataOutput}

/**
 * A serializer using plain old java (object output) serialization
 */
object SynthGraphSerializer extends Serializer[ SynthGraph ] {
   def write( v: SynthGraph, out: DataOutput ) {
      val oos = new ObjectOutputStream( out )
      oos.writeObject( v )
   }

   def read( in: DataInput ) : SynthGraph = {
      val ois = new ObjectInputStream( in )
      ois.readObject().asInstanceOf[ SynthGraph ]
   }
}
