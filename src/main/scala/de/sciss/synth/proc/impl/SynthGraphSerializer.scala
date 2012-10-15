package de.sciss.synth.proc.impl

import de.sciss.synth.{Lazy, ControlProxyLike, SynthGraph}
import java.io.{ObjectInputStream, ObjectOutputStream}
import de.sciss.lucre.{DataInput, DataOutput}
import de.sciss.lucre.stm.ImmutableSerializer
import collection.immutable.{IndexedSeq => IIdxSeq}

/**
 * A serializer using plain old java (object output) serialization
 */
object SynthGraphSerializer extends ImmutableSerializer[ SynthGraph ] {
//   private final val SER_VERSION = 1

   def write( v: SynthGraph, out: DataOutput ) {
//      out.writeUnsignedByte( SER_VERSION )
//      val lazies = v.sources
//      out.writeInt( lazies.size )
      val oos = new ObjectOutputStream( out )
//      lazies.foreach { ge =>
//         oos.writeObject( ge )
//      }
//      oos.flush()
//      val proxies = v.controlProxies.iterator
//      while( proxies.hasNext ) {
//         val p = proxies.next()
//         out.writeUnsignedByte( 1 )
//         oos.writeObject( p )
//         oos.flush()
//      }
//      out.writeUnsignedByte( 0 )
oos.writeObject( v )
      oos.close()
   }

   def read( in: DataInput ) : SynthGraph = {
//      val cookie = in.readUnsignedByte()
//      require( cookie == SER_VERSION, "Unexpected cookie " + cookie )
//      val numLazies  = in.readInt()
      val ois        = new ObjectInputStream( in )
//      val lazies     = IIdxSeq.fill( numLazies ) {
//         ois.readObject().asInstanceOf[ Lazy ]
//      }
//      var hasNext = in.readUnsignedByte()
//      val proxieB = Set.newBuilder[ ControlProxyLike[ _ ]]
//      while( hasNext == 1 ) {
//         val p    = ois.readObject().asInstanceOf[ ControlProxyLike[ _ ]]
//         proxieB += p
//         hasNext  = in.readUnsignedByte()
//      }
//      val proxies = proxieB.result()
val res = ois.readObject().asInstanceOf[ SynthGraph ]
      ois.close()
//      SynthGraph( lazies, proxies )
res
   }
}
