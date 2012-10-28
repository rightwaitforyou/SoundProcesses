/*
 *  SynthGraphSerializer.scala
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

package de.sciss.synth
package proc
package impl

import java.io.{ObjectInputStream, ObjectOutputStream}
import de.sciss.lucre.{DataInput, DataOutput, stm}
import stm.ImmutableSerializer

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
