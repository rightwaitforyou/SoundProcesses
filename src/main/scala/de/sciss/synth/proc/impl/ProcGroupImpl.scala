/*
 *  ProcGroupImpl.scala
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
package impl

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.DataOutput
import de.sciss.collection.txn.{HASkipList, SkipList, Ordering => TxnOrdering}

object ProcGroupImpl {
   private val SER_VERSION = 0

   def empty[ S <: Sys[ S ]]( implicit tx: S#Tx ) : ProcGroup[ S ] = new New[ S ]( tx )

   private sealed trait Impl[ S <: Sys[ S ]] extends ProcGroup[ S ] {
      protected def seq: SkipList[ S, Proc[ S ]]

      final def add( procs: Proc[ S ]* )( implicit tx: S#Tx ) {
         procs.foreach( seq.add( _ ))
         sys.error( "TODO" )
      }

      final def remove( procs: Proc[ S ]* )( implicit tx: S#Tx ) {
         procs.foreach( seq.remove( _ ))
         sys.error( "TODO" )
      }

      final def write( out: DataOutput ) {
         out.writeUnsignedByte( SER_VERSION )
         id.write( out )
         seq.write( out )
         sys.error( "TODO" )
      }

      final def dispose()( implicit tx: S#Tx ) {
         id.dispose()
         seq.dispose()
         sys.error( "TODO" )
      }
   }

   private def procOrdering[ S <: Sys[ S ]]( implicit tx: S#Tx ) : TxnOrdering[ S#Tx, Proc[ S ]] =
      new TxnOrdering[ S#Tx, Proc[ S ]] {
         private val idOrdering = tx.system.idOrdering
         def compare( a: Proc[ S ], b: Proc[ S ])( implicit tx: S#Tx ) : Int = idOrdering.compare( a.id, b.id )
      }

   private final class New[ S <: Sys[ S ]]( tx0: S#Tx ) extends Impl[ S ] {
      val id            = tx0.newID()
      protected val seq = {
         implicit val tx      = tx0
         implicit val procOrd = procOrdering[ S ]
         implicit val listSer = HASkipList.serializer[ S, Proc[ S ]]()
         // Proc.serializer[ S ]
//         tx0.newVar( id, )
         HASkipList.empty[ S, Proc[ S ]]
      }

      def collectionChanged   = sys.error( "TODO" )
      def elementChanged      = sys.error( "TODO" )
      def changed             = sys.error( "TODO" )
   }
}
