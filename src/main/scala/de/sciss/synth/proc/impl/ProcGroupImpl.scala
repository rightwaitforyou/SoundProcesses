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

import de.sciss.collection.txn.{HASkipList, SkipList, Ordering => TxnOrdering}
import de.sciss.lucre.{DataInput, event => evt, DataOutput}
import de.sciss.lucre.stm.{InMemory, TxnSerializer, Sys}

object ProcGroupImpl {
   private val SER_VERSION = 0

   def empty[ S <: Sys[ S ]]( implicit tx: S#Tx ) : ProcGroup[ S ] = new New[ S ]( tx )

   def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : ProcGroup[ S ] =
      serializer[ S ].read( in, access )

   def serializer[ S <: Sys[ S ]] : TxnSerializer[ S#Tx, S#Acc, ProcGroup[ S ]] =
      anySer.asInstanceOf[ TxnSerializer[ S#Tx, S#Acc, ProcGroup[ S ]]]

   private val anySer = new Serializer[ InMemory ]

   private class Serializer[ S <: Sys[ S ]] extends evt.NodeSerializer[ S, ProcGroup[ S ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : ProcGroup[ S ] =
         new Read( in, access, targets, tx )
   }

   @volatile private var declMap = Map.empty[ Class[ _ ], Decl[ _ ]]

   private def getDecl[ S <: Sys[ S ]]( implicit tx: S#Tx ) : Decl[ S ] = {
      val clz = tx.system.manifest.erasure
      declMap.getOrElse( clz, {
         val declNew = new Decl[ S ]
         declMap += clz -> declNew
         declNew
      }).asInstanceOf[ Decl[ S ]]
   }

   private class Decl[ S <: Sys[ S ]] extends evt.Decl[ S, ProcGroup[ S ]] {
      val serializer: evt.Reader[ S, Impl[ S ]] = new evt.Reader[ S, Impl[ S ]] {
         def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Impl[ S ] =
            new Read( in, access, targets, tx )
      }

      type Update = ProcGroup.Update[ S ]

      import ProcGroup._

      declare[ Collection[ S ]]( _.collectionChanged )
   }

   private sealed trait Impl[ S <: Sys[ S ]] extends ProcGroup[ S ] with evt.Compound[ S, ProcGroup[ S ], Decl[ S ]] {
      protected def seq: SkipList[ S, Proc[ S ]]

      import ProcGroup._

      final def add( procs: Proc[ S ]* )( implicit tx: S#Tx ) {
         procs.foreach { p =>
            seq += p
            elementChanged += p
         }
         collectionChanged( Added( this, procs.toIndexedSeq ))
      }

      final def remove( procs: Proc[ S ]* )( implicit tx: S#Tx ) {
         procs.foreach { p =>
            seq -= p
            elementChanged -= p
         }
         collectionChanged( Removed( this, procs.toIndexedSeq ))
      }

      final protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( SER_VERSION )
         seq.write( out )
      }

      final protected def disposeData()( implicit tx: S#Tx ) {
         seq.dispose()
      }

//      final lazy val collectionChanged : Compound.CollectionEvent[ S, Impl[ S ], Decl[ S ], Proc[ S ], Proc.Update[ S ], Collection[ S ]] =
//         collection( (p: Proc[ S ]) => p.changed ).map( Element( this, _ ))
      final lazy val collectionChanged : evt.Trigger[ S, Collection[ S ], ProcGroup[ S ]] = event[ Collection[ S ]]
      final lazy val elementChanged    = collection( (p: Proc[ S ]) => p.changed ).map( Element( this, _ ))
      final lazy val changed           = collectionChanged | elementChanged
   }

   private def procOrdering[ S <: Sys[ S ]]( implicit tx: S#Tx ) : TxnOrdering[ S#Tx, Proc[ S ]] =
      new TxnOrdering[ S#Tx, Proc[ S ]] {
         private val idOrdering = tx.system.idOrdering
         def compare( a: Proc[ S ], b: Proc[ S ])( implicit tx: S#Tx ) : Int = idOrdering.compare( a.id, b.id )
      }

   private final class New[ S <: Sys[ S ]]( tx0: S#Tx ) extends Impl[ S ] {
      protected val decl      = getDecl[ S ]( tx0 )
      protected val targets   = evt.Targets[ S ]( tx0 )

      protected val seq = {
         implicit val tx      = tx0
         implicit val procOrd = procOrdering[ S ]
//         implicit val listSer = HASkipList.serializer[ S, Proc[ S ]]()
//         tx0.newVar( id, )
         HASkipList.empty[ S, Proc[ S ]]
      }
   }

   private final class Read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc, protected val targets: evt.Targets[ S ], tx0: S#Tx )
   extends Impl[ S ] {
      protected val decl      = getDecl[ S ]( tx0 )
      protected val seq = {
         implicit val tx      = tx0
         implicit val procOrd = procOrdering[ S ]
         HASkipList.read[ S, Proc[ S ]]( in, access )
      }
   }
}
