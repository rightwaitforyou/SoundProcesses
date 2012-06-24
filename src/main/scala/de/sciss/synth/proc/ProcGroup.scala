package de.sciss.synth.proc

import de.sciss.lucre.expr.{Type, SpanLike, BiGroup}
import de.sciss.lucre.stm.{TxnSerializer, Sys}
import de.sciss.synth.expr.SpanLikes
import de.sciss.lucre.event.EventLike
import de.sciss.lucre.DataInput

///*
// *  ProcGroup.scala
// *  (SoundProcesses)
// *
// *  Copyright (c) 2010-2012 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is free software; you can redistribute it and/or
// *  modify it under the terms of the GNU General Public License
// *  as published by the Free Software Foundation; either
// *  version 2, june 1991 of the License, or (at your option) any later version.
// *
// *  This software is distributed in the hope that it will be useful,
// *  but WITHOUT ANY WARRANTY; without even the implied warranty of
// *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// *  General Public License for more details.
// *
// *  You should have received a copy of the GNU General Public
// *  License (gpl.txt) along with this software; if not, write to the Free Software
// *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.synth.proc
//
//import collection.immutable.{IndexedSeq => IIdxSeq}
//import de.sciss.lucre.{event => evt}
//import impl.ProcGroupImpl
//import de.sciss.lucre.DataInput
//import de.sciss.lucre.stm.{TxnSerializer, Sys}
//import de.sciss.collection.txn.{Iterator => TxnIterator}
//
//object ProcGroup {
//   // ---- implementation forwards ----
//
//   def empty[ S <: Sys[ S ]]( implicit tx: S#Tx ) : ProcGroup[ S ] = ProcGroupImpl.empty[ S ]
//
//   def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : ProcGroup[ S ] = ProcGroupImpl.read( in, access )
//
//   implicit def serializer[ S <: Sys[ S ]] : TxnSerializer[ S#Tx, S#Acc, ProcGroup[ S ]] = ProcGroupImpl.serializer[ S ]
//
//   // ---- event types ----
//
//   sealed trait Update[ S <: Sys[ S ]] {
//      def group: ProcGroup[ S ]
//   }
//   sealed trait Collection[ S <: Sys[ S ]] extends Update[ S ]
//   final case class Added[   S <: Sys[ S ]]( group: ProcGroup[ S ], procs:   IIdxSeq[ Proc[        S ]]) extends Collection[ S ]
//   final case class Removed[ S <: Sys[ S ]]( group: ProcGroup[ S ], procs:   IIdxSeq[ Proc[        S ]]) extends Collection[ S ]
//   final case class Element[ S <: Sys[ S ]]( group: ProcGroup[ S ], changes: IIdxSeq[ Proc.Update[ S ]]) extends Update[     S ]
//}
//trait ProcGroup[ S <: Sys[ S ]] extends evt.Node[ S ] {
//   import ProcGroup._
//
//   def id: S#ID
//
//   // ---- actions ----
//
//   def add( procs: Proc[ S ]* )( implicit tx: S#Tx ) : Unit
//   def remove( procs: Proc[ S ]* )( implicit tx: S#Tx ) : Unit
//
//   // ---- querying ----
//
//   def iterator( implicit tx: S#Tx ) : TxnIterator[ S#Tx, Proc[ S ]]
//
//   // ---- events ----
//
//   def collectionChanged: evt.Event[ S, Collection[ S ], ProcGroup[ S ]]
//   def elementChanged:    evt.Event[ S, Element[ S ],    ProcGroup[ S ]]
//   def changed:           evt.Event[ S, Update[ S ],     ProcGroup[ S ]]
//}

object ProcGroupX {
   type Update[ S <: Sys[ S ]] = BiGroup.Update[ S, Proc[ S ], Proc.Update[ S ]]

   type Var[ S <: Sys[ S ]] = BiGroup.Var[ S, Proc[ S ], Proc.Update[ S ]]

   private implicit val spanType : Type[ SpanLike ] = SpanLikes

   private def eventView[ S <: Sys[ S ]]( proc: Proc[ S ]) : EventLike[ S, Proc.Update[ S ], Proc[ S ]] = proc.changed

   def newVar[ S <: Sys[ S ]]( implicit tx: S#Tx ) : ProcGroupX.Var[ S ] =
      BiGroup.newGenericVar[ S, Proc[ S ], Proc.Update[ S ]]( eventView )

   def readVar[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : ProcGroupX.Var[ S ] =
      BiGroup.readGenericVar[ S, Proc[ S ], Proc.Update[ S ]]( in, access, eventView )

//   def readVar[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : BiGroup.Var[ S, Proc[ S ], Proc.Update[ S ]] =
//      BiGroup.readGenericVar[ S, Proc[ S ], Proc.Update[ S ]]( in, access, eventView )

   implicit def serializer[ S <: Sys[ S ]] : TxnSerializer[ S#Tx, S#Acc, ProcGroup[ S ]] = {
      BiGroup.serializer[ S, Proc[ S ], Proc.Update[ S ]]( eventView )
   }

   implicit def varSerializer[ S <: Sys[ S ]] : TxnSerializer[ S#Tx, S#Acc, ProcGroupX.Var[ S ]] = {
      BiGroup.varSerializer[ S, Proc[ S ], Proc.Update[ S ]]( eventView )
   }

//   implicit def varSerializer[ S <: Sys[ S ]] : TxnSerializer[ S#Tx, S#Acc, BiGroup.Var[ S, Proc[ S ], Proc.Update[ S ]]] = {
//      BiGroup.varSerializer[ S, Proc[ S ], Proc.Update[ S ]]( eventView )
//   }
}
//object ProcGroup_ {
//   private def eventView[ S <: Sys[ S ]]( proc: Proc[ S ]) : EventLike[ S, Proc.Update[ S ], Proc[ S ]] = proc.changed
//   private implicit val spanType : Type[ SpanLike ] = SpanLikes
//
//   def varSerializer[ S <: Sys[ S ]] : TxnSerializer[ S#Tx, S#Acc, BiGroup.Var[ S, Proc[ S ], Proc.Update[ S ]]] = {
//      BiGroup.varSerializer[ S, Proc[ S ], Proc.Update[ S ]]( eventView )
//   }
//
//   def serializer[ S <: Sys[ S ]] : TxnSerializer[ S#Tx, S#Acc, ProcGroup[ S ]] = {
//      BiGroup.serializer[ S, Proc[ S ], Proc.Update[ S ]]( eventView )
//   }
//}