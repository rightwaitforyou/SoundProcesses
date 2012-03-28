/*
 *  ProcGroup.scala
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

import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.lucre.event.EventLike
import de.sciss.lucre.stm.{Disposable, Writer, Sys}

object ProcGroup {
   def empty[ S <: Sys[ S ]]( implicit tx: S#Tx ) : ProcGroup[ S ] = sys.error( "TODO" )

   sealed trait Update[ S <: Sys[ S ]] {
      def group: ProcGroup[ S ]
   }
   sealed trait Collection[ S <: Sys[ S ]] extends Update[ S ]
   final case class Added[ S <: Sys[ S ]](   group: ProcGroup[ S ], procs: IIdxSeq[ Proc[ S ]]) extends Collection[ S ]
   final case class Removed[ S <: Sys[ S ]]( group: ProcGroup[ S ], procs: IIdxSeq[ Proc[ S ]]) extends Collection[ S ]
   final case class Element[ S <: Sys[ S ]]( group: ProcGroup[ S ], changes: IIdxSeq[ Proc.Update[ S ]]) extends Update[ S ]
}
trait ProcGroup[ S <: Sys[ S ]] extends Disposable[ S#Tx ] with Writer {
   import ProcGroup._

   def id: S#ID

   def add( procs: Proc[ S ]* )( implicit tx: S#Tx ) : Unit
   def remove( procs: Proc[ S ]* )( implicit tx: S#Tx ) : Unit

   def collectionChanged: EventLike[ S, Collection[ S ], ProcGroup[ S ]]
   def elementChanged:    EventLike[ S, Element[ S ], ProcGroup[ S ]]
   def changed:           EventLike[ S, Update[ S ], ProcGroup[ S ]]
}
