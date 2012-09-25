/*
 *  package.scala
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

import de.sciss.lucre.{bitemp, event => evt}
import bitemp.BiGroup
import evt.Sys

package object proc {
   private[proc] type I = evt.InMemory

   type ProcGroup[ S <: Sys[ S ]] = BiGroup[ S, Proc[ S ], Proc.Update[ S ]]
   type TimedProc[ S <: Sys[ S ]] = BiGroup.TimedElem[ S, Proc[ S ]]
   type ProcTransport[ S <: Sys[ S ]] = Transport[ S, Proc[ S ], Transport.Proc.Update[ S ]] // Proc.Update[ S ]
   type Param = Double

//   type ScanElem[ S <: Sys[ S ]] = de.sciss.synth.proc.Scan.Elem

//   type Scan[ S <: Sys[ S ]] = BiPin.Expr[ S, Scan_.Elem[ S ]]

//   type Grapheme[ S <: Sys[ S ]] = BiPin[ S, Scan_.Elem[ S ], Scan_.Elem.Update[ S ]]

   def ??? : Nothing = sys.error( "TODO" )

   implicit def dummyInMem[ S <: Sys[ S ]]( tx: S#Tx ) : _HasInMem.type = _HasInMem

   object _HasInMem {
      def inMemory : evt.InMemory#Tx = ???
   }
}