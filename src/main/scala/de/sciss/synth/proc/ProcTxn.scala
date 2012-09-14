/*
 *  ProcTxn.scala
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

import de.sciss.osc
import de.sciss.synth.{osc => sosc}
import concurrent.stm.{TxnLocal, InTxn}
import impl.ProcTxnImpl

object ProcTxn {
   private val current = TxnLocal( initialValue = ProcTxnImpl()( _ ))

   def apply()( implicit tx: InTxn ) : ProcTxn = current.get

//   implicit def peer( implicit tx: ProcTxn ) : InTxn = tx.peer

   sealed abstract class FilterMode
   case object Always extends FilterMode
   case object IfChanges extends FilterMode
   case object RequiresChange extends FilterMode
}
trait ProcTxn {
   import ProcTxn.FilterMode

   def peer: InTxn

   def add( msg: osc.Message with sosc.Send, change: Option[ (FilterMode, RichState, Boolean) ], audible: Boolean,
            dependencies: Map[ RichState, Boolean ] = Map.empty, noErrors: Boolean = false ) : Unit
}