/*
 *  InMemoryImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2013 Hanns Holger Rutz. All rights reserved.
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

import concurrent.stm.InTxn
import de.sciss.lucre.{stm, event => evt}
import evt.impl.ReactionMapImpl

object InMemoryImpl {
   def apply() : InMemory = new System

   private final class TxnImpl( val system: InMemory, val peer: InTxn )
   extends stm.impl.InMemoryImpl.TxnMixin[ InMemory ] with evt.impl.InMemoryImpl.TxnMixin[ InMemory ]
   with ProcTxnFullImpl[ InMemory ] {
      override def toString = "proc.InMemory#Tx@" + hashCode.toHexString

      def inMemory : InMemory#Tx = this
   }

   private final class System extends stm.impl.InMemoryImpl.Mixin[ InMemory ]
   with InMemory with ReactionMapImpl.Mixin[ InMemory ] {
      private type S = InMemory
      def wrap( peer: InTxn ) : S#Tx = new TxnImpl( this, peer )
      override def toString = "proc.InMemory@" + hashCode.toHexString
   }
}