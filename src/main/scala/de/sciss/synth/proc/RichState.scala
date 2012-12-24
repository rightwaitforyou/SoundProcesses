/*
 *  RichState.scala
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

import concurrent.stm.{Ref => ScalaRef}

object RichState {
   def apply( owner: Any, name: String, init: Boolean ) : RichState = new Impl( owner, name, init )
   def and( that: RichState )( owner: Any, name: String, init: Boolean ) : RichState = new And( that, owner, name, init )

   private final class Impl( val owner: Any, val name: String, init: Boolean ) extends RichState {
      val value = ScalaRef( init )
      def swap( newValue: Boolean )( implicit tx: ProcTxn ) : Boolean = value.swap( newValue )( tx.peer )
      def get( implicit tx: ProcTxn ) : Boolean = value.get( tx.peer )
   }

   private final class And( that: RichState, val owner: Any, val name: String, init: Boolean ) extends RichState {
      val value = ScalaRef( init )
      def swap( newValue: Boolean )( implicit tx: ProcTxn ) : Boolean = {
         value.swap( newValue )( tx.peer ) && that.get
      }
      def get( implicit tx: ProcTxn ) : Boolean = value.get( tx.peer ) && that.get
   }

   sealed abstract class FilterMode
   case object Always extends FilterMode
   case object IfChanges extends FilterMode
   case object RequiresChange extends FilterMode
}
sealed trait RichState {
   protected def value: ScalaRef[ Boolean ]
   def swap( newValue: Boolean )( implicit tx: ProcTxn ) : Boolean
   def get( implicit tx: ProcTxn ) : Boolean
   final def set( newValue: Boolean )( implicit tx: ProcTxn ) { value.set( newValue )( tx.peer )}

   protected def owner: Any
   def name: String
   override def toString = "<" + owner.toString + " " + name + ">"
}