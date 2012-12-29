/*
 *  ResourceImpl.scala
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

import concurrent.stm.{TSet, Ref}

private[proc] trait ResourceImpl extends Resource {
   import Resource.TimeStamp

   private val timeStampRef   = Ref( 0 )
   private val dependentsRef  = TSet.empty[ Resource ]

   final def isOnline( implicit tx: Txn ) : Boolean = timeStamp >= 0

   final protected def disposed()( implicit tx: Txn ) {
      require( dependentsRef.isEmpty( tx.peer ), "Disposing a resource which still has dependents : " + this )
      timeStamp_=( -1 )
   }

   final def timeStamp( implicit tx: Txn ) : TimeStamp = timeStampRef.get( tx.peer )
   final def timeStamp_=( value: TimeStamp )( implicit tx: Txn ) {
      timeStampRef.set( value )( tx.peer )
   }

   final def addDependent( dependent: Resource )( implicit tx: Txn ) {
      require( dependent.server == server, "Dependency uses divergent server : " + dependent )
      dependentsRef.add( dependent )( tx.peer )
   }

   final def removeDependent( dependent: Resource )( implicit tx: Txn ) {
      dependentsRef.remove( dependent )( tx.peer )
   }
}