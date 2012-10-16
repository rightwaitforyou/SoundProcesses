/*
 *  RichSynth.scala
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

import collection.breakOut
import ProcTxn.RequiresChange

final case class RichSynth( synth: Synth, synthDef: RichSynthDef ) extends RichNode( false ) {
   def node: Node = synth

   override def toString = "Synth(id=" + synth.id + ", def=" + synthDef.name + ")"

   def play( target: RichNode, args: Seq[ ControlSetMap ] = Nil, addAction: AddAction = addToHead,
             buffers: Seq[ RichBuffer ] = Nil )( implicit tx: ProcTxn ) {

      require( target.server == server )
      buffers.foreach( b => require( b.server == server ))

      val dependencies: Map[ RichState, Boolean ] = buffers.map( _.hasContent -> true )( breakOut )
      tx.add( synth.newMsg( synthDef.name, target.node, args, addAction ),
              change = Some( (RequiresChange, isOnline, true) ),
              audible = true,
              dependencies = dependencies + (target.isOnline -> true) + (synthDef.isOnline -> true) )
   }
}
