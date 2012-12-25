/*
 *  Synth.scala
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

import collection.breakOut
import de.sciss.synth.{Synth => SSynth, ControlSetMap, AddAction, addToHead}

object Synth {
   def apply( synthDef: SynthDef ) : Synth = {
      new Synth( SSynth( synthDef.server.peer ), synthDef )
   }
}
final case class Synth private( peer: SSynth, synthDef: SynthDef ) extends Node( false ) {
   override def toString = "Synth(id=" + peer.id + ", def=" + synthDef.name + ")"

   def server: Server = synthDef.server

   def play( target: Node, args: Seq[ ControlSetMap ] = Nil, addAction: AddAction = addToHead,
             buffers: Seq[ Buffer ] = Nil )( implicit tx: Txn ) {

      require( target.server == server )
      buffers.foreach( b => require( b.server == server ))

      val dependencies: Map[ State, Boolean ] = buffers.map( _.hasContent -> true )( breakOut )
      tx.addMessage( peer.newMsg( synthDef.name, target.peer, args, addAction ),
                     change = Some( (RequiresChange, isOnline, true) ),
                     audible = true,
                     dependencies = dependencies + (target.isOnline -> true) + (synthDef.isOnline -> true) )

//      peer.register()   // ok to call multiple times
   }
}
