/*
 *  RichSynthDef.scala
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

import de.sciss.synth.{Synth, addToHead, AddAction, ControlSetMap, SynthDef, Server, SynthGraph}
import ProcTxn.IfChanges

object RichSynthDef {
   def apply( server: Server, graph: SynthGraph )( implicit tx: ProcTxn ) : RichSynthDef =
      ProcDemiurg.getSynthDef( server, graph )
}
final case class RichSynthDef( server: Server, synthDef: SynthDef ) /* extends RichObject */ {
   val isOnline: RichState = new RichState( this, "isOnline", false )

   def name : String = synthDef.name

   /**
    *    Actually checks if the def is already online.
    *    Only if that is not the case, the receive message
    *    will be queued.
    */
   def recv()( implicit tx: ProcTxn ) {
      tx.add( synthDef.recvMsg, change = Some( (IfChanges, isOnline, true) ), audible = false )
   }

   def play( target: RichNode, args: Seq[ ControlSetMap ] = Nil,
             addAction: AddAction = addToHead, bufs: Seq[ RichBuffer ] = Nil )( implicit tx: ProcTxn ) : RichSynth = {
      recv()  // make sure it is online
      val synth   = Synth( server )
      val rs      = RichSynth( synth, this )
      rs.play( target, args, addAction, bufs )
      rs
   }
}
