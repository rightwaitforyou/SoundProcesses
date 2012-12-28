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

import de.sciss.synth.{Synth => SSynth, UGenGraph, SynthGraph, ControlSetMap, AddAction, addToHead}
import impl.{SynthImpl => Impl}

object Synth {
   def apply( graph: SynthGraph, nameHint: Option[ String ] = None )
            ( target: Node, args: Seq[ ControlSetMap ] = Nil, addAction: AddAction = addToHead,
              dependencies: List[ Resource ] = Nil )( implicit tx: Txn ) : Synth = {
      val df   = ProcDemiurg.getSynthDef( target.server, graph, nameHint )
      play( df, target, args, addAction, dependencies )
   }

   private[proc] def expanded( graph: UGenGraph, nameHint: Option[ String ] = None )
                             ( target: Node, args: Seq[ ControlSetMap ] = Nil, addAction: AddAction = addToHead,
                               dependencies: List[ Resource ] = Nil )( implicit tx: Txn ) : Synth = {
      val df = ProcDemiurg.getSynthDef( target.server, graph, nameHint )
      play( df, target, args, addAction, dependencies )
   }

   private def play( df: SynthDef, target: Node, args: Seq[ ControlSetMap ], addAction: AddAction,
                     dependencies: List[ Resource ])( implicit tx: Txn ) : Synth = {
      val res  = new Impl( SSynth( target.server.peer ), df )
      res.play( target, args, addAction, df :: dependencies )
      res
   }
}
trait Synth extends Node {
   def peer: SSynth
   def definition: SynthDef
}