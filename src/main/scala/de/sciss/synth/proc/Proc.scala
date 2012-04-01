/*
 *  Proc.scala
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

import de.sciss.lucre.expr.Expr
import de.sciss.synth.SynthGraph
import impl.ProcImpl
import de.sciss.lucre.stm.{TxnSerializer, Sys}
import de.sciss.lucre.{event => evt, DataInput}

object Proc {
   // ---- implementation forwards ----

   def apply[ S <: Sys[ S ]]()( implicit tx: S#Tx ) : Proc[ S ] = ProcImpl[ S ]()

   def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Proc[ S ] = ProcImpl.read( in, access )

   implicit def serializer[ S <: Sys[ S ]] : TxnSerializer[ S#Tx, S#Acc, Proc[ S ]] = ProcImpl.serializer[ S ]

   // ---- event types ----

   sealed trait Update[ S <: Sys[ S ]] {
      def proc: Proc[ S ]
   }
   final case class Renamed[ S <: Sys[ S ]](        proc: Proc[ S ], change: evt.Change[ String ])     extends Update[ S ]
   final case class GraphChanged[ S <: Sys[ S ]](   proc: Proc[ S ], change: evt.Change[ SynthGraph ]) extends Update[ S ]
   final case class PlayingChanged[ S <: Sys[ S ]]( proc: Proc[ S ], change: evt.Change[ Boolean ])    extends Update[ S ]
//   final case class Started[ S <: Sys[ S ]](        proc: Proc[ S ])                               extends Update[ S ]
   final case class FreqChanged[ S <: Sys[ S ]](    proc: Proc[ S ], change: evt.Change[ Double ])     extends Update[ S ]
//   final case class Stopped[ S <: Sys[ S ]](        proc: Proc[ S ])                               extends Update[ S ]
}
trait Proc[ S <: Sys[ S ]] extends evt.Node[ S ] {
   import Proc._

   def id: S#ID

   // ---- "fields" ----

   def name_# : Expr.Var[ S, String ]
   def name( implicit tx: S#Tx ) : String
   def name_=( expr: Expr[ S, String ])( implicit tx: S#Tx ) : Unit

   def graph( implicit tx: S#Tx ) : SynthGraph
   def graph_=( g: SynthGraph )( implicit tx: S#Tx ) : Unit
   def graph_=( block: => Any )( implicit tx: S#Tx ) : Unit

   def playing_# : Expr.Var[ S, Boolean ]
   def playing( implicit tx: S#Tx ) : Boolean
   def playing_=( expr: Expr[ S, Boolean ])( implicit tx: S#Tx ) : Unit

   // ---- controls preview demo ----

   def freq_# : Expr.Var[ S, Double ]
   def freq( implicit tx: S#Tx ) : Double
   def freq_=( f: Expr[ S, Double ])( implicit tx: S#Tx ) : Unit

   /**
    * Same as `playing = true`
    */
   def play()( implicit tx: S#Tx ) : Unit
   /**
    * Same as `playing = false`
    */
   def stop()( implicit tx: S#Tx ) : Unit

   // ---- events ----

   def renamed:         evt.Event[ S, Renamed[ S ],         Proc[ S ]]
   def graphChanged:    evt.Event[ S, GraphChanged[ S ],    Proc[ S ]]
   def playingChanged:  evt.Event[ S, PlayingChanged[ S ],  Proc[ S ]]

   def freqChanged: evt.Event[ S, FreqChanged[ S ], Proc[ S ]]

//   def started:         evt.Event[ S, Started[ S ],         Proc[ S ]]
//   def stopped:         evt.Event[ S, Stopped[ S ],         Proc[ S ]]
   def changed:         evt.Event[ S, Update[ S ],          Proc[ S ]]
}