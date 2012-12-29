/*
 *  Proc.scala
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

package de.sciss.synth
package proc

import de.sciss.lucre.{event => evt, expr, DataInput}
import expr.Expr
import impl.{ProcImpl => Impl}
import collection.immutable.{IndexedSeq => IIdxSeq}

object Proc {
   // ---- implementation forwards ----

   def apply[ S <: evt.Sys[ S ]]( implicit tx: S#Tx /*, store: ArtifactStore[ S ] */ ) : Proc[ S ] = Impl[ S ]

   def read[ S <: evt.Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Proc[ S ] = Impl.read( in, access )

   implicit def serializer[ S <: evt.Sys[ S ]] : evt.NodeSerializer[ S, Proc[ S ]] = Impl.serializer[ S ]

   // ---- event types ----

   final case class Update[ S <: evt.Sys[ S ]]( proc: Proc[ S ], changes: IIdxSeq[ Change[ S ]])

   sealed trait Change[ +S ]

   sealed trait StateChange extends Change[ Nothing ]
   final case class Rename( change: evt.Change[ String ]) extends StateChange
   final case class GraphChange( change: evt.Change[ SynthGraph ]) extends StateChange
//   final case class PlayingChange[ S <: evt.Sys[ S ]]( proc: Proc[ S ], change: BiPin.Expr.Update[ S, Boolean ])  extends StateChange[ S ]
//   final case class FreqChange[ S <: evt.Sys[ S ]](    proc: Proc[ S ], change: BiPin.ExprUpdate[ S, Double ])    extends Update[ S ]

   sealed trait AssociativeChange extends StateChange { def key: AssociativeKey  }
   final case class AssociationAdded( key: AssociativeKey ) extends AssociativeChange
   final case class AssociationRemoved( key: AssociativeKey ) extends AssociativeChange
//   final case class GraphemeAdded( name: String ) extends AssociativeChange
//   final case class GraphemeRemoved( name: String ) extends AssociativeChange

//   final case class AssociativeChange[ S <: evt.Sys[ S ]]( proc: Proc[ S ], added:   Set[ AssociativeKey ],
//                                                                        removed: Set[ AssociativeKey ]) extends StateChange[ S ] {
//      override def toString = "AssociativeChange(" + proc +
//         (if( added.isEmpty ) "" else ", added = " + added.mkString( ", " )) +
//         (if( removed.isEmpty) "" else ", removed = " + removed.mkString( ", " )) + ")"
//   }
   sealed trait AssociativeKey { def name: String }
   final case class ScanKey(     name: String ) extends AssociativeKey {
      override def toString = "[scan: " + name + "]"
   }
   final case class GraphemeKey( name: String ) extends AssociativeKey {
      override def toString = "[grapheme: " + name + "]"
   }

//   final case class ParamChange[ S <: evt.Sys[ S ]]( proc: Proc[ S ], changes: Map[ String, IIdxSeq[ BiPin.Expr.Update[ S, Param ]]]) extends Update[ S ]

//   final case class ScanChange[     S <: evt.Sys[ S ]]( proc: Proc[ S ], changes: Map[ String, IIdxSeq[ Scan.Update[     S ]]]) extends Update[ S ] {
//      override def toString = "ScanChange(" + proc + ", change = " + changes.map( e => e._1 + " -> " + e._2.mkString( ", " )).mkString( "(" + ", " + ")" ) + ")"
//   }
//   final case class GraphemeChange[ S <: evt.Sys[ S ]]( proc: Proc[ S ], changes: Map[ String, IIdxSeq[ Grapheme.Update[ S ]]]) extends Update[ S ] {
//      override def toString = "GraphemeChange(" + proc + ", change = " + changes.map( e => e._1 + " -> " + e._2.mkString( ", " )).mkString( "(" + ", " + ")" ) + ")"
//   }

   final case class ScanChange[ S <: evt.Sys[ S ]]( key: String, scanUpdate: Scan.Update[ S ]) extends Change[ S ] {
      override def toString = "ScanChange(" + key + ", " + scanUpdate + ")"
   }
   final case class GraphemeChange[ S <: evt.Sys[ S ]]( key: String, graphemeUpdate: Grapheme.Update[ S ]) extends Change[ S ] {
      override def toString = "GraphemeChange(" + key + ", " + graphemeUpdate + ")"
   }
}
trait Proc[ S <: evt.Sys[ S ]] extends evt.Node[ S ] {
   import Proc._

   // ---- "fields" ----

// OOO
//   def name_# : Expr.Var[ S, String ]
   def name( implicit tx: S#Tx ) : Expr[ S, String ]
   def name_=( expr: Expr[ S, String ])( implicit tx: S#Tx ) : Unit

   def graph( implicit tx: S#Tx ) : Code[ SynthGraph ]
   def graph_=( g: Code[ SynthGraph ])( implicit tx: S#Tx ) : Unit
//   def graph_=( block: => Any )( implicit tx: S#Tx ) : Unit

//// OOO
////   def playing_# : Expr.Var[ S, Boolean ]
//   def playing( implicit tx: S#Tx, chr: Chronos[ S ]) : Expr[ S, Boolean ]
//   def playing_=( expr: Expr[ S, Boolean ])( implicit tx: S#Tx, chr: Chronos[ S ]) : Unit

   // ---- controls preview demo ----

//   def par: ParamMap[ S ]
   def scans: Scans.Modifiable[ S ]
   def graphemes: Graphemes.Modifiable[ S ]

//   /**
//    * Same as `playing = true`
//    */
//   def play()( implicit tx: S#Tx, chr: Chronos[ S ]) : Unit
//   /**
//    * Same as `playing = false`
//    */
//   def stop()( implicit tx: S#Tx, chr: Chronos[ S ]) : Unit

   // ---- events ----

//   def stateChanged:    evt.Event[ S, StateChange[ S ],  Proc[ S ]]
//   def graphChanged:    evt.Event[ S, GraphChange[ S ],    Proc[ S ]]
//   def playingChanged:  evt.Event[ S, PlayingChange[ S ],  Proc[ S ]]
//   def paramChanged:    evt.Event[ S, ParamChange[ S ],    Proc[ S ]]
//   def freqChanged:     evt.Event[ S, FreqChange[ S ],     Proc[ S ]]

   def changed:         evt.Event[ S, Update[ S ],         Proc[ S ]]
}