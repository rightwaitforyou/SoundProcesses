/*
 *  ProcImpl.scala
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
package impl

import de.sciss.lucre.stm.Sys
import de.sciss.synth.SynthGraph
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.DataOutput

object ProcImpl {
   def apply[ S <: Sys[ S ]]()( implicit tx: S#Tx ) : Proc[ S ] = new New[ S ]( tx )

   private sealed trait Impl[ S <: Sys[ S ]] extends Proc[ S ] {
      def id: S#ID
      protected def graphVar : S#Var[ SynthGraph ]

      final def name( implicit tx: S#Tx ) : String = name_#.value
      final def name_=( s: Expr[ S, String ])( implicit tx: S#Tx ) { name_#.set( s )}
      final def playing( implicit tx: S#Tx ) : Boolean = playing_#.value
      final def playing_=( b: Expr[ S, Boolean ])( implicit tx: S#Tx ) { playing_#.set( b )}
      final def graph( implicit tx: S#Tx ) : SynthGraph = graphVar.get
      final def graph_=( g: SynthGraph )( implicit tx: S#Tx ) { sys.error( "TODO" )}
      final def graph_=( block: => Any )( implicit tx: S#Tx ) { graph_=( SynthGraph( block ))}
      final def play()( implicit tx: S#Tx ) { sys.error( "TODO" )} // { playing_#.set( true )}
      final def stop()( implicit tx: S#Tx ) { sys.error( "TODO" )} // { playing_#.set( false )}

      final def write( out: DataOutput ) {
         sys.error( "TODO" )
      }

      final def dispose()( implicit tx: S#Tx ) {
         sys.error( "TODO" )
      }
   }

   private final class New[ S <: Sys[ S ]]( tx0: S#Tx ) extends Impl[ S ] {
      val id               = tx0.newID()
      val name_#           = sys.error( "TODO" )
      val playing_#        = sys.error( "TODO" )
      val graphVar         = sys.error( "TODO" )

      def renamed          = sys.error( "TODO" )
      def graphChanged     = sys.error( "TODO" )
      def playingChanged   = sys.error( "TODO" )
      def started          = sys.error( "TODO" )
      def stopped          = sys.error( "TODO" )
   }
}
