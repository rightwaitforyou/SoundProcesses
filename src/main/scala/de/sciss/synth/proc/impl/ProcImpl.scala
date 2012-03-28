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
import de.sciss.synth.expr._
import de.sciss.lucre.{DataInput, DataOutput}

object ProcImpl {
   private val SER_VERSION = 0

   def apply[ S <: Sys[ S ]]()( implicit tx: S#Tx ) : Proc[ S ] = new New[ S ]( tx )

   def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Proc[ S ] = new Read[ S ]( in, access, tx )

   private val emptyGraph = SynthGraph {}

   private sealed trait Impl[ S <: Sys[ S ]] extends Proc[ S ] {

      protected def graphVar : S#Var[ SynthGraph ]

      final def name( implicit tx: S#Tx ) : String = name_#.value
      final def name_=( s: Expr[ S, String ])( implicit tx: S#Tx ) { name_#.set( s )}
      final def playing( implicit tx: S#Tx ) : Boolean = playing_#.value
      final def playing_=( b: Expr[ S, Boolean ])( implicit tx: S#Tx ) { playing_#.set( b )}
      final def graph( implicit tx: S#Tx ) : SynthGraph = graphVar.get
      final def graph_=( g: SynthGraph )( implicit tx: S#Tx ) { graphVar.set( g )}
      final def graph_=( block: => Any )( implicit tx: S#Tx ) { graph_=( SynthGraph( block ))}
      final def play()( implicit tx: S#Tx ) { playing_#.set( true  )}
      final def stop()( implicit tx: S#Tx ) { playing_#.set( false )}

      final protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( SER_VERSION )
         name_#.write( out )
         playing_#.write( out )
         graphVar.write( out )
      }

      final protected def disposeData()( implicit tx: S#Tx ) {
         name_#.dispose()
         playing_#.dispose()
         graphVar.dispose()
      }

      override def toString = "Proc" + id

      override def hashCode : Int = id.##
      override def equals( that: Any ) = that.isInstanceOf[ Proc[ _ ]] &&
         (that.asInstanceOf[ Proc[ _ ]].id == id)
   }

   private final class New[ S <: Sys[ S ]]( tx0: S#Tx ) extends Impl[ S ] {
//      val id                  = tx0.newID()
      val name_#              = Strings.newVar[ S ]( "unnamed" )( tx0 )
      val playing_#           = Booleans.newVar[ S ]( true )( tx0 )
      protected val graphVar  = tx0.newVar[ SynthGraph ]( id, emptyGraph )( SynthGraphSerializer )

      def renamed             = sys.error( "TODO" )
      def graphChanged        = sys.error( "TODO" )
      def playingChanged      = sys.error( "TODO" )
      def started             = sys.error( "TODO" )
      def stopped             = sys.error( "TODO" )

      def targets = sys.error( "TODO" )
      def changed = sys.error( "TODO" )
      def select( slot: Int, invariant: Boolean ) = sys.error( "TODO" )
   }

   private final class Read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc, tx0: S#Tx ) extends Impl[ S ] {
//      val id                  = tx0.readID( in, access )
      val name_#              = Strings.readVar[  S ]( in, access )( tx0 )
      val playing_#           = Booleans.readVar[ S ]( in, access )( tx0 )
      protected val graphVar  = tx0.readVar[ SynthGraph ]( id, in )( SynthGraphSerializer )

      def renamed             = sys.error( "TODO" )
      def graphChanged        = sys.error( "TODO" )
      def playingChanged      = sys.error( "TODO" )
      def started             = sys.error( "TODO" )
      def stopped             = sys.error( "TODO" )

      def targets = sys.error( "TODO" )
      def changed = sys.error( "TODO" )
      def select( slot: Int, invariant: Boolean ) = sys.error( "TODO" )
   }
}
