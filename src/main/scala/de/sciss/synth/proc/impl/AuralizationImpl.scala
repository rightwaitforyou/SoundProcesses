/*
 *  AuralizationImpl.scala
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

import de.sciss.synth.{ServerConnection, Server}
import concurrent.stm.TxnLocal
import de.sciss.lucre.stm.{Sys, InMemory, Cursor}

object AuralizationImpl {
   def run[ S <: Sys[ S ]]( group: S#Entry[ ProcGroup[ S ]], config: Server.Config = Server.Config() )
                          ( implicit cursor: Cursor[ S ]) : Auralization[ S ] = {
      val res = new Boot( group, config, cursor )
      res.start()
      res
   }

   private object Actions {
      def empty[ S <: Sys[ S ]] : Actions[ S ] = anyEmpty.asInstanceOf[ Actions[ S ]]

      private val anyEmpty = Actions[ InMemory ]( Set.empty, Set.empty )
   }
   private case class Actions[ S <: Sys[ S ]]( play: Set[ Proc[ S ]], stop: Set[ Proc[ S ]]) {
      def addPlay( p: Proc[ S ]) : Actions[ S ] = {
         if( play.contains( p )) this else {
            if( stop.contains( p )) {
               copy( stop = stop - p )
            } else {
               copy( play = play + p )
            }
         }
      }
      def addStop( p: Proc[ S ]) : Actions[ S ] = {
         if( stop.contains( p )) this else {
            if( play.contains( p )) {
               copy( play = play - p )
            } else {
               copy( stop = stop + p )
            }
         }
      }
   }

   private final class Boot[ S <: Sys[ S ]]( groupA: S#Entry[ ProcGroup[ S ]], config: Server.Config,
                                             cursor: Cursor[ S ])
   extends Auralization[ S ] {

      private val actions = TxnLocal( Actions.empty[ S ])

      def start() {
         /* val booting = */ Server.boot( "SoundProcesses", config ) {
            case ServerConnection.Aborted =>
            case ServerConnection.Running( s ) => booted( s )
         }
      }

      def booted( s: Server ) {
         cursor.step { implicit tx =>
            implicit val itx = tx.peer
            val group = groupA.get
            group.elements.foreach { p =>
               if( p.playing ) actions.transform( _.addPlay( p ))
            }
            group.changed.reactTx { implicit tx => (e: ProcGroup.Update[ S ]) => e match {
               case _ =>
            }}
         }
      }
   }
}
