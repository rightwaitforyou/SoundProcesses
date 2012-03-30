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
import de.sciss.lucre.stm.{IdentifierMap, Sys, InMemory, Cursor}
import de.sciss.osc.Dump

//import collection.immutable.{IndexedSeq => IIdxSeq}
import concurrent.stm.{Txn => ScalaTxn, TxnLocal}

object AuralizationImpl {
   var dumpOSC = true

   def run[ S <: Sys[ S ]]( group: S#Entry[ ProcGroup[ S ]], config: Server.Config = Server.Config() )
                          ( implicit cursor: Cursor[ S ]) : Auralization[ S ] = {
      val boot = new Boot( group, config, cursor )
      Runtime.getRuntime.addShutdownHook( new Thread( new Runnable {
         def run() { boot.shutDown() }
      }))
      boot.start()
      boot
   }

//   private sealed trait Action // [ S <: Sys[ S ]]
//   private final case class ActionPlay() extends Action
//   private final case class ActionStop() extends Action
//
//   private object Actions {
//      def empty[ S <: Sys[ S ]] : Actions[ S ] = // anyEmpty.asInstanceOf[ Actions[ S ]]
//         sys.error( "TODO" )
//
////      private val anyEmpty = Actions[ InMemory ]( Map.empty )
//   }
//   private case class Actions[ S <: Sys[ S ]]( map: Map[ AuralProc, Action ]) {
//      def nonEmpty : Boolean = map.nonEmpty
//
//      def addPlay( p: Proc[ S ]) : Actions[ S ] = {
//         map.get( p ) match {
//            case Some( ActionStop() )  => this.copy( map = map - p )
//            case None                  => this.copy( map = map + (p -> ActionPlay()) )
//            case _                     => this
//         }
//      }
//
//      def addStop( p: Proc[ S ]) : Actions[ S ] = {
//         map.get( p ) match {
//            case Some( ActionPlay() )  => this.copy( map = map - p )
//            case None                  => this.copy( map = map + (p -> ActionStop()) )
//            case _                     => this
//         }
//      }
//   }

   private final class Boot[ S <: Sys[ S ]]( groupA: S#Entry[ ProcGroup[ S ]], config: Server.Config,
                                             cursor: Cursor[ S ])
   extends Auralization[ S ] {

//      private val actions: TxnLocal[ Actions[ S ]] = TxnLocal( initialValue = { implicit itx =>
//         ScalaTxn.beforeCommit { implicit itx =>
//            val m = actions().map
//            if( m.nonEmpty ) ScalaTxn.afterCommit { _ =>
//               processActions( m )
//            }
//         }
//         Actions.empty[ S ]
//      })

//      private def processActions( m: Map[ Proc[ S ], Action ]) {
//         m.foreach {
//            case (p, ActionPlay()) =>
//            case (p, ActionStop()) =>
//         }
//      }

      private val sync        = new AnyRef
      private var connection  = Option.empty[ Either[ ServerConnection, Server ]]

      def start() {
         sync.synchronized {
            val c = Server.boot( "SoundProcesses", config ) {
               case ServerConnection.Aborted =>
                  sync.synchronized { connection = None }
               case ServerConnection.Running( s ) =>
                  sync.synchronized { connection = Some( Right( s ))}
                  booted( s )
            }
            connection = Some( Left( c ))
         }
      }

      def shutDown() {
         sync.synchronized {
            connection.foreach {
               case Left( c )    => c.abort
               case Right( s )   => s.quit
            }
            connection = None
         }
      }

      private def booted( server: Server ) {
         if( dumpOSC ) server.dumpOSC( Dump.Text )
         cursor.step { implicit tx =>
            val viewMap: IdentifierMap[ S#Tx, S#ID, AuralProc ] = tx.newInMemoryIDMap[ AuralProc ]
            val booted  = new Booted( server, viewMap )
            ProcDemiurg.addServer( server )( ProcTxn()( tx.peer ))
            val group   = groupA.get
            group.elements.foreach( booted.procAdded( _ ))
            group.changed.reactTx { implicit tx => (e: ProcGroup.Update[ S ]) => e match {
               case ProcGroup.Added( _, procs ) =>
                  procs.foreach( booted.procAdded( _ ))
//                  println( procs.mkString( "added: ", ",", "" ))
               case ProcGroup.Removed( _, procs ) =>
                  println( procs.mkString( "aural removed: ", ",", "" ))
               case ProcGroup.Element( _, changes ) =>
                  println( changes.mkString( "aural changes: ", ",", "" ))
               case _ =>
            }}
         }
      }
   }

   private final class Booted[ S <: Sys[ S ]]( server: Server, viewMap: IdentifierMap[ S#Tx, S#ID, AuralProc ]) {
      def procAdded( p: Proc[ S ])( implicit tx: S#Tx ) {
         val aural = AuralProc( server, p.name, p.graph )
         viewMap.put( p.id, aural )
         println( "aural added " + p + " -- playing? " + p.playing )
         if( p.playing ) {
            implicit val ptx = ProcTxn()( tx.peer )
            aural.play()
//            actions.transform( _.addPlay( p ))
         }
      }
   }
}
