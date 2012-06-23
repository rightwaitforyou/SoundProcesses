/*
 *  AuralPresentationImpl.scala
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

import de.sciss.lucre.stm.{IdentifierMap, Sys, InMemory, Cursor}
import de.sciss.osc.Dump
import de.sciss.lucre.event.Change
import de.sciss.synth.{SynthGraph, ServerConnection, Server}
import de.sciss.lucre.expr.Chronos

//import collection.immutable.{IndexedSeq => IIdxSeq}
import concurrent.stm.{Txn => ScalaTxn, TxnLocal}
import SoundProcesses.logConfig

object AuralPresentationImpl {
   var dumpOSC = true

   def run[ S <: Sys[ S ], A ]( group: S#Entry[ A ], config: Server.Config = Server.Config() )
                          ( implicit cursor: Cursor[ S ], chr: Chronos[ S ], groupView: A => ProcGroup[ S ]) : AuralPresentation[ S ] = {
      val boot = new Boot( group, config, cursor, groupView )
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

   private final class Boot[ S <: Sys[ S ], A ]( groupA: S#Entry[ A ], config: Server.Config,
                                                 cursor: Cursor[ S ], groupView: A => ProcGroup[ S ])
                                               ( implicit chr: Chronos[ S ])
   extends AuralPresentation[ S ] {

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
            val group   = groupView( groupA.get )
            group.changed.react { x => println( "Aural observation: " + x )}
//            group.iterator.foreach( booted.procAdded( _ ))
//            group.changed.reactTx { implicit tx => (e: ProcGroup.Update[ S ]) => e match {
//               case ProcGroup.Added( _, procs ) =>
//                  procs.foreach( booted.procAdded( _ ))
//               case ProcGroup.Removed( _, procs ) =>
//                  procs.foreach( booted.procRemoved( _ ))
//               case ProcGroup.Element( _, changes ) =>
//                  changes.foreach {
//                     case Proc.Renamed( proc, Change( _, newName )) =>
//                        booted.procRenamed( proc, newName )
//                     case Proc.GraphChanged( proc, Change( _, newGraph )) =>
//                        booted.procGraphChanged( proc, newGraph )
////                     case Proc.PlayingChanged( proc, Change( _, newPlaying )) =>
////                        booted.procPlayingChanged( proc, newPlaying )
////                     case Proc.FreqChanged( proc, Change( _, newFreq )) =>
////                        booted.procFreqChanged( proc, newFreq )
//case _ =>
//                  }
//                  println( changes.mkString( "aural changes: ", ",", "" ))
//               case _ =>
//            }}
         }
      }
   }

   private final class Booted[ S <: Sys[ S ]]( server: Server, viewMap: IdentifierMap[ S#Tx, S#ID, AuralProc ])
                                             ( implicit chr: Chronos[ S ]) {
      def procAdded( p: Proc[ S ])( implicit tx: S#Tx ) {
         val aural = AuralProc( server, p.name.value, p.graph, p.freq.value )
         viewMap.put( p.id, aural )
         val playing = p.playing.value
         logConfig( "aural added " + p + " -- playing? " + playing )
         if( playing ) {
            implicit val ptx = ProcTxn()( tx.peer )
            aural.play()
//            actions.transform( _.addPlay( p ))
         }
      }

      def procRemoved( p: Proc[ S ])( implicit tx: S#Tx ) {
         viewMap.get( p.id ) match {
            case Some( aural ) =>
               viewMap.remove( p.id )
               implicit val ptx = ProcTxn()( tx.peer )
               logConfig( "aural removed " + p + " -- playing? " + aural.playing )
               if( aural.playing ) {
                  aural.stop()
               }
            case _ =>
               println( "WARNING: could not find view for proc " + p )
         }
      }

      def procRenamed( p: Proc[ S ], newName: String )( implicit tx: S#Tx ) {
         viewMap.get( p.id ) match {
            case Some( aural ) =>
               implicit val ptx = ProcTxn()( tx.peer )
               logConfig( "aural renamed " + p + " -- " + newName )
               aural.name = newName
            case _ =>
               println( "WARNING: could not find view for proc " + p )
         }
      }

      def procPlayingChanged( p: Proc[ S ], newPlaying: Boolean )( implicit tx: S#Tx ) {
         viewMap.get( p.id ) match {
            case Some( aural ) =>
               implicit val ptx = ProcTxn()( tx.peer )
               logConfig( "aural playing " + p + " -- " + newPlaying )
               aural.playing = newPlaying
            case _ =>
               println( "WARNING: could not find view for proc " + p )
         }
      }

      def procGraphChanged( p: Proc[ S ], newGraph: SynthGraph )( implicit tx: S#Tx ) {
         viewMap.get( p.id ) match {
            case Some( aural ) =>
               implicit val ptx = ProcTxn()( tx.peer )
               logConfig( "aural graph changed " + p )
               aural.graph = newGraph
            case _ =>
               println( "WARNING: could not find view for proc " + p )
         }
      }

      def procFreqChanged( p: Proc[ S ], newFreq: Double )( implicit tx: S#Tx ) {
         viewMap.get( p.id ) match {
            case Some( aural ) =>
               implicit val ptx = ProcTxn()( tx.peer )
               logConfig( "aural freq changed " + p )
               aural.freq = newFreq
            case _ =>
               println( "WARNING: could not find view for proc " + p )
         }
      }
   }
}
