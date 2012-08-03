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

import de.sciss.lucre.{stm, bitemp}
import stm.{TxnSerializer, Source, IdentifierMap, Sys, Cursor}
import de.sciss.osc.Dump
import de.sciss.synth.{SynthGraph, ServerConnection, Server}
import bitemp.{BiGroup, Chronos}

import SoundProcesses.logConfig
import concurrent.stm.Txn

object AuralPresentationImpl {
   var dumpOSC = true

   def run[ S <: Sys[ S ]]( transport: Transport[ S, Proc[ S ]], config: Server.Config = Server.Config() )
                          ( implicit tx: S#Tx, cursor: Cursor[ S ]) : AuralPresentation[ S ] = {
//      require( Txn.findCurrent.isEmpty, "AuralPresentation.run must be called outide of transaction" )
      val boot = new Boot( cursor.position, transport, config )
      Txn.afterCommit( _ => {
         Runtime.getRuntime.addShutdownHook( new Thread( new Runnable {
            def run() { boot.shutDown() }
         }))
         boot.start()
      })( tx.peer )
      boot
   }

   private final class Boot[ S <: Sys[ S ]]( csrPos: S#Acc, transportStale: Transport[ S, Proc[ S ]], config: Server.Config )
                                           ( implicit cursor: Cursor[ S ])
   extends AuralPresentation[ S ] {

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
            val viewMap: IdentifierMap[ S#ID, S#Tx, AuralProc ] = tx.newInMemoryIDMap[ AuralProc ]
            val booted  = new Booted( server, viewMap )
            ProcDemiurg.addServer( server )( ProcTxn()( tx.peer ))
            val transport = tx.refresh( csrPos, transportStale )
            transport.changed.react { x => println( "Aural observation: " + x )}
            if( transport.playing.value ) {
               implicit val chr: Chronos[ S ] = transport
               transport.iterator.foreach { case (_, p) => booted.procAdded( p )}
            }
            transport.changed.reactTx { implicit tx => {
               case Transport.Advance( tr, true, time, added, removed, params ) =>
                  implicit val chr: Chronos[ S ] = tr
//println( "AQUI: added = " + added + "; removed = " + removed )
                  removed.foreach { case (_, p)    => booted.procRemoved( p )}
                  params.foreach  { case (_, p, m) => booted.procParamsChanged( p, m )}
                  added.foreach   { case (_, p)    => booted.procAdded( p )}
               case _ =>
            }}
         }
      }
   }

   private final class Booted[ S <: Sys[ S ]]( server: Server, viewMap: IdentifierMap[ S#ID, S#Tx, AuralProc ]) {
      def procAdded( timed: BiGroup.TimedElem[ S, Proc[ S ]])( implicit tx: S#Tx, chr: Chronos[ S ]) {
//         val name    = p.name.value
         val p       = timed.value
         val graph   = p.graph
         val entries = p.par.entriesAt( chr.time )
         val aural   = AuralProc( server, /* name, */ graph, entries )
         viewMap.put( timed.id, aural )
         val playing = p.playing.value
         logConfig( "aural added " + p + " -- playing? " + playing )
         if( playing ) {
            implicit val ptx = ProcTxn()( tx.peer )
            aural.play()
//            actions.transform( _.addPlay( p ))
         }
      }

      def procRemoved( timed: BiGroup.TimedElem[ S, Proc[ S ]])( implicit tx: S#Tx ) {
         val id = timed.id
         viewMap.get( id ) match {
            case Some( aural ) =>
               viewMap.remove( id )
               implicit val ptx = ProcTxn()( tx.peer )
               logConfig( "aural removed " + timed.value + " -- playing? " + aural.playing )
               if( aural.playing ) {
                  aural.stop()
               }
            case _ =>
               println( "WARNING: could not find view for proc " + timed.value )
         }
      }

//      def procRenamed( p: Proc[ S ], newName: String )( implicit tx: S#Tx ) {
//         viewMap.get( p.id ) match {
//            case Some( aural ) =>
//               implicit val ptx = ProcTxn()( tx.peer )
//               logConfig( "aural renamed " + p + " -- " + newName )
//               aural.name = newName
//            case _ =>
//               println( "WARNING: could not find view for proc " + p )
//         }
//      }

      def procPlayingChanged( timed: BiGroup.TimedElem[ S, Proc[ S ]], newPlaying: Boolean )( implicit tx: S#Tx ) {
         viewMap.get( timed.id ) match {
            case Some( aural ) =>
               implicit val ptx = ProcTxn()( tx.peer )
               logConfig( "aural playing " + timed.value + " -- " + newPlaying )
               aural.playing = newPlaying
            case _ =>
               println( "WARNING: could not find view for proc " + timed.value )
         }
      }

      def procGraphChanged( timed: BiGroup.TimedElem[ S, Proc[ S ]], newGraph: SynthGraph )( implicit tx: S#Tx ) {
         viewMap.get( timed.id ) match {
            case Some( aural ) =>
               implicit val ptx = ProcTxn()( tx.peer )
               logConfig( "aural graph changed " + timed.value )
               aural.graph = newGraph
            case _ =>
               println( "WARNING: could not find view for proc " + timed.value )
         }
      }

      def procParamsChanged( timed: BiGroup.TimedElem[ S, Proc[ S ]], changes: Map[ String, Param ])( implicit tx: S#Tx ) {
         viewMap.get( timed.id ) match {
            case Some( aural ) =>
               implicit val ptx = ProcTxn()( tx.peer )
               logConfig( "aural freq changed " + timed.value )
               aural.addParams( changes )
            case _ =>
               println( "WARNING: could not find view for proc " + timed.value )
         }
      }
   }
}
