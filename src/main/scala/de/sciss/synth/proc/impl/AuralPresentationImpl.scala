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
import stm.{IdentifierMap, Sys, Cursor}
import de.sciss.osc.Dump
import de.sciss.synth.{SynthGraph, ServerConnection, Server}
import bitemp.{BiGroup, Chronos}

import SoundProcesses.logConfig
import concurrent.stm.Txn

object AuralPresentationImpl {
   def run[ S <: Sys[ S ]]( transport: Transport[ S, Proc[ S ]], aural: AuralSystem )
                          ( implicit tx: S#Tx, cursor: Cursor[ S ]) : AuralPresentation[ S ] = {

      val c = new Client( cursor.position, transport, aural )
      Txn.afterCommit( _ => aural.addClient( c ))( tx.peer )
      c
   }

   private final class Client[ S <: Sys[ S ]]( csrPos: S#Acc, transportStale: Transport[ S, Proc[ S ]],
                                               aural: AuralSystem )( implicit cursor: Cursor[ S ])
   extends AuralPresentation[ S ] with AuralSystem.Client {

      override def toString = "AuralPresentation@" + hashCode.toHexString

      private val sync     = new AnyRef
      private var running  = Option.empty[ Impl[ S ]]

      def dispose()( implicit tx: S#Tx ) {
         // XXX TODO dispose running
      }

      def stopped() {
         aural.removeClient( this )
         sync.synchronized {
            running.foreach { impl =>
               // XXX TODO dispose
            }
            running = None
         }
      }

      def started( server: Server ) {
         val impl = cursor.step { implicit tx =>
            val viewMap: IdentifierMap[ S#ID, S#Tx, AuralProc ] = tx.newInMemoryIDMap[ AuralProc ]
            val booted  = new Impl( server, viewMap )
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
            booted
         }
         sync.synchronized( running = Some( impl ))
      }
   }

   private final class Impl[ S <: Sys[ S ]]( server: Server, viewMap: IdentifierMap[ S#ID, S#Tx, AuralProc ]) {
      def dispose()( implicit tx: S#Tx ) {
         viewMap.dispose()
      }

      def procAdded( timed: BiGroup.TimedElem[ S, Proc[ S ]])( implicit tx: S#Tx, chr: Chronos[ S ]) {
//         val name    = p.name.value
         val p       = timed.value
         val graph   = p.graph
         val entries = Map.empty[ String, Double ] // XXX TODO p.par.entriesAt( chr.time )
         val aural   = AuralProc( server, /* name, */ graph.value.synthGraph, entries )
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
