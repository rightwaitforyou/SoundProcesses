/*
 *  ProcTxnImpl.scala
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

import de.sciss.osc
import de.sciss.synth.{Server, osc => sosc}
import actors.Futures
import collection.breakOut
import collection.immutable.{IntMap, Queue => IQueue, IndexedSeq => IIdxSeq}
import concurrent.stm.InTxn
import SoundProcesses.logTxn

object ProcTxnImpl {
   trait Flushable { def flush() : Unit }

   def apply()( implicit tx: InTxn ) : ProcTxn with Flushable = new Impl( tx )

   import ProcTxn._

   private final case class Entry( idx: Int, msg: osc.Message with sosc.Send,
                                   change: Option[ (FilterMode, RichState, Boolean) ],
                                   audible: Boolean, dependencies: Map[ RichState, Boolean ],
                                   noError: Boolean )

   private final case class EntryEdge( sourceVertex: Entry, targetVertex: Entry ) extends Topology.Edge[ Entry ]

   private val errOffMsg   = osc.Message( "/error", -1 )
   private val errOnMsg    = osc.Message( "/error", -2 )

   var timeoutFun : () => Unit = () => ()

   private final class Impl( val peer: InTxn ) extends ProcTxn with Flushable {
      tx =>

      override def toString = "ProcTxn(" + peer + ")"

      private var entries     = IQueue.empty[ Entry ]
      private var entryMap    = Map.empty[ (RichState, Boolean), Entry ]
      private var stateMap    = Map.empty[ RichState, Boolean ]
      private var entryCnt    = 0

      private var beforeHooks = IIdxSeq.empty[ ProcTxn => Unit ]

      def beforeCommit( handler: ProcTxn => Unit ) {
         beforeHooks :+= handler
      }

      def flush() {
         if( beforeHooks.nonEmpty ) beforeHooks.foreach( _.apply( this ))

         logTxn( "flush" )
         val (clumps, maxSync) = establishDependancies
val server = Server.default // XXX vergación
         clumps.foreach { tup =>
            val (idx, msgs) = tup
            if( idx <= maxSync ) {
               val syncMsg    = server.syncMsg
               val syncID     = syncMsg.id
               val bndl       = osc.Bundle.now( (msgs :+ syncMsg): _* )
               val fut        = server !! (bndl, { case sosc.SyncedMessage( `syncID` ) => true })
               // XXX should use heuristic for timeouts
               Futures.awaitAll( 10000L, fut ) match {
                  case Some( true ) :: Nil =>
                  case _ =>
                     fut.revoke()
                     timeoutFun()
                     sys.error( "Timeout (while waiting for /synced " + syncID + ")" )
               }
            } else {
               server ! osc.Bundle.now( msgs: _* ) // XXX eventually audible could have a bundle time
            }
         }
      }

      // XXX TODO IntMap lost. might eventually implement the workaround
      // by jason zaugg : http://gist.github.com/452874
      private def establishDependancies : (Map[ Int, IIdxSeq[ osc.Message ]], Int) = {
         var topo = Topology.empty[ Entry, EntryEdge ]

         var clumpEdges = Map.empty[ Entry, Set[ Entry ]]

         entries.foreach( targetEntry => {
            topo = topo.addVertex( targetEntry )
            targetEntry.dependencies.foreach( dep => {
               entryMap.get( dep ).map( sourceEntry => {
                  val edge = EntryEdge( sourceEntry, targetEntry )
                  topo.addEdge( edge ) match {
                     case Some( (newTopo, _, _) ) => {
                        topo = newTopo
                        // clumping occurs when a synchronous message depends on
                        // an asynchronous message
                        if( !sourceEntry.msg.isSynchronous && targetEntry.msg.isSynchronous ) {
                           clumpEdges += targetEntry -> (clumpEdges.getOrElse( targetEntry, Set.empty ) + sourceEntry)
                        }
                     }
                     case None => {
                        sys.error( "Unsatisfied dependancy " + edge )
                     }
                  }
               }).getOrElse({
                  val (state, value) = dep
                  if( stateMap.get( state ) != Some( value )) {
                     sys.error( "Unsatisfied dependancy " + dep )
                  }
               })
            })
         })

         // clumping
         var clumpIdx   = 0
         var clumpMap   = Map.empty[ Entry, Int ]
         var clumps     = IntMap.empty[ List[ Entry ]]
         val audibleIdx = Int.MaxValue
         topo.vertices.foreach( targetEntry => {
            if( targetEntry.audible ) {
               clumps += audibleIdx -> (targetEntry :: clumps.getOrElse( audibleIdx, Nil ))
               clumpMap += targetEntry -> audibleIdx
            } else {
               val depIdx = clumpEdges.get( targetEntry ).map( set => {
                  set.map( clumpMap.getOrElse( _, sys.error( "Unsatisfied dependancy " + targetEntry ))).max
               }).getOrElse( -1 )
               if( depIdx > clumpIdx ) sys.error( "Unsatisfied dependancy " + targetEntry )
               if( depIdx == clumpIdx ) clumpIdx += 1
               clumps += clumpIdx -> (targetEntry :: clumps.getOrElse( clumpIdx, Nil ))
               clumpMap += targetEntry -> clumpIdx
            }
         })

//         if( verbose ) clumps.foreach( tup => {
//            val (idx, msgs) = tup
//            println( "clump #" + idx + " : " + msgs.toList )
//         })

         val sorted: Map[ Int, IIdxSeq[ osc.Message ]] = clumps mapValues { entries =>
            var noError = false
            entries.sortWith( (a, b) => {
               // here comes the tricky bit:
               // preserve dependencies, but also
               // entry indices in the case that there
               // are no indices... we should modify
               // topology instead eventually XXX
               val someB = Some( b )
               val someA = Some( a )
               val adep  = a.dependencies.exists( tup => entryMap.get( tup ) == someB )
               if( !adep ) {
                  val bdep = b.dependencies.exists( tup => entryMap.get( tup ) == someA )
                  if( !bdep ) a.idx < b.idx
                  else true
               } else false

            }).flatMap( entry => {
               if( entry.noError == noError ) {
                  List( entry.msg )
               } else {
                  noError = !noError
                  List( if( noError ) errOffMsg else errOnMsg, entry.msg )
               }
            })( breakOut )
         }
         (sorted, if( clumps.contains( audibleIdx )) clumpIdx else clumpIdx - 1)
      }

      def add( msg: osc.Message with sosc.Send, change: Option[ (FilterMode, RichState, Boolean) ], audible: Boolean,
               dependencies: Map[ RichState, Boolean ], noError: Boolean = false ) {

         logTxn( "add " + ((msg, change, audible, dependencies, noError)) )

         def processDeps : Entry = {
            dependencies.foreach { tup =>
               val (state, _) = tup
               if( !stateMap.contains( state )) {
                  stateMap += state -> state.get( tx )
               }
            }
            val entry = Entry( entryCnt, msg, change, audible, dependencies, noError )
            entryCnt += 1
            entries = entries.enqueue( entry )
            entry
         }

         change.map( tup => {
            val (mode, state, value) = tup
            val changed = state.get( tx ) != value
            require( changed || (mode != RequiresChange) )
            if( changed || (mode == Always) ) {
               // it is important that processDeps is
               // executed before state.set as the object
               // might depend on a current state of its own
               val entry = processDeps
               entryMap += (state, value) -> entry
               if( changed ) state.set( value )( tx )
            }
         }).getOrElse( processDeps )
      }
   }
}
