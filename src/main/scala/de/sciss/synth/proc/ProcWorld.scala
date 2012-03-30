/*
 *  ProcWorld.scala
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

import collection.immutable.{IndexedSeq => IIdxSeq, Set => ISet}
import impl.AuralProc
import de.sciss.synth.{UGenGraph, addAfter, addBefore, Group, Server, SynthDef, SynthGraph}
import concurrent.stm.{Ref => ScalaRef}

object ProcWorld {
// MMM
//   case class Update( procsAdded: ISet[ Proc ], procsRemoved: ISet[ Proc ])
//   type Listener = TxnModel.Listener[ Update ]
}

class ProcWorld /* MMM extends TxnModel[ ProcWorld.Update ] */ {
   import ProcWorld._

// EEE
//   private type Topo = Topology[ AuralProc, ProcEdge ]
   val ugenGraphs = ScalaRef( Map.empty[ UGenGraph, RichSynthDef ])
// EEE
//   private val topologyRef = ScalaRef[ Topo ]( Topology.empty )

// MMM
//   protected def fullUpdate( implicit tx: ProcTxn ) = Update( topologyRef().vertices.toSet, Set.empty )
//   protected def emptyUpdate = Update( Set.empty, Set.empty )

// EEE
//   def topology( implicit tx: ProcTxn ) = topologyRef()

   def addProc( p: AuralProc )( implicit tx: ProcTxn ) {
// MMM
//      touch()

// EEE
//      topologyRef.transform( _.addVertex( p ))

// MMM
//      updateRef.transform( u => if( u.procsRemoved.contains( p )) {
//          u.copy( procsRemoved = u.procsRemoved - p )
//      } else {
//          u.copy( procsAdded   = u.procsAdded   + p )
//      })
   }

   def removeProc( p: AuralProc )( implicit tx: ProcTxn ) {
// MMM
//      touch()

// EEE
//      topologyRef.transform( _ removeVertex p )

// MMM
//      updateRef.transform( u => if( u.procsAdded.contains( p )) {
//          u.copy( procsAdded = u.procsAdded - p )
//      } else {
//          u.copy( procsRemoved = u.procsRemoved + p )
//      })
   }

// EEE
//   def addEdge( e: ProcEdge )( implicit tx: ProcTxn ) : Option[ (Topo, Proc, IIdxSeq[ Proc ])] = {
//      val res = topologyRef().addEdge( e )
//      res.foreach( tup => topologyRef.set( tup._1 ))
//      res
//   }
//
//   def removeEdge( e: ProcEdge )( implicit tx: ProcTxn ) {
//      topologyRef.transform( _.removeEdge( e ))
//   }
}

// MMM
//case class ProcDemiurgUpdate( factoriesAdded: ISet[ ProcFactory ], factoriesRemoved: ISet[ ProcFactory ])

object ProcDemiurg /* MMM extends TxnModel[ ProcDemiurgUpdate ] */ {
   demi =>

// MMM
//   type Update    = ProcDemiurgUpdate
//   type Listener  = TxnModel.Listener[ Update ]

   var verbose = false

   private val syn = new AnyRef
   private var servers = Set.empty[ Server ]

   private var uniqueDefID    = 0
   private def nextDefID()    = { val res = uniqueDefID; uniqueDefID += 1; res }

   def addServer( server: Server ) { syn.synchronized {
      if( servers.contains( server )) return
      servers += server
      worlds += server -> new ProcWorld
   }}

   // commented out for debugging inspection
   var worlds = Map.empty[ Server, ProcWorld ]

// FFF
//   private val factoriesRef = Ref( Set.empty[ ProcFactory ])
//   def factories( implicit tx: ProcTxn ) : Set[ ProcFactory ] = factoriesRef()

// MMM
//   protected def fullUpdate( implicit tx: ProcTxn ) = ProcDemiurgUpdate( factoriesRef(), Set.empty )
//   protected def emptyUpdate = ProcDemiurgUpdate( Set.empty, Set.empty )

// FFF
//   def addFactory( pf: ProcFactory )( implicit tx: ProcTxn ) {
//      touch
//      factoriesRef.transform( _ + pf )
//      updateRef.transform( u => if( u.factoriesRemoved.contains( pf )) {
//          u.copy( factoriesRemoved = u.factoriesRemoved - pf )
//      } else {
//          u.copy( factoriesAdded = u.factoriesAdded + pf )
//      })
//   }
//
//   def removeFactory( pf: ProcFactory )( implicit tx: ProcTxn ) {
//      touch
//      factoriesRef.transform( _ - pf )
//      updateRef.transform( u => if( u.factoriesAdded.contains( pf )) {
//          u.copy( factoriesAdded = u.factoriesAdded - pf )
//      } else {
//          u.copy( factoriesRemoved = u.factoriesRemoved + pf )
//      })
//   }

   def addVertex( e: AuralProc )( implicit tx: ProcTxn ) { syn.synchronized {
      val world = worlds( e.server )
      world.addProc( e )
   }}

   def removeVertex( e: AuralProc )( implicit tx: ProcTxn ) { syn.synchronized {
      val world = worlds( e.server )
      world.removeProc( e )
   }}

// EEE
//   def addEdge( e: ProcEdge )( implicit tx: ProcTxn ) { syn.synchronized {
//      val world = worlds( e.sourceVertex.server )
//      val res = world.addEdge( e )
//      if( res.isEmpty ) error( "Could not add edge" )
//
//      val Some( (newTopo, source, affected) ) = res
//      if( verbose ) println( "NEW TOPO = " + newTopo + "; SOURCE = " + source + "; AFFECTED = " + affected )
//      if( affected.isEmpty ) {
//         return
//      }
//
//      val srcGroup   = source.groupOption
//      val tgtGroups  = affected.map( p => (p, p.groupOption) )
//      val isAfter    = source == e.sourceVertex
//
//      def startMoving( g: RichGroup ) {
//         var succ                = g
//         var pred : RichGroup    = null
//         val iter                = tgtGroups.iterator
//         while( iter.hasNext ) {
//            pred = succ
//            val (target, tgtGroup) = iter.next()
//            tgtGroup match {
//               case Some( g2 ) => {
//                  if( isAfter ) {
//                     g2.moveAfter( true, pred )
//                  } else {
//                     g2.moveBefore( true, pred )
//                  }
//                  succ = g2
//               }
//               case None => {
//                  val g2 = RichGroup( Group( target.server ))
//                  g2.play( pred, if( isAfter ) addAfter else addBefore )
//                  target.group = g2
//                  succ = g2
//               }
//            }
//         }
//      }
//
//      srcGroup match {
//         case None => {
//            val g = RichGroup( Group( source.server ))
//            g.play( RichGroup.default( g.server ))
//            source.group = g
//            startMoving( g )
//         }
//         case Some( g ) => startMoving( g )
//      }
//   }}

// EEE
//   def removeEdge( e: ProcEdge )( implicit tx: ProcTxn ) { syn.synchronized {
//      val world = worlds( e.sourceVertex.server )
//      world.removeEdge( e )
//   }}

   def getSynthDef( server: Server, graph: SynthGraph )( implicit tx: ProcTxn ) : RichSynthDef = syn.synchronized {
      val w    = worlds( server )

      // XXX note: unfortunately we have sideeffects in the expansion, such as
      // includeParam for ProcAudioOutput ... And anyways, we might allow for
      // indeterminate GE.Lazies, thus we need to check for UGenGraph equality,
      // not SynthGraph equality
      val u = graph.expand

      implicit val itx = tx.peer
      w.ugenGraphs.get.get( u ).getOrElse {
         val name = "proc" + nextDefID()
         val rd   = RichSynthDef( server, SynthDef( name, u ))
         w.ugenGraphs.transform( _ + (u -> rd) )
         rd
      }
   }
}