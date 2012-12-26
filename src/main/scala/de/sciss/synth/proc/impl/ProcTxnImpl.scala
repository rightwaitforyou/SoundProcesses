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
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.synth.{osc => sosc}

private[proc] object ProcTxnImpl {
//   private final case class Entry( idx: Int, msg: osc.Message with sosc.Send,
////                                   change: Option[ (FilterMode, State, Boolean) ],
//                                   audible: Boolean, dependencies: Map[ State, Boolean ],
//                                   noError: Boolean )
//
//   private final case class EntryEdge( sourceVertex: Entry, targetVertex: Entry ) extends Topology.Edge[ Entry ]

//   private val errOffMsg   = osc.Message( "/error", -1 )
//   private val errOnMsg    = osc.Message( "/error", -2 )

   var timeoutFun : () => Unit = () => ()

//   trait Flushable { def flush() : Unit }
//
//   def apply()( implicit tx: InTxn ) : ProcTxn with Flushable = new Impl( tx )
//

   /**
    * A data type encapsulating all the outgoing OSC bundles for this transaction.
    *
    * @param firstStamp the time stamp of the first bundle in the payload, shifted 1 bit to the right
    *                   (i.e. actual bundle index)
    * @param payload    the succession of bundles, represented as a sequence of a sequence of messages
    */
   private final case class Bundles( firstStamp: Int, payload: IIdxSeq[ IIdxSeq[ osc.Message ]])
   private final val noBundles = Bundles( 0, IIdxSeq.empty )

   var TIMEOUT_MILLIS = 10000L
}
private[proc] trait ProcTxnImpl[ S <: Sys[ S ]] extends Sys.Txn[ S ] {
   tx =>

   import ProcTxnImpl._

//   private var bundles = IntMap.empty[ ... ]

   private var bundlesMap = Map.empty[ Server, Bundles ]

   private def flush() {
      bundlesMap.foreach { case (server, bundles) =>
         val peer = server.peer

         def loop( pay: List[ IIdxSeq[ osc.Message with sosc.Send ]], idx: Int ) {
            pay match {
               case msgs :: Nil if msgs.forall( _.isSynchronous ) =>
                  val p = msgs match {
                     case IIdxSeq( msg )  => msg   // one message, send it out directly
                     case _               => osc.Bundle.now( msgs: _* )
                  }
                  peer ! p

               case msgs :: tail =>
                  val syncMsg    = server.peer.syncMsg()
                  val syncID     = syncMsg.id
                  val bndl       = osc.Bundle.now( (msgs :+ syncMsg): _* )
                  peer !? (TIMEOUT_MILLIS, bndl, {
                     case sosc.SyncedMessage( `syncID` ) =>
                        loop( tail )
                     case sosc.TIMEOUT =>
                        logTxn( "TIMEOUT while sending OSC bundle!" )
                        loop( tail )
                  })

               case Nil => sys.error( "Unexpected case - empty bundle" )
            }
         }

         loop( bundles.payload.toList, bundles.firstStamp )
      }
   }

   def addMessage( resource: Resource, message: osc.Message with sosc.Send, audible: Boolean, dependencies: Seq[ Resource ],
                   noErrors: Boolean ) {

//      val rsrc = system.resources

      val server  = resource.server
      val tsOld   = resource.timeStamp( tx )
      require( tsOld >= 0, "Already disposed : " + resource )

      // calculate the maximum time stamp from the dependencies. this includes
      // the resource as its own dependency (since we should send out messages
      // in monotonic order)
      var dTsMax  = tsOld
      dependencies.foreach { dep =>
         val dts = dep.timeStamp( tx )
         require( dts >= 0, "Dependency already disposed : " + dep )
         if( dts > dTsMax ) dTsMax = dts
         dep.addDependent( resource )( tx )  // validates dep's server
      }

//      val dAsync     = (dTsMax & 1) == 1
      val msgAsync   = !message.isSynchronous

      // if the message is asynchronous, it suffices to ensure that the time stamp's async bit is set.
      // otherwise clear the async flag (& ~1), and if the maximum dependency is async, increase the time stamp
      // (from bit 1, i.e. `+ 2`); this second case is efficiently produced through 'rounding up' (`(_ + 1) & ~1`).
      val tsNew      = if( msgAsync ) dTsMax | 1 else (dTsMax + 1) & ~1

      val bOld       = bundlesMap.getOrElse( server, noBundles )
      val bNew       = if( bOld.payload.isEmpty ) {
         afterCommit( flush() )
         Bundles( tsNew, IIdxSeq( IIdxSeq( message )))

      } else {
         val idxOld  = bOld.firstStamp
         val idxNew  = tsNew >> 1
         val payOld  = bOld.payload
         val szOld   = payOld.size
         if( idxNew == idxOld - 1 ) {   // prepend to front
            val payNew = IIdxSeq( message ) +: payOld
            bOld.copy( firstStamp = idxNew, payload = payNew )

         } else if( idxNew == idxOld + szOld ) {      // append to back
            val payNew  = payOld :+ IIdxSeq( message )
            bOld.copy( payload = payNew )

         } else {
            // we don't need the assertion, since we are going to call payload.apply which would
            // through an out of bounds exception if the assertion wouldn't hold
//            assert( idxNew >= idxOld && idxNew < idxOld + szOld )
            val payIdx = idxNew - idxOld
            val payNew = payOld.updated( payIdx, payOld( payIdx ) :+ message )
            bOld.copy( payload = payNew )
         }
      }

      bundlesMap += server -> bNew
   }

//   private var entries     = IQueue.empty[ Entry ]
//   private var entryMap    = Map.empty[ (State, Boolean), Entry ]
//   private var stateMap    = Map.empty[ State, Boolean ]
//   private var entryCnt    = 0
//
//   private var beforeHooks = IIdxSeq.empty[ Txn => Unit ]
//
//   def beforeCommit( handler: Txn => Unit ) {
//      beforeHooks :+= handler
//   }

//   def flush() {
//      if( beforeHooks.nonEmpty ) beforeHooks.foreach( _.apply( this ))
//
//      logTxn( "flush @" + hashCode().toHexString + " (peer=" + peer.hashCode().toHexString + ")" )
//      val (clumps, maxSync) = establishDependancies
//val server = Server.default // XXX vergación
//      clumps.foreach { tup =>
//         val (idx, msgs) = tup
//         if( idx <= maxSync ) {
//            val syncMsg    = server.syncMsg
//            val syncID     = syncMsg.id
//            val bndl       = osc.Bundle.now( (msgs :+ syncMsg): _* )
//            @volatile var success = false
//            val resp       = sosc.Responder.once( server ) {
//               case sosc.SyncedMessage( `syncID` ) =>
//                  bndl.synchronized {
//                     success = true
//                     bndl.notify()
//                  }
//            }
//            bndl.synchronized {
//               server ! bndl
//               bndl.wait( 10000L )
//            }
//            if( !success ) {
//               resp.remove()
//               timeoutFun()
//               sys.error( "Timeout (while waiting for /synced " + syncID + ")" )
//            }
//         } else {
//            server ! osc.Bundle.now( msgs: _* ) // XXX eventually audible could have a bundle time
//         }
//      }
//   }

//   // XXX TODO IntMap lost. might eventually implement the workaround
//   // by jason zaugg : http://gist.github.com/452874
//   private def establishDependancies : (Map[ Int, IIdxSeq[ osc.Message ]], Int) = {
//      var topo = Topology.empty[ Entry, EntryEdge ]
//
//      var clumpEdges = Map.empty[ Entry, Set[ Entry ]]
//
//      entries.foreach( targetEntry => {
//         topo = topo.addVertex( targetEntry )
//         targetEntry.dependencies.foreach( dep => {
//            entryMap.get( dep ).map( sourceEntry => {
//               val edge = EntryEdge( sourceEntry, targetEntry )
//               topo.addEdge( edge ) match {
//                  case Some( (newTopo, _, _) ) => {
//                     topo = newTopo
//                     // clumping occurs when a synchronous message depends on
//                     // an asynchronous message
//                     if( !sourceEntry.msg.isSynchronous && targetEntry.msg.isSynchronous ) {
//                        clumpEdges += targetEntry -> (clumpEdges.getOrElse( targetEntry, Set.empty ) + sourceEntry)
//                     }
//                  }
//                  case None => {
//                     sys.error( "Unsatisfied dependancy " + edge )
//                  }
//               }
//            }).getOrElse({
//               val (state, value) = dep
//               if( stateMap.get( state ) != Some( value )) {
//                  sys.error( "Unsatisfied dependancy " + dep )
//               }
//            })
//         })
//      })
//
//      // clumping
//      var clumpIdx   = 0
//      var clumpMap   = Map.empty[ Entry, Int ]
//      var clumps     = IntMap.empty[ List[ Entry ]]
//      val audibleIdx = Int.MaxValue
//      topo.vertices.foreach( targetEntry => {
//         if( targetEntry.audible ) {
//            clumps += audibleIdx -> (targetEntry :: clumps.getOrElse( audibleIdx, Nil ))
//            clumpMap += targetEntry -> audibleIdx
//         } else {
//            val depIdx = clumpEdges.get( targetEntry ).map( set => {
//               set.map( clumpMap.getOrElse( _, sys.error( "Unsatisfied dependancy " + targetEntry ))).max
//            }).getOrElse( -1 )
//            if( depIdx > clumpIdx ) sys.error( "Unsatisfied dependancy " + targetEntry )
//            if( depIdx == clumpIdx ) clumpIdx += 1
//            clumps += clumpIdx -> (targetEntry :: clumps.getOrElse( clumpIdx, Nil ))
//            clumpMap += targetEntry -> clumpIdx
//         }
//      })
//
////         if( verbose ) clumps.foreach( tup => {
////            val (idx, msgs) = tup
////            println( "clump #" + idx + " : " + msgs.toList )
////         })
//
//      val sorted: Map[ Int, IIdxSeq[ osc.Message ]] = clumps mapValues { entries =>
//         var noError = false
//         entries.sortWith( (a, b) => {
//            // here comes the tricky bit:
//            // preserve dependencies, but also
//            // entry indices in the case that there
//            // are no indices... we should modify
//            // topology instead eventually XXX
//            val someB = Some( b )
//            val someA = Some( a )
//            val adep  = a.dependencies.exists( tup => entryMap.get( tup ) == someB )
//            if( !adep ) {
//               val bdep = b.dependencies.exists( tup => entryMap.get( tup ) == someA )
//               if( !bdep ) a.idx < b.idx
//               else true
//            } else false
//
//         }).flatMap( entry => {
//            if( entry.noError == noError ) {
//               List( entry.msg )
//            } else {
//               noError = !noError
//               List( if( noError ) errOffMsg else errOnMsg, entry.msg )
//            }
//         })( breakOut )
//      }
//      (sorted, if( clumps.contains( audibleIdx )) clumpIdx else clumpIdx - 1)
//   }



//   def add( msg: osc.Message with sosc.Send, change: Option[ (FilterMode, State, Boolean) ], audible: Boolean,
//            dependencies: Map[ State, Boolean ], noError: Boolean = false ) {
//
//      logTxn( "add " + ((msg, change, audible, dependencies, noError)) )
//
//      def processDeps : Entry = {
//         dependencies.foreach { tup =>
//            val (state, _) = tup
//            if( !stateMap.contains( state )) {
//               stateMap += state -> state.get( tx )
//            }
//         }
//         val entry = Entry( entryCnt, msg, change, audible, dependencies, noError )
//         entryCnt += 1
//         entries = entries.enqueue( entry )
//         entry
//      }
//
//      change.map( tup => {
//         val (mode, state, value) = tup
//         val changed = state.get( tx ) != value
//         require( changed || (mode != RequiresChange) )
//         if( changed || (mode == Always) ) {
//            // it is important that processDeps is
//            // executed before state.set as the object
//            // might depend on a current state of its own
//            val entry = processDeps
//            entryMap += (state, value) -> entry
//            if( changed ) state.set( value )( tx )
//         }
//      }).getOrElse( processDeps )
//   }
   //   def beforeCommit( fun: Txn => Unit ) : Unit
}
