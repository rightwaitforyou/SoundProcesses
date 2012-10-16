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

package de.sciss.synth
package proc
package impl

import de.sciss.lucre.{stm, bitemp, event => evt}
import stm.{IdentifierMap, Cursor}
import bitemp.Chronos
import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import concurrent.stm.{InTxn, TxnLocal, Txn}
import SoundProcesses.{logAural => log}
import UGenGraphBuilder.MissingIn
import graph.scan

object AuralPresentationImpl {
   def run[ S <: evt.Sys[ S ], I <: stm.Sys[ I ]]( transport: ProcTransport[ S ], aural: AuralSystem[ S ])
                          ( implicit tx: S#Tx, bridge: S#Tx => I#Tx, cursor: Cursor[ S ]) : AuralPresentation[ S ] = {

      val dummy = DummySerializerFactory[ I ]
      import dummy._
      implicit val itx: I#Tx  = tx
      val id                  = itx.newID()
      val running             = itx.newVar[ Option[ RunningImpl[ S ]]]( id, None )
      val c                   = new Client[ S, I ]( running, transport, aural )
      aural.addClient( c )
      c
   }

   private final class Client[ S <: evt.Sys[ S ], I <: stm.Sys[ I ]]( running: I#Var[ Option[ RunningImpl[ S ]]],
                                                                      transport: ProcTransport[ S ],
                                                                      aural: AuralSystem[ S ])
                                                                    ( implicit cursor: Cursor[ S ], bridge: S#Tx => I#Tx )
   extends AuralPresentation[ S ] with AuralSystem.Client[ S ] {

      override def toString = "AuralPresentation@" + hashCode.toHexString

      def dispose()( implicit tx: S#Tx ) {
         // XXX TODO dispose running
      }

      def stopped()( implicit tx: S#Tx ) {
         implicit val itx: I#Tx = tx
         aural.removeClient( this )
         running.get.foreach { impl =>
            // XXX TODO dispose
         }
         running.set( None )
      }

      def started( server: Server )( implicit tx: S#Tx ) {
         implicit val itx: I#Tx = tx
         log( "started" )

         val viewMap: IdentifierMap[ S#ID, S#Tx, AuralProc ]      = tx.newInMemoryIDMap
         val scanMap: IdentifierMap[ S#ID, S#Tx, (String, S#ID) ] = tx.newInMemoryIDMap

         val booted  = new RunningImpl( server, viewMap, scanMap )
         ProcDemiurg.addServer( server )( ProcTxn()( tx.peer ))
//            transport.react { x => println( "Aural observation: " + x )}

         def t_play( time: Long )( implicit tx: S#Tx ) {
            transport.iterator.foreach { case (_, timed) => booted.procAdded( time, timed )}
         }

         def t_stop( time: Long )( implicit tx: S#Tx ) {
            transport.iterator.foreach { case (_, timed) => booted.procRemoved( timed )}
         }

         if( transport.isPlaying ) t_play( transport.time )

         transport.reactTx { implicit tx => {
            // only when playing
            case Transport.Advance( tr, time, isSeek, true, added, removed, changes ) =>
               log( "at " + time + " added " + added.mkString(   "[", ", ", "]" ) +
                                "; removed " + removed.mkString( "[", ", ", "]" ) +
                                "; changes? " + changes.nonEmpty )
               removed.foreach { timed             => booted.procRemoved( timed )}
               changes.foreach { case (timed, m)   => booted.procUpdated( timed, m )}
               added.foreach   { timed             => booted.procAdded( time, timed )}

            case Transport.Play( tr, time ) => t_play( time )
            case Transport.Stop( tr, time ) => t_stop( time )

            case _ =>
//                  log( "other " + other )
         }}

         running.set( Some( booted ))
      }
   }

//   sealed trait Running[ S <: Sys[ S ]] {
////      def addScanIn(  proc: Proc[ S ], time: Long, key: String )( implicit tx: S#Tx ) : Int
////      def addScanOut( proc: Proc[ S ], time: Long, key: String, numChannels: Int )( implicit tx: S#Tx ) : Unit
//      def scanInValue( proc: Proc[ S ], time: Long, key: String )( implicit tx: S#Tx ) : Option[ Scan_.Value[ S ]]
//   }

/*
   private final class MonoSegmentWriter( seg: Scan_.Value.MonoSegment, val bus: RichAudioBus, aural: AuralProc, sampleRate: Double )
   extends DynamicAudioBusUser with RichAudioBus.User with TxnPlayer {
      private val synthRef: Ref[ Option[ RichSynth ]] = ???

      protected def synth( implicit tx: ProcTxn ) : Option[ RichSynth ] = synthRef.get( tx.peer )
      protected def synth_=( rso: Option[ RichSynth ])( implicit tx: ProcTxn ) {
         val oldSynth = synthRef.swap( rso )( tx.peer )
         rso.foreach( addMapBusConsumer )
         oldSynth.foreach( _.free( audible = true ))
      }

      protected def addMapBusConsumer( rs: RichSynth )( implicit tx: ProcTxn ) {
//         val rb = mapBus
//         rs.write( rb -> "$out" )
         rs.write( bus -> "$out" )
      }

      protected def graph = SynthGraph {
         import ugen._

         val start   = "$start".ir
         val stop    = "$stop".ir
         val dur     = "$dur".ir
         val sig = seg.shape match {
            case `linShape` =>
               Line.ar( start, stop, dur, doneAction = freeSelf )
            case `expShape` =>
               if( seg.start != 0f && seg.stop != 0f && seg.start * seg.stop > 0f ) {
                  XLine.ar( start, stop, dur, doneAction = freeSelf )
               } else {
                  Line.ar( 0, 0, dur, doneAction = freeSelf )
               }
            case _ =>
               val env = Env( start, Env.Seg( dur = dur, targetLevel = stop,
                                              shape = varShape( "$shape".ir, "$curve".ir( 0 ))) :: Nil )
               EnvGen.ar( env, doneAction = freeSelf )

         }
         Out.ar( "$out".kr, sig )
      }

      // ---- TxnPlayer ----

      def play( implicit tx: ProcTxn ) {
         type Ctl = List[ ControlSetMap ]

         val g          = graph
         val rsd        = RichSynthDef( aural.server, g )
         val durSecs    = seg.dur * sampleRate
         val ctl0: Ctl  = List( "$start" -> seg.start, "$stop" -> seg.stop, "$dur" -> durSecs )
         val shp        = seg.shape
         val ctl1: Ctl  = if( shp != linShape && shp != expShape ) ("$shape" -> seg.shape.id) :: ctl0 else ctl0
         val ctl: Ctl   = if( shp.curvature != 0f ) ("$curve" -> shp.curvature) :: ctl1 else ctl1
         val rs         = rsd.play( aural.preGroup, ctl )

         synth_=( Some( rs ))

//         rs.onEndTxn { implicit tx =>
//            synth.foreach( rs2 => if( rs == rs2 ) {
//               ctrl.glidingDone
//            })
//         }
      }

      def stop( implicit tx: ProcTxn ) {
         synthRef.swap( None )( tx.peer ).foreach( _.free( audible = true ))
      }

      def isPlaying( implicit tx: ProcTxn ) : Boolean = synth.map( _.isOnline.get ).getOrElse( false )

      // ---- RichAudioBus.User ----

      def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) {
         ???
      }

      // ---- DynamicAudioBusUser ----

      def add( implicit tx: ProcTxn ) {
         bus.addWriter( this )
      }

      def remove( implicit tx: ProcTxn ) {
         bus.removeWriter( this )
      }

      def migrateTo( newBus: RichAudioBus )( implicit tx: ProcTxn ) : DynamicAudioBusUser = {
         ???
      }
   }
*/
   private final class AuralProcBuilder[ S <: evt.Sys[ S ]]( val ugen: UGenGraphBuilder[ S ]) {
      var outBuses: Map[ String, RichAudioBus ] = Map.empty
//      def finish : AuralProc = {
//         val ug = ugen.finish
//         AuralProc()
//      }

//      def id: S#ID = ugen.timed.id
   }

   /*
    * @param missingMap maps each missing input to a set of builders who requested that input
    * @param idMap      map's timed-ids to aural proc builders
    * @param seq        sequence of builders in the current transaction
    */
   private final case class OngoingBuild[ S <: evt.Sys[ S ]]( var missingMap: Map[ MissingIn[ S ], Set[ AuralProcBuilder[ S ]]] =
                                                            Map.empty[ MissingIn[ S ], Set[ AuralProcBuilder[ S ]]],
                                                          var idMap: Option[ IdentifierMap[ S#ID, S#Tx, AuralProcBuilder[ S ]]] =
                                                            None,
                                                          var seq: IIdxSeq[ AuralProcBuilder[ S ]] = IIdxSeq.empty )

   private final class RunningImpl[ S <: evt.Sys[ S ]]( server: Server, viewMap: IdentifierMap[ S#ID, S#Tx, AuralProc ],
                                                    scanMap: IdentifierMap[ S#ID, S#Tx, (String, S#ID) ])
   extends AuralPresentation.Running[ S ] {
      // ongoingBuild is a transaction local field storing a mutable object. This is
      // totally fine since a transaction is not shared across threads. the idea is
      // that a fresh object is retrieved for each new transaction. If it is touched,
      // a beforeCommit hook is invoked before the transaction successfully completes,
      // building the unfinished aural procs if possible.
//      private val ongoingBuild: TxnLocal[ OngoingBuild[ S ]] =
//         TxnLocal( init = OngoingBuild(), beforeCommit = beforeCommit )

      private val ongoingBuild: TxnLocal[ OngoingBuild[ S ]] =
         TxnLocal( initialValue = { itx =>
            ProcTxn()( itx ).beforeCommit( flush )
            OngoingBuild()
         })

//      private def getNumChannels( timed: TimedProc[ S ], key: String )( implicit tx: S#Tx ) : Int = {
//         viewMap.get( timed.id ).flatMap({ aural =>
//            implicit val ptx = ProcTxn()( tx.peer )
//            aural.getBus( key ).map( _.numChannels )
//         }).getOrElse( throw MissingIn( timed, key ))
//      }

      private def launchProc( builder: AuralProcBuilder[ S ]) {
         val ugen          = builder.ugen
         // finalise the ugen graph
         val ug            = ugen.finish
         implicit val tx   = ugen.tx
         implicit val itx  = tx.peer
         implicit val ptx  = ProcTxn()
         // get a rich synth def and synth playing just in the default group
         // (we'll have additional messages moving the group into place if needed,
         // as well as setting and mapping controls)
         val df            = ProcDemiurg.getSynthDef( server, ug ) // RichSynthDef()
         val synth         = df.play( target = RichGroup.default( server ), addAction = addToHead )

         // ---- handle input buses ----
         val time       = ugen.time
         val timed      = ugen.timed
         var setMap     = IIdxSeq.empty[ ControlSetMap ]
         var busUsers   = IIdxSeq.empty[ DynamicBusUser ]
         val p          = timed.value

         import Grapheme.Segment

         ugen.scanIns.foreach { case (key, numCh) =>

            def ensureChannels( n: Int ) {
               require( n == numCh, "Scan input changed number of channels (expected " + numCh + " but found " + n + ")" )
            }

            def makeBusMapper( timedID: S#ID, key: String ) {
               val bus = getBus( timedID, key ).getOrElse( // ... or could just stick with the default control value
                  sys.error( "Bus disappeared " + timedID + " -> " + key ))
               ensureChannels( bus.numChannels )  // ... or could insert a channel coercing synth
               val bm = BusNodeSetter.mapper( scan.inControlName( key /* ! not k */ ), bus, synth )
               bm.add()
               busUsers :+= bm
            }

            val sourceOpt = p.scans.get( key ).flatMap( _.source )
            sourceOpt.foreach {   // if not found, stick with default
               case Scan.Link.Grapheme( peer ) =>
                  val segmOpt = peer.segment( time )
                  segmOpt.foreach { // again if not found... stick with default
                     case const: Segment.Const =>
                        ensureChannels( const.numChannels )  // ... or could just adjust to the fact that they changed
//                        setMap :+= ((key -> const.numChannels) : ControlSetMap)
                        val cKey = scan.inControlName( key )
                        setMap :+= (if( const.numChannels == 1 ) {
                           ControlSetMap.Single( cKey, const.values.head.toFloat )
                        } else {
                           ControlSetMap.Multi( cKey, const.values.map( _.toFloat ))
                        })

                     case segm: Segment.Curve =>
                        ensureChannels( segm.numChannels )  // ... or could just adjust to the fact that they changed
                        ??? // MonoSegmentWriter

                     case audio: Segment.Audio =>
                        ??? // AudioFileWriter
                  }
               case Scan.Link.Scan( peer ) =>
                  scanMap.get( peer.id ).foreach { case (sourceKey, sourceTimedID) =>
                     makeBusMapper( sourceTimedID, sourceKey )
                  }
            }
         }

         // ---- handle output buses ----
         val outBuses = builder.outBuses
         builder.outBuses.foreach { case (key, bus) =>
            val bw = BusNodeSetter.writer( scan.outControlName( key ), bus, synth )
            bw.add()
            busUsers :+= bw
         }

         // wrap as AuralProc and save it in the identifier map for later lookup
         val aural = AuralProc( synth, outBuses, busUsers )
         if( setMap.nonEmpty ) synth.set( audible = true, setMap: _* )
         log( "launched " + aural )
         viewMap.put( timed.id, aural )

      }

      // called before the transaction successfully completes.
      // this is the place where we launch completely built procs.
      private def flush( ptx: ProcTxn ) {
         val itx = ptx.peer
         ongoingBuild.get( itx ).seq.foreach { builder =>
            val ugen = builder.ugen
            if( ugen.isComplete ) {
               launchProc( builder )
            } else {
               // XXX TODO: do we need to free buses associated with ugen.scanOuts ?
               println( "Warning: Incomplete aural proc build for " + ugen.timed.value )
            }
         }
      }

      private def getBus( timedID: S#ID, key: String )( implicit tx: S#Tx ) : Option[ RichAudioBus ] = {
         viewMap.get( timedID ) match {
            case Some( aural ) =>
               implicit val ptx = ProcTxn()( tx.peer )
               aural.getBus( key )
            case _ =>
               ongoingBuild.get( tx.peer ).idMap.flatMap { map =>
                  map.get( timedID ).flatMap( _.outBuses.get( key ))
               }
         }
      }

//      private def getBusNumChannels( timed: TimedProc[ S ], key: String )( implicit tx: S#Tx ) : Int = {
//         val bus = getBus( timed, key ).getOrElse( throw MissingIn( timed, key ))
//         bus.numChannels
//      }

      // called by UGenGraphBuilderImpl
      def scanInNumChannels( timed: TimedProc[ S ], time: Long, key: String )( implicit tx: S#Tx ) : Int = {
         val scanOpt    = timed.value.scans.get( key )
         val sourceOpt  = scanOpt.flatMap( _.source )
         val chansOpt   = sourceOpt.flatMap {
            case Scan.Link.Grapheme( peer ) =>
               peer.valueAt( time ).map( _.numChannels )
            case Scan.Link.Scan( peer ) =>
               scanMap.get( peer.id ).flatMap { case (sourceKey, sourceTimedID) =>
                  getBus( sourceTimedID, sourceKey ).map( _.numChannels )
               }
         }
         chansOpt.getOrElse( 1 )   // producing a non-mapped monophonic control with default value; sounds sensible?
      }

      def dispose()( implicit tx: S#Tx ) {
         viewMap.dispose()
      }

      def procAdded( time: Long, timed: TimedProc[ S ])( implicit tx: S#Tx ) {
//         val name = timed.value.name.value
         log( "added " + timed + " (name = " + timed.value.name.value + ")" )

         val timedID    = timed.id
         val ugen       = UGenGraphBuilder( this, timed, time )
         val builder    = new AuralProcBuilder( ugen )
         val ongoing    = ongoingBuild.get( tx.peer )
         ongoing.seq  :+= builder

         // initialise the id-to-builder map if necessary
         val builderMap = ongoing.idMap.getOrElse {
            val m = tx.newInMemoryIDMap[ AuralProcBuilder[ S ]]
            ongoing.idMap = Some( m )
            m
         }
         // add the builder to it.
         builderMap.put( timedID, builder )

         // store the look up information for the scans
         // (this is only needed because Scan.Link.Scan reveals
         // only the Scan which in turn doesn't currently carry
         // key and proc information, so it can't be recovered
         // otherwise; in the future this may change)
         val scans   = timed.value.scans
         scans.iterator.foreach { case (key, scan) =>
            scanMap.put( scan.id, key -> timedID )
         }

         incrementalBuild( ongoing, builder )
      }

      // note: builder.outBuses will be updated by this method
      private def incrementalBuild( ongoing: OngoingBuild[ S ], builder: AuralProcBuilder[ S ])( implicit tx: S#Tx ) {
         // simpler algorithm (does not allow for circular relationships):
         // - just store with the missing keys, and wait for other finished ugs to show up within the txn

         // more inquisitive algorithm:
         // - register scanIns
         // - register scanOuts
         // - register missingScanIns
         // - look up (one, arbitrary) missingScanIn for any of the scanOuts
         // - if found, continue building

         val ugen       = builder.ugen
         val isComplete = ugen.tryBuild()

         log( "incremental " + ugen.timed + "; completed? " + isComplete )

         // detect which new scan outputs have been determined in the last iteration
         // (newOuts is a map from `name: String` to `numChannels Int`)
         val newOuts    = ugen.scanOuts.filterNot { case (key, _) => builder.outBuses.contains( key )}
         // if there were any, create rich audio buses for them, and store them in the builder's bus map.
         //    note that these buses initially do not have any real resources allocated, so it's safe to
         //    forget about them and have them gc'ed if the process does not complete by the end of the txn.
         if( newOuts.nonEmpty ) {
            val newBuses = newOuts.mapValues( numCh => RichBus.audio( server, numCh ))
            builder.outBuses ++= newBuses
         }

         // if the last iteration did not complete the build process, store the missing in keys
         // (since missingMap is a map and the values are sets, it is safe to re-add existing entries)
         if( !isComplete ) {
            var newMissing = ongoing.missingMap
            ugen.missingIns.foreach { miss =>
               newMissing += miss -> (newMissing.getOrElse( miss, Set.empty ) + builder)
            }
            ongoing.missingMap = newMissing
         }

         // if new scan outputs have been determined, find out whether other incomplete
         // processes depend on them, so that their building might be advanced
         val retry = if( newOuts.nonEmpty ) {
            // the retried entries are those whose missing scan ins contain
            // any of the newly determined scan outs
            val keys: Set[ MissingIn[ S ]] = newOuts.map({ case (key, _) => MissingIn( ugen.timed, key )})( breakOut )
            // divide missing map according to these keys
            val (retE, keep) = ongoing.missingMap.partition { case (key, _) => keys.contains( key )}
            // merge all the found builder sets together
            val ret: Set[ AuralProcBuilder[ S ]] = retE.flatMap( _._2 )( breakOut )
            // ...and update the ongoing information by removing the builders to retry from the missing map
            // (they will add themselves again in the recursive `afterIncr` call)
            ongoing.missingMap = keep
            ret

         } else {
            Set.empty[ AuralProcBuilder[ S ]]
         }

         // advance the ugen graph builder for all processes which have been selected for retry
         retry.foreach( r => incrementalBuild( ongoing, r ))
      }

      def procRemoved( timed: TimedProc[ S ])( implicit tx: S#Tx ) {
         val timedID = timed.id
         viewMap.get( timedID ) match {
            case Some( aural ) =>
               viewMap.remove( timedID )
               implicit val ptx = ProcTxn()( tx.peer )
               log( "removed " + timed ) // + " -- playing? " + aural.playing )
               aural.stop()

            case _ =>
               println( "WARNING: could not find view for proc " + timed.value )
         }


         // remove auxiliary scan map (see procAdded)
         val scans   = timed.value.scans
         scans.iterator.foreach { case (key, scan) =>
            scanMap.remove( scan.id )
         }
      }

//      def procGraphChanged( timed: TimedProc[ S ], newGraph: SynthGraph )( implicit tx: S#Tx ) {
//         viewMap.get( timed.id ) match {
//            case Some( aural ) =>
//               implicit val ptx = ProcTxn()( tx.peer )
//               logConfig( "aural graph changed " + timed.value )
//               aural.graph_=( newGraph )
//            case _ =>
//               println( "WARNING: could not find view for proc " + timed.value )
//         }
//      }

      def procUpdated( timed: TimedProc[ S ], change: Transport.Proc.Update[ S ])( implicit tx: S#Tx ) {
//         viewMap.get( timed.id ) match {
//            case Some( aural ) =>
//               implicit val ptx = ProcTxn()( tx.peer )
//               logConfig( "aural freq changed " + timed.value )
//               aural.addParams( changes )
//            case _ =>
//               println( "WARNING: could not find view for proc " + timed.value )
//         }
      }
   }
}
