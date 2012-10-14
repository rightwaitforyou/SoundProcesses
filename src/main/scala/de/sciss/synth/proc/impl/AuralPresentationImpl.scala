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
import evt.Sys
import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import concurrent.stm.{InTxn, TxnLocal, Txn}
import SoundProcesses.logConfig
import UGenGraphBuilder.MissingIn
import graph.scan

object AuralPresentationImpl {
   def run[ S <: Sys[ S ]]( transport: ProcTransport[ S ], aural: AuralSystem )
                          ( implicit tx: S#Tx, cursor: Cursor[ S ]) : AuralPresentation[ S ] = {

      val c = new Client( /* cursor.position, */ transport, aural )
      Txn.afterCommit( _ => aural.addClient( c ))( tx.peer )
      c
   }

   private final class Client[ S <: Sys[ S ]]( /* csrPos: S#Acc, */ transport: ProcTransport[ S ],
                                               aural: AuralSystem )( implicit cursor: Cursor[ S ])
   extends AuralPresentation[ S ] with AuralSystem.Client {

      override def toString = "AuralPresentation@" + hashCode.toHexString

      private val sync     = new AnyRef
      private var running  = Option.empty[ RunningImpl[ S ]]

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
            val booted  = new RunningImpl( server, viewMap )
            ProcDemiurg.addServer( server )( ProcTxn()( tx.peer ))
            transport.react { x => println( "Aural observation: " + x )}
            if( transport.isPlaying ) {
               implicit val chr: Chronos[ S ] = transport
               transport.iterator.foreach { case (_, p) => booted.procAdded( p )}
            }
            transport.reactTx { implicit tx => {
               case Transport.Advance( tr, isSeek, true, time, added, removed, changes ) =>
                  implicit val chr: Chronos[ S ] = tr
//println( "AQUI: added = " + added + "; removed = " + removed )
                  removed.foreach { timed             => booted.procRemoved( timed )}
                  changes.foreach { case (timed, m)   => booted.procUpdated( timed, m )}
                  added.foreach   { timed             => booted.procAdded( timed )}
               case _ =>
            }}
            booted
         }
         sync.synchronized( running = Some( impl ))
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
   private final class AuralProcBuilder[ S <: Sys[ S ]]( val ugen: UGenGraphBuilder[ S ]) {
      var outBuses: Map[ String, RichAudioBus ] = Map.empty
//      def finish : AuralProc = {
//         val ug = ugen.finish
//         AuralProc()
//      }

      def id: S#ID = ugen.timed.id
   }

   /*
    * @param missingMap maps each missing input to a set of builders who requested that input
    * @param idMap      map's timed-ids to aural proc builders
    * @param seq        sequence of builders in the current transaction
    */
   private final case class OngoingBuild[ S <: Sys[ S ]]( var missingMap: Map[ MissingIn[ S ], Set[ AuralProcBuilder[ S ]]] =
                                                            Map.empty[  MissingIn[ S ], Set[ AuralProcBuilder[ S ]]],
                                                          var idMap: Option[ IdentifierMap[ S#ID, S#Tx, AuralProcBuilder[ S ]]] =
                                                            None,
                                                          var seq: IIdxSeq[ AuralProcBuilder[ S ]] = IIdxSeq.empty )

   private final class RunningImpl[ S <: Sys[ S ]]( server: Server, viewMap: IdentifierMap[ S#ID, S#Tx, AuralProc ])
   extends AuralPresentation.Running[ S ] {
      // ongoingBuild is a transaction local field storing a mutable object. This is
      // totally fine since a transaction is not shared across threads. the idea is
      // that a fresh object is retrieved for each new transaction. If it is touched,
      // a beforeCommit hook is invoked before the transaction successfully completes,
      // building the unfinished aural procs if possible.
      private val ongoingBuild: TxnLocal[ OngoingBuild[ S ]] =
         TxnLocal( init = OngoingBuild(), beforeCommit = beforeCommit ) //  Map.empty[ MissingIn[ S ], UGenGraphBuilder[ S ]]))

//      private def getNumChannels( timed: TimedProc[ S ], key: String )( implicit tx: S#Tx ) : Int = {
//         viewMap.get( timed.id ).flatMap({ aural =>
//            implicit val ptx = ProcTxn()( tx.peer )
//            aural.getBus( key ).map( _.numChannels )
//         }).getOrElse( throw MissingIn( timed, key ))
//      }

      private def beforeCommit( itx: InTxn ) {
         ongoingBuild.get( itx ).seq.foreach { builder =>
            val ugen          = builder.ugen
            if( ugen.isComplete ) {
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

               ugen.scanIns.foreach { case (key, numCh) =>
                  import Grapheme.Value._

                  def ensureChannels( n: Int ) {
                     require( n == numCh, "Scan input changed number of channels (expected " + numCh + " but found " + n + ")" )
                  }

                  def makeBusMapper( t: TimedProc[ S ], k: String ) {
                     val bus = getBus( t, k ).getOrElse( // ... or could just stick with the default control value
                        sys.error( "Bus disappeared " + t.value + " -> " + k ))
                     ensureChannels( bus.numChannels )  // ... or could insert a channel coercing synth
                     val bm = BusNodeSetter.mapper( scan.inControlName( key /* ! not k */ ), bus, synth )
                     bm.add()
                     busUsers :+= bm
                  }

//                  timed.value.graphemes.get( key ).flatMap( _.valueAt( time )).foreach {   // if not found, stick with default
//                     case const: Const =>
//                        ensureChannels( const.numChannels )  // ... or could just adjust to the fact that they changed
//                        setMap :+= ((key -> const.numChannels) : ControlSetMap)
//
//                     case segm: Segment =>
//                        ensureChannels( segm.numChannels )  // ... or could just adjust to the fact that they changed
//                        ??? // MonoSegmentWriter
//
//                     case audio: Audio =>
//                        ??? // AudioFileWriter
//
////                     case Source                         => makeBusMapper( timed,       key       )
////                     case Sink( sourceTimed, sourceKey ) => makeBusMapper( sourceTimed, sourceKey )
//                  }

                  ??? // need to look at time.value.scans instead
               }

               // ---- handle output buses ----
               val outBuses      = builder.outBuses
               builder.outBuses.foreach { case (key, bus) =>
                  val bw = BusNodeSetter.writer( scan.outControlName( key ), bus, synth )
                  bw.add()
                  busUsers :+= bw
               }

               // wrap as AuralProc and save it in the identifier map for later lookup
               val aural = AuralProc( synth, outBuses, busUsers )
               viewMap.put( builder.id, aural )

            } else {
               println( "Warning: Incomplete aural proc build for " + ugen.timed.value )
            }
         }
      }

      private def getBus( timed: TimedProc[ S ], key: String )( implicit tx: S#Tx ) : Option[ RichAudioBus ] = {
         viewMap.get( timed.id ) match {
            case Some( aural ) =>
               implicit val ptx = ProcTxn()( tx.peer )
               aural.getBus( key )
            case _ =>
               ongoingBuild.get( tx.peer ).idMap.flatMap { map =>
                  map.get( timed.id ).flatMap( _.outBuses.get( key ))
               }
         }
      }

//      private def getBusNumChannels( timed: TimedProc[ S ], key: String )( implicit tx: S#Tx ) : Int = {
//         val bus = getBus( timed, key ).getOrElse( throw MissingIn( timed, key ))
//         bus.numChannels
//      }

      def scanInNumChannels( timed: TimedProc[ S ], time: Long, key: String )( implicit tx: S#Tx ) : Int = {
         // XXX TODO: since we are only interested in the number of channels at this point,
         // we might add a more efficient method to `Scans`
         timed.value.graphemes.get( key ).flatMap( _.valueAt( time )) match {
            case Some( value )   => value.numChannels
            case _               => 1 // producing a non-mapped monophonic control with default value; sounds sensible?
         }
      }

      def dispose()( implicit tx: S#Tx ) {
         viewMap.dispose()
      }

      def procAdded( timed: TimedProc[ S ])( implicit tx: S#Tx, chr: Chronos[ S ]) {
         val p       = timed.value
         val time    = chr.time
         logConfig( "aural added " + p ) // + " -- playing? " + playing )
         buildAuralProc( timed, time )
      }

      private def buildAuralProc( timed: TimedProc[ S ], time: Long )( implicit tx: S#Tx ) {
         val ugen      = UGenGraphBuilder( this, timed, time )
         val builder   = new AuralProcBuilder( ugen )
         val ongoing   = ongoingBuild.get( tx.peer )
         ongoing.seq :+= builder
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
         ugen.tryBuild()

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

         implicit val itx     = tx.peer
//         val oldOngoing       = ongoingBuild()
//         // if the ugen build completed, add it to the complete set
//         val newOngoing0 = if( ugen.isComplete ) {
//            oldOngoing.copy( seq = oldOngoing.seq :+ builder )
//         } else oldOngoing
//val newOngoing0 = ongoingBuild()

         // initialise the id-to-builder map if necessary
         val builderMap = ongoing.idMap.getOrElse {
            val m = tx.newInMemoryIDMap[ AuralProcBuilder[ S ]]
            ongoing.idMap = Some( m )
            m
         }
         // add the (possibly new) builder to it. redundant overwrites do not cause problems
         builderMap.put( ugen.timed.id, builder )

         // if the last iteration did not complete the build process, store the missing in keys
         // (since missingMap is a map and the values are sets, it is safe to re-add existing entries)
         if( !ugen.isComplete ) {
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
         val id = timed.id
         viewMap.get( id ) match {
            case Some( aural ) =>
               viewMap.remove( id )
               implicit val ptx = ProcTxn()( tx.peer )
               logConfig( "aural removed " + timed.value ) // + " -- playing? " + aural.playing )
               aural.stop()

            case _ =>
               println( "WARNING: could not find view for proc " + timed.value )
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
