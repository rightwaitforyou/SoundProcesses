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

import de.sciss.lucre.{stm, bitemp}
import stm.{IdentifierMap, Sys, Cursor}
import bitemp.Chronos
import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import concurrent.stm.{TxnLocal, Ref, Txn}
import SoundProcesses.logConfig
import UGenGraphBuilder.MissingIn

object AuralPresentationImpl {
   def run[ S <: Sys[ S ]]( transport: ProcTransport[ S ], aural: AuralSystem )
                          ( implicit tx: S#Tx, cursor: Cursor[ S ]) : AuralPresentation[ S ] = {

      val c = new Client( cursor.position, transport, aural )
      Txn.afterCommit( _ => aural.addClient( c ))( tx.peer )
      c
   }

   private final class Client[ S <: Sys[ S ]]( csrPos: S#Acc, transportStale: ProcTransport[ S ],
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
            val transport = tx.refresh( csrPos, transportStale )
            transport.changed.react { x => println( "Aural observation: " + x )}
            if( transport.playing.value ) {
               implicit val chr: Chronos[ S ] = transport
               transport.iterator.foreach { case (_, p) => booted.procAdded( p )}
//               val it = transport.iterator
//               if( it.nonEmpty ) {
//                  val timed = it.collect({ case (_, pt) if pt.value.playing.value => pt })
//                  timed.foreach( booted.procAdded )
//               }
            }
            transport.changed.reactTx { implicit tx => {
               case Transport.Advance( tr, true, time, added, removed, params ) =>
                  implicit val chr: Chronos[ S ] = tr
//println( "AQUI: added = " + added + "; removed = " + removed )
                  removed.foreach { case (_, p)    => booted.procRemoved( p )}
                  params.foreach  { case (_, p, m) => booted.procParamsChanged( p, m )}
                  added.foreach   { case (_, p)    => booted.procAdded( p )}
//                  if( added.nonEmpty ) {
//                     val timed = added.collect { case (_, pt) if pt.value.playing.value => pt }
//                     booted.procsAdded( timed )
//                  }
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
   private final case class AuralProcBuilder[ S <: Sys[ S ]]( builder: UGenGraphBuilder[ S ],
                                                              outBuses: Map[ String, RichAudioBus ]) {
      def finish( ug: UGenGraph ) : AuralProc = ???
   }

   private final case class OngoingBuild[ S <: Sys[ S ]]( missing: Map[ MissingIn[ S ], AuralProcBuilder[ S ]] =
                                                            Map.empty[  MissingIn[ S ], AuralProcBuilder[ S ]],
                                                          incomplete: Option[ IdentifierMap[ S#ID, S#Tx, AuralProcBuilder[ S ]]] =
                                                            None )

   private final class RunningImpl[ S <: Sys[ S ]]( server: Server, viewMap: IdentifierMap[ S#ID, S#Tx, AuralProc ])
   extends AuralPresentation.Running[ S ] {
      private val ongoingBuild: TxnLocal[ OngoingBuild[ S ]] =
         TxnLocal( init = OngoingBuild() ) //  Map.empty[ MissingIn[ S ], UGenGraphBuilder[ S ]]))

//      private def getNumChannels( timed: TimedProc[ S ], key: String )( implicit tx: S#Tx ) : Int = {
//         viewMap.get( timed.id ).flatMap({ aural =>
//            implicit val ptx = ProcTxn()( tx.peer )
//            aural.getBus( key ).map( _.numChannels )
//         }).getOrElse( throw MissingIn( timed, key ))
//      }

      private def getNumChannels( timed: TimedProc[ S ], key: String )( implicit tx: S#Tx ) : Int = {
         val busOpt = viewMap.get( timed.id ) match {
            case Some( aural ) =>
               implicit val ptx = ProcTxn()( tx.peer )
               aural.getBus( key )
            case _ =>
               ongoingBuild.get( tx.peer ).incomplete.flatMap { map =>
                  map.get( timed.id ).flatMap( _.outBuses.get( key ))
               }
         }

         busOpt.map( _.numChannels ).getOrElse( throw MissingIn( timed, key ))
      }

      def scanInNumChannels( timed: TimedProc[ S ], time: Long, key: String )( implicit tx: S#Tx ) : Int = {
         timed.value.scans.valueAt( key, time ) match {
            case Some( value ) =>
               import Scan_.Value._
               value match {
                  case MonoConst( _ )                 => 1
                  case MonoSegment( _, _, _, _ )      => 1
                  case Source                         => getNumChannels( timed, key )
                  case Sink( sourceTimed, sourceKey ) => getNumChannels( sourceTimed, sourceKey )
               }
            case _ => 1 // producing a non-mapped monophonic control with default value; sounds sensible?
         }
      }

      def dispose()( implicit tx: S#Tx ) {
         viewMap.dispose()
      }

//      def procsAdded( timed: IIdxSeq[ TimedProc[ S ]])( implicit tx: S#Tx, chr: Chronos[ S ]) {
//         timed.foreach( procAdded )
//      }

      def procAdded( timed: TimedProc[ S ])( implicit tx: S#Tx, chr: Chronos[ S ]) {
         val p       = timed.value
         val playing = p.playing.value
         if( !playing ) return

         val time    = chr.time
//         val graph   = p.graph.value
//
//         val entries = Map.empty[ String, Double ] // XXX TODO p.par.entriesAt( chr.time )
//         val aural   = AuralProc( server, /* name, */ graph, entries )
//         viewMap.put( timed.id, aural )
         logConfig( "aural added " + p ) // + " -- playing? " + playing )
//         if( playing ) {
            playProc( timed, time )
//         }
      }

      private def playProc( timed: TimedProc[ S ], time: Long )( implicit tx: S#Tx ) {
         val ugb        = UGenGraphBuilder( this, timed, time )
         val isComplete = ugb.tryBuild()

         // simpler algorithm (does not allow for circular relationships):
         // - just store with the missing keys, and wait for other finished ugs to show up within the txn

         // more inquisitive algorithm:
         // - register scanIns
         // - register scanOuts
         // - register missingScanIns
         // - look up (one, arbitrary) missingScanIn for any of the scanOuts
         // - if found, continue building

         implicit val itx     = tx.peer
         val incrMissing      = ugb.missingIns
         val ongoing          = ongoingBuild()
         val oldMiss          = ongoing.missing
         val (retryE, keep)   = oldMiss.partition { case (miss, _) => incrMissing.contains( miss )}
         val retry: Set[ AuralProcBuilder[ S ]] = retryE.map( _._2 )( breakOut )

         val apb = AuralProcBuilder( ugb, Map.empty )

         val newMiss    = keep ++ incrMissing.map( _ -> apb )
         val newOngoing = ongoing.copy( missing = newMiss )

         retry.foreach { ugbRet =>

         }

         ???

//         implicit val ptx = ProcTxn()( tx.peer )
//         aural.play()
//            actions.transform( _.addPlay( p ))
      }

      def procRemoved( timed: TimedProc[ S ])( implicit tx: S#Tx ) {
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

      def procPlayingChanged( timed: TimedProc[ S ], newPlaying: Boolean )( implicit tx: S#Tx ) {
         viewMap.get( timed.id ) match {
            case Some( aural ) =>
               implicit val ptx = ProcTxn()( tx.peer )
               logConfig( "aural playing " + timed.value + " -- " + newPlaying )
               aural.playing_=( newPlaying )
            case _ =>
               println( "WARNING: could not find view for proc " + timed.value )
         }
      }

      def procGraphChanged( timed: TimedProc[ S ], newGraph: SynthGraph )( implicit tx: S#Tx ) {
         viewMap.get( timed.id ) match {
            case Some( aural ) =>
               implicit val ptx = ProcTxn()( tx.peer )
               logConfig( "aural graph changed " + timed.value )
               aural.graph_=( newGraph )
            case _ =>
               println( "WARNING: could not find view for proc " + timed.value )
         }
      }

      def procParamsChanged( timed: TimedProc[ S ], changes: Map[ String, Param ])( implicit tx: S#Tx ) {
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
