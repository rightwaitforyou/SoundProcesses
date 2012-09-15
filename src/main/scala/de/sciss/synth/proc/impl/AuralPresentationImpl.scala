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
import bitemp.{BiGroup, Chronos}
import collection.immutable.{IndexedSeq => IIdxSeq}
import SoundProcesses.logConfig
import concurrent.stm.{Ref, Txn}

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
//               transport.iterator.foreach { case (_, p) => booted.procAdded( p )}
               val it = transport.iterator
               if( it.nonEmpty ) {
                  val timed = it.collect({ case (_, pt) if pt.value.playing.value => pt })
                  booted.procsAdded( timed.toIndexedSeq )
               }
            }
            transport.changed.reactTx { implicit tx => {
               case Transport.Advance( tr, true, time, added, removed, params ) =>
                  implicit val chr: Chronos[ S ] = tr
//println( "AQUI: added = " + added + "; removed = " + removed )
                  removed.foreach { case (_, p)    => booted.procRemoved( p )}
                  params.foreach  { case (_, p, m) => booted.procParamsChanged( p, m )}
                  // added.foreach   { case (_, p)    => booted.procAdded( p )}
                  if( added.nonEmpty ) {
                     val timed = added.collect { case (_, pt) if pt.value.playing.value => pt }
                     booted.procsAdded( timed )
                  }
               case _ =>
            }}
            booted
         }
         sync.synchronized( running = Some( impl ))
      }
   }

   sealed trait Running[ S <: Sys[ S ]] {
//      def addScanIn(  proc: Proc[ S ], time: Long, key: String )( implicit tx: S#Tx ) : Int
//      def addScanOut( proc: Proc[ S ], time: Long, key: String, numChannels: Int )( implicit tx: S#Tx ) : Unit
      def scanInValue( proc: Proc[ S ], time: Long, key: String )( implicit tx: S#Tx ) : Option[ Scan_.Value[ S ]]
   }

   private final class MonoSegmentWriter( seg: Scan_.Value.MonoSegment[ _ ], val bus: RichAudioBus, aural: AuralProc )
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
         val ctl0: Ctl  = List( "$start" -> seg.start, "$stop" -> seg.stop, "$dur" -> seg.dur )
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

   private final class RunningImpl[ S <: Sys[ S ]]( server: Server, viewMap: IdentifierMap[ S#ID, S#Tx, AuralProc ])
   extends Running[ S ] {
      def scanInValue( proc: Proc[ S ], time: Long, key: String )( implicit tx: S#Tx ) : Option[ Scan_.Value[ S ]] = {
         ???
      }

//      def addScanIn( proc: Proc[ S ], time: Long, key: String )( implicit tx: S#Tx ) : Int = {
//         proc.scans.valueAt( key, time ) match {
//            case Some( value ) =>
//               value match {
//                  case Scan_.Value.MonoConst( _ ) => 1
//                  case seg @ Scan_.Value.MonoSegment( _, _, _, _ ) =>
//                     val aural = viewMap.getOrElse( proc.id, sys.error( "Missing aural view of process " + proc ))
//                     implicit val procTxn = ProcTxn()( tx.peer )
//                     val bus = aural.getBus( key ).getOrElse {
//                        val _bus = RichBus.audio( server, 1 )
//                        aural.setBus( key, Some( _bus ))
//                        _bus
//                     }
//                     val user: RichAudioBus.User = ???
//                     bus.addReader( user )
//                     1
//
//                  case Scan_.Value.Synthesis( sourceProc ) =>
//                     val sourceAural = viewMap.getOrElse( sourceProc.id, sys.error( "Missing aural view of process " + sourceProc ))
//
//                     ???
//               }
//            case _ => -1   // special result: no value found, use default
//         }
////         throw new MissingInfo
//      }

//      def addScanOut( proc: Proc[ S ], time: Long, key: String, numChannels: Int )( implicit tx: S#Tx ) {
//
//         ???
//      }

      def dispose()( implicit tx: S#Tx ) {
         viewMap.dispose()
      }

      def procsAdded( timed: IIdxSeq[ BiGroup.TimedElem[ S, Proc[ S ]]])( implicit tx: S#Tx, chr: Chronos[ S ]) {
         timed.foreach { pt =>
//         val time    = chr.time
            val p       = pt.value
            val graph   = p.graph.value
   //         val scanMap = pg.scans
   //         if( scanMap.nonEmpty ) {
   //            val scans   = p.scans
   //            scanMap.map { case (key, dir) =>
   //               scans.get( key ).map { scan =>
   //                  scan.intersect( time ).headOption.map { case (startEx, elem) =>
   //                     elem match {
   //                        case Scan_.Mono( levelExpr, shape ) =>
   //                          val level = levelExpr.value
   //                     }
   //                  }
   //               }
   //            }
   //         }

            val entries = Map.empty[ String, Double ] // XXX TODO p.par.entriesAt( chr.time )
            val aural   = AuralProc( server, /* name, */ graph, entries )
            viewMap.put( pt.id, aural )
            val playing = p.playing.value
            logConfig( "aural added " + p + " -- playing? " + playing )
            if( playing ) {
               implicit val ptx = ProcTxn()( tx.peer )
               aural.play()
   //            actions.transform( _.addPlay( p ))
            }
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
               aural.playing_=( newPlaying )
            case _ =>
               println( "WARNING: could not find view for proc " + timed.value )
         }
      }

      def procGraphChanged( timed: BiGroup.TimedElem[ S, Proc[ S ]], newGraph: SynthGraph )( implicit tx: S#Tx ) {
         viewMap.get( timed.id ) match {
            case Some( aural ) =>
               implicit val ptx = ProcTxn()( tx.peer )
               logConfig( "aural graph changed " + timed.value )
               aural.graph_=( newGraph )
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
