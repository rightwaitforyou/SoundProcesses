/*
 *  SegmentWriter.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2013 Hanns Holger Rutz. All rights reserved.
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

package de.sciss
package synth
package proc
package impl

import concurrent.stm

final class SegmentWriter( segm: Grapheme.Segment.Curve, val bus: RichAudioBus, aural: AuralProc, sampleRate: Double )
extends DynamicAudioBusUser with RichAudioBus.User with TxnPlayer {
   private val synthRef = stm.Ref( Option.empty[ Synth ])

   protected def synth( implicit tx: Txn ) : Option[ Synth ] = synthRef.get( tx.peer )
   protected def synth_=( rso: Option[ Synth ])( implicit tx: Txn ) {
      val oldSynth = synthRef.swap( rso )( tx.peer )
      rso.foreach( addMapBusConsumer )
      oldSynth.foreach( _.free( audible = true ))
   }

   protected def addMapBusConsumer( rs: Synth )( implicit tx: Txn ) {
//         val rb = mapBus
//         rs.write( rb -> "$out" )
      rs.write( bus -> "$out" )
   }

   protected def graph = SynthGraph {
      import ugen._

      val start   = "$start".ir
      val stop    = "$stop".ir
      val dur     = "$dur".ir
      val sig: GE = segm.values.map { case (segmStart, segmStop, segmShape) =>
         segmShape match {
            case `linShape` =>
               Line.ar( start, stop, dur, doneAction = freeSelf )
            case `expShape` =>
               if( segmStart != 0f && segmStop != 0f && segmStart * segmStop > 0f ) {
                  XLine.ar( start, stop, dur, doneAction = freeSelf )
               } else {
                  Line.ar( 0, 0, dur, doneAction = freeSelf )
               }
            case _ =>
               val env = Env( start, Env.Seg( dur = dur, targetLevel = stop,
                                              shape = varShape( "$shape".ir, "$curve".ir( 0 ))) :: Nil )
               EnvGen.ar( env, doneAction = freeSelf )

         }
      }
      Out.ar( "$out".kr, sig )
   }

   // ---- TxnPlayer ----

   def play( implicit tx: Txn ) {
      ???
//         type Ctl = List[ ControlSetMap ]
//
//         val g          = graph
//         val rsd        = SynthDef( aural.server, g )
//         val durSecs    = segm.span.length * sampleRate
//         val ctl0: Ctl  = List( "$start" -> seg.start, "$stop" -> seg.stop, "$dur" -> durSecs )
//         val shp        = seg.shape
//         val ctl1: Ctl  = if( shp != linShape && shp != expShape ) ("$shape" -> seg.shape.id) :: ctl0 else ctl0
//         val ctl: Ctl   = if( shp.curvature != 0f ) ("$curve" -> shp.curvature) :: ctl1 else ctl1
//         val rs         = rsd.play( aural.preGroup, ctl )
//
//         synth_=( Some( rs ))
//
////         rs.onEndTxn { implicit tx =>
////            synth.foreach( rs2 => if( rs == rs2 ) {
////               ctrl.glidingDone
////            })
////         }
   }

   def stop( implicit tx: Txn ) {
      synthRef.swap( None )( tx.peer ).foreach( _.free( audible = true ))
   }

   def isPlaying( implicit tx: Txn ) : Boolean = synth.map( _.isOnline ).getOrElse( false )

   // ---- RichAudioBus.User ----

   def busChanged( bus: AudioBus )( implicit tx: Txn ) {
      ???
   }

   // ---- DynamicAudioBusUser ----

   def add()( implicit tx: Txn ) {
      bus.addWriter( this )
   }

   def remove()( implicit tx: Txn ) {
      bus.removeWriter( this )
   }

   def migrateTo( newBus: RichAudioBus )( implicit tx: Txn ) : DynamicAudioBusUser = {
      ???
   }
}
