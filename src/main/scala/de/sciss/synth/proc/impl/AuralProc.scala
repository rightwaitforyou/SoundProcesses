/*
 *  AuralProc.scala
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

import concurrent.stm.{Ref => ScalaRef}
import de.sciss.synth.{addToHead, ControlSetMap, Server, SynthGraph}

object AuralProc {
//   implicit object Serializer extends stm.Serializer[ AuralProc ] {
//      def write( v: AuralProc, out: DataOutput ) { v.write( out )}
//      def read( in: DataInput ) : AuralProc = {
//         val name = in.readString()
//         new Impl( name )
//      }
//   }

   def apply( server: Server, initName: String, initGraph: SynthGraph, initFreq: Double ) : AuralProc = {
      new Impl( server, initName, initGraph, initFreq )
   }

   private final class Impl( val server: Server, name0: String, graph0: SynthGraph, freq0: Double )
   extends AuralProc {

      private val groupRef    = ScalaRef( Option.empty[ RichGroup ])
      private val synthRef    = ScalaRef( Option.empty[ RichSynth ])
      private val nameRef     = ScalaRef( name0 )
      private val graphRef    = ScalaRef( graph0 )
//      private val synthDefRef = ScalaRef( Option.empty[ RichSynthDef ])

      private val freqRef     = ScalaRef( freq0 )

      def group( implicit tx: ProcTxn ) : Option[ RichGroup ] = groupRef.get( tx.peer )
      def graph( implicit tx: ProcTxn ) : SynthGraph          = graphRef.get( tx.peer )
      def graph_=( g: SynthGraph )( implicit tx: ProcTxn ) {
         graphRef.set( g )( tx.peer )
         if( playing ) {
            stop()
            play()
         }
      }

      def name( implicit tx: ProcTxn ) : String = nameRef.get( tx.peer )
      def name_=( n: String )( implicit tx: ProcTxn ) { nameRef.set( n )( tx.peer )}

      def play()( implicit tx: ProcTxn ) {
         val gr         = graph
         val df         = ProcDemiurg.getSynthDef( server, gr )

         val target     = RichGroup.default( server )
         val addAction  = addToHead
         val args: Seq[ ControlSetMap ] = Seq( "freq" -> freq.toFloat )
         val bufs       = Seq.empty[ RichBuffer ]

         val synth      = df.play( target, args, addAction, bufs )

         val old        = synthRef.swap( Some( synth ))( tx.peer )
         old.foreach( _.free() )
      }

      def stop()( implicit tx: ProcTxn ) {
         val synth = synthRef.swap( None )( tx.peer )
         synth.foreach( _.free() )
      }

      def playing( implicit tx: ProcTxn ) : Boolean = synthRef.get( tx.peer ).map( _.isOnline.get ).getOrElse( false )
      def playing_=( p: Boolean )( implicit tx: ProcTxn ) {
         if( p ) play() else stop()
      }

      def freq( implicit tx: ProcTxn ) : Double = freqRef.get( tx.peer )
      def freq_=( f: Double )( implicit tx: ProcTxn ) {
         implicit val itx = tx.peer
         val old = freqRef.swap( f )
         if( f != old ) {
            synthRef.get.foreach( _.set( true, "freq" -> f ))
         }
      }
   }
}
sealed trait AuralProc /* extends Writer */ {
   def server : Server
   def name( implicit tx: ProcTxn ) : String
   def name_=( n: String )( implicit tx: ProcTxn ) : Unit
   def group( implicit tx: ProcTxn ) : Option[ RichGroup ]
   def play()( implicit tx: ProcTxn ) : Unit
   def playing( implicit tx: ProcTxn ) : Boolean
   def playing_=( p: Boolean )( implicit tx: ProcTxn ) : Unit
   def stop()( implicit tx: ProcTxn ) : Unit

   def graph( implicit tx: ProcTxn ) : SynthGraph
   def graph_=( g: SynthGraph )( implicit tx: ProcTxn ) : Unit

   def freq( implicit tx: ProcTxn ) : Double
   def freq_=( f: Double )( implicit tx: ProcTxn ) : Unit
}
