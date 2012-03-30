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

import de.sciss.lucre.{DataInput, DataOutput, stm}
import stm.{InMemory, Durable, Writer}
import concurrent.stm.{InTxn, TxnLocal, Ref => ScalaRef}
import de.sciss.synth.{Server, SynthGraph, Synth}

object AuralProc {
//   implicit object Serializer extends stm.Serializer[ AuralProc ] {
//      def write( v: AuralProc, out: DataOutput ) { v.write( out )}
//      def read( in: DataInput ) : AuralProc = {
//         val name = in.readString()
//         new Impl( name )
//      }
//   }

   def apply( name: String, server: Server ) : AuralProc = {
      new Impl( name, server )
   }

   private final class Impl( val name: String, val server: Server )
   extends AuralProc {

      private val groupRef = ScalaRef( Option.empty[ RichGroup ])
//      private val synthRef = ScalaRef( Option.empty[ RichSynth ])
      private val graphRef = ScalaRef( ProcImpl.emptyGraph )

      def group( implicit tx: ProcTxn ) : Option[ RichGroup ] = groupRef.get( tx.peer )
      def graph( implicit tx: ProcTxn ) : SynthGraph          = graphRef.get( tx.peer )
      def graph_=( g: SynthGraph )( implicit tx: ProcTxn ) {
         sys.error( "TODO" )
      }

      def play()( implicit tx: ProcTxn ) {
         val gr   = graph
      }
   }
}
sealed trait AuralProc /* extends Writer */ {
   def server : Server
   def name : String
   def group( implicit tx: ProcTxn ) : Option[ RichGroup ]
   def play()( implicit tx: ProcTxn ) : Unit

   def graph( implicit tx: ProcTxn ) : SynthGraph
   def graph_=( g: SynthGraph )( implicit tx: ProcTxn ) : Unit
}
