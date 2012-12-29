/*
 *  scan.scala
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

package de.sciss.synth
package proc
package graph

import collection.immutable.{IndexedSeq => IIdxSeq}

object scan {
   private def outsideOfContext() = sys.error( "Expansion out of context" )

//   private[proc] sealed trait Elem {
//      def key: String
//      def dir: ProcGraph.Direction
//   }

   private[proc] def outControlName( key: String ) : String = "$out_" + key
   private[proc] def inControlName(  key: String ) : String = "$in_" + key

   private final case class In( key: String, default: Double )
   extends GE.Lazy /* with Elem */ with AudioRated {
      def displayName = "scan.In"

      override def toString = displayName + "(\"" + key + "\")"

      def makeUGens: UGenInLike = {
         UGenGraph.builder match {
            case b: UGenGraphBuilder[ _ ] =>
               val numChannels = b.addScanIn( key )
               val ctlName = inControlName( key )
               if( numChannels == 1 ) {
                  ctlName.ar( default ).expand
               } else if( numChannels > 1 ) {
                  ctlName.ar( default, IIdxSeq.fill( numChannels - 1 )( default ): _* ).expand
               } else {
                  UGenInGroup.empty
               }

            case other => outsideOfContext()
         }
      }

//      def dir: ProcGraph.Direction = ProcGraph.In
   }

//   private final case class Out( key: String, signal: GE )
//   extends UGenSource.ZeroOut( "scan.Out" ) with Elem with WritesBus {
////      def displayName = "scan.Out"
//
//      override def toString = displayName + "(\"" + key + "\")"
//
//      protected def makeUGen(args: IndexedSeq[ UGenIn ]) {}
//
//      protected def makeUGens {}
//   }

   private final case class Out( key: String, in: GE )
   extends UGenSource.ZeroOut( "Out" ) /* Lazy.Expander[ Unit ] */ /* with Elem */ with WritesBus {
//      def displayName = "scan.Out"

      override def toString = "scan.Out(\"" + key + "\")"

//      protected def makeUGens {
//         val bus  = ("$out_" + key).kr
//         val out  = ugen.Out.ar( bus, in )
//         val inEx = in.expand
//      }

      protected def makeUGens {
         val bus = outControlName( key ).kr
         unwrap( IIdxSeq( bus.expand ) ++ in.expand.outputs )
      }

      // first arg: bus control, remaining args: signal to write; thus numChannels = _args.size - 1
      protected def makeUGen( _args: IIdxSeq[ UGenIn ]) {
         val busArg        = _args.head
         val sigArg        = _args.tail
         val numChannels   = sigArg.size
         UGenGraph.builder match {
            case b: UGenGraphBuilder[ _ ] =>
               b.addScanOut( key, numChannels )
            case other => outsideOfContext()
         }
         val sigArgAr = sigArg.map { ui =>
            if( ui.rate == audio ) ui else new UGen.SingleOut( "K2A", audio, IIdxSeq( ui ))
         }
         new UGen.ZeroOut( name, audio, busArg +: sigArgAr, isIndividual = true )
      }

//      def dir: ProcGraph.Direction = ProcGraph.Out
   }
}
final case class scan( key: String ) {
//   private def add( direction: ProcGraph.Direction ) {
//      ProcGraph.builder.addScan( key, direction )
//   }

   def ar( default: Double ) : GE = scan.In( key, default )

//   def ar( default: Double ) : GE = {
//      val ctl = ("$in_" + key).ar( default )
//      add( ProcGraph.In )
//      ctl
//   }

   def :=( in: GE ) {
      scan.Out( key, in )
   }

//   def :=( in: GE ) {
//      val bus = ("$out_" + key).kr
//      Out.ar( bus, in )
//      add( ProcGraph.Out )
//   }
}