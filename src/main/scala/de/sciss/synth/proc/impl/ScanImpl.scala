/*
 *  ScanImpl.scala
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

import de.sciss.lucre.{event => evt, DataInput, DataOutput, stm, data}
import data.SkipList
import stm.Sys
import evt.Event
import annotation.switch

object ScanImpl {
   import Scan.Link

   sealed trait Update[ S ]

   def apply[ S <: Sys[ S ]]( implicit tx: S#Tx ) : Scan[ S ] = {
      val targets    = evt.Targets[ S ]   // XXX TODO: partial?
      val id         = targets.id
      val sourceRef  = tx.newVar[ Option[ Link[ S ]]]( id, None )
//      val sinkSet    = SkipList.Set.empty[ S, Link[ S ]]
      val sinkSet: SkipList.Set[ S, Link[ S ]] = ???
      new ImplNew( targets, sourceRef, sinkSet )
   }

   def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Scan[ S ] = {
      serializer[ S ].read( in, access )
//      val targets    = evt.Targets.read[ S ]( in, access )
   }

   implicit def serializer[ S <: Sys[ S ]] : evt.NodeSerializer[ S, Scan[ S ]] =
      anySer.asInstanceOf[ evt.NodeSerializer[ S, Scan[ S ]]]

   private val anySer : evt.NodeSerializer[ I, Scan[ I ]] = new Ser[ I ]

//   private final class Ser[ S <: Sys[ S ]] extends evt.Reader[ S, Impl[ S ]] with stm.Serializer[ S#Tx, S#Acc, Scan[ S ]]
   private final class Ser[ S <: Sys[ S ]] extends evt.NodeSerializer[ S, Scan[ S ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Scan[ S ] = {
         val id         = targets.id
         val sourceRef  = tx.readVar[ Option[ Link[ S ]]]( id, in )
         val sinkSet: SkipList.Set[ S, Link[ S ]] = ???
         new ImplNew( targets, sourceRef, sinkSet )
      }
   }

   implicit def linkSerializer[ S <: Sys[ S ]] : stm.Serializer[ S#Tx, S#Acc, Link[ S ]] =
      anyLinkSer.asInstanceOf[ stm.Serializer[ S#Tx, S#Acc, Link[ S ]]]

   private val anyLinkSer : stm.Serializer[ I#Tx, I#Acc, Link[ I ]] = new LinkSer[ I ]

   private final class LinkSer[ S <: Sys[ S ]] extends stm.Serializer[ S#Tx, S#Acc, Link[ S ]] {
      def write( link: Link[ S ], out: DataOutput) {
         link match {
            case Link.Grapheme( peer ) =>
               out.writeUnsignedByte( 0 )
               peer.write( out )
            case Link.Scan( peer ) =>
               out.writeUnsignedByte( 1 )
               peer.write( out )
         }
      }

      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Link[ S ] = {
         (in.readUnsignedByte(): @switch) match {
            case 0 =>
               val peer = Grapheme.read[ S ]( in, access )
               Link.Grapheme( peer )
            case 1 =>
               val peer = Scan.read[ S ]( in, access )
               Link.Scan( peer )
            case cookie => sys.error( "Unexpected cookie " + cookie )
         }
      }
   }

   private abstract class Impl[ S <: Sys[ S ]]
   extends Scan[ S ]
   with evt.StandaloneLike[ S, Scan.Update[ S ], Scan[ S ]]
   with evt.Generator[ S, Scan.Update[ S ], Scan[ S ]]
   with evt.Root[ S, Scan.Update[ S ]] {
      me =>

      protected def sourceRef : S#Var[ Option[ Link[ S ]]]
      protected def sinkSet : SkipList.Set[ S, Link[ S ]]

      override def toString = "Scan" + id

      final def sinks( implicit tx: S#Tx ) : data.Iterator[ S#Tx, Link[ S ]] = sinkSet.iterator

      final def addSink( sink: Link[ S ])( implicit tx: S#Tx ) : Boolean = {
         val res = sinkSet.add( sink )
         if( res ) fire( Scan.SinkAdded( me, sink ))
         res
      }
      final def removeSink( sink: Link[ S ])( implicit tx: S#Tx ) : Boolean = {
         val res = sinkSet.remove( sink )
         if( res ) fire( Scan.SinkRemoved( me, sink ))
         res
      }

      final def source( implicit tx: S#Tx ) : Option[ Link[ S ]] = sourceRef.get

      final def source_=( link: Option[ Link[ S ]])( implicit tx: S#Tx ) {
         if( setSource( link )) {
            link match {
               case Some( Link.Scan( peer )) => peer.addSink( this )
               case _ =>
            }
         }
      }

      private def setSource( link: Option[ Link[ S ]])( implicit tx: S#Tx ) : Boolean = {
         val old = sourceRef.get
         if( old == link ) return false

         sourceRef.set( link )
         old match {
            case Some( Link.Scan( peer )) => peer.removeSink( this )
            case _ =>
         }
         fire( Scan.SourceChanged( me, link ))
         true
      }

      final def changed: Event[ S, Scan.Update[ S ], Scan[ S ]] = this

      // called in the implementation from addSink( Link.Scan( _ )). the difference
      // to source_= is that this method should not establish the opposite connection
      // by calling addSink on the source, as this would result in an infinite feedback.
      // still, this method should fire an Scan.SourceChanged event.
      private[proc] def setScanSource( source: Scan[ S ])( implicit tx: S#Tx ) {
         setSource( Some( source: Link[ S ]) )
      }

      final protected def writeData( out: DataOutput ) {
         sourceRef.write( out )
         sinkSet.write( out )
      }

      final protected def disposeData()(implicit tx: S#Tx) {
         sourceRef.dispose()
         sinkSet.dispose()
      }

      final protected def reader: evt.Reader[ S, Scan[ S ]] = serializer
   }

   private final class ImplNew[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ],
                                                protected val sourceRef: S#Var[ Option[ Link[ S ]]],
                                                protected val sinkSet: SkipList.Set[ S, Link[ S ]])
   extends Impl[ S ]
}
