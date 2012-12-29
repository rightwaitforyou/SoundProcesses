/*
 *  ScanImpl.scala
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
package impl

import de.sciss.lucre.{event => evt, DataInput, DataOutput, stm, data, expr}
import stm.IdentifierMap
import evt.{impl => evti, Event, Sys}
import annotation.switch
import expr.LinkedList
import proc.Scan

object ScanImpl {
   import Scan.Link

   sealed trait Update[ S ]

   def apply[ S <: Sys[ S ]]( implicit tx: S#Tx ) : Scan[ S ] = {
      val targets    = evt.Targets[ S ]   // XXX TODO: partial?
      val id         = targets.id
      val sourceRef  = tx.newVar[ Option[ Link[ S ]]]( id, None )
      val sinkMap    = tx.newDurableIDMap[ Link[ S ]]
      val sinkList   = LinkedList.Modifiable[ S, Link[ S ]]
      new Impl( targets, sourceRef, sinkMap, sinkList )
   }

   def read[ S <: Sys[ S ]]( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Scan[ S ] = {
      serializer[ S ].read( in, access )
   }

   implicit def serializer[ S <: Sys[ S ]] : evt.NodeSerializer[ S, Scan[ S ]] =
      anySer.asInstanceOf[ evt.NodeSerializer[ S, Scan[ S ]]]

   private type I = evt.InMemory

   private val anySer : evt.NodeSerializer[ I, Scan[ I ]] = new Ser[ I ]

   private final class Ser[ S <: Sys[ S ]] extends evt.NodeSerializer[ S, Scan[ S ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Scan[ S ] = {
         val id         = targets.id
         val sourceRef  = tx.readVar[ Option[ Link[ S ]]]( id, in )
         val sinkMap    = tx.readDurableIDMap[ Link[ S ]]( in )
         val sinkList   = LinkedList.Modifiable.read[ S, Link[ S ]]( in, access )
         new Impl( targets, sourceRef, sinkMap, sinkList )
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

   // TODO: the crappy sinkList is only needed because the id map does not have an iterator...
   // we should really figure out how to add iterator functionality to the id map!!!
   private final class Impl[ S <: Sys[ S ]]( protected val targets: evt.Targets[ S ],
                                             protected val sourceRef: S#Var[ Option[ Link[ S ]]],
                                             protected val sinkMap: IdentifierMap[ S#ID, S#Tx, Link[ S ]],
                                             protected val sinkList: LinkedList.Modifiable[ S, Link[ S ], Unit ])
   extends Scan[ S ]
   with evti.StandaloneLike[ S, Scan.Update[ S ], Scan[ S ]]
   with evti.Generator[ S, Scan.Update[ S ], Scan[ S ]] {
      override def toString = "Scan" + id

      def sinks( implicit tx: S#Tx ) : data.Iterator[ S#Tx, Link[ S ]] = sinkList.iterator

      def addSink( sink: Link[ S ])( implicit tx: S#Tx ) : Boolean = {
         val sinkID  = sink.id
         if( sinkMap.contains( sinkID )) return false

         sinkMap.put( sinkID, sink )
         sinkList.addHead( sink )   // faster than addLast; but perhaps we should use addLast to have a better iterator order?
         sink match {
            case Link.Scan( peer ) => peer.setScanSource( this )  // source_= would create loop!
            case _ =>
         }
         fire( Scan.SinkAdded( this, sink ))
         true
      }

      def removeSink( sink: Link[ S ])( implicit tx: S#Tx ) : Boolean = {
         val sinkID  = sink.id
         if( !sinkMap.contains( sinkID )) return false
         sinkMap.remove( sinkID )
         sinkList.remove( sink )
         fire( Scan.SinkRemoved( this, sink ))
         true
      }

      def source( implicit tx: S#Tx ) : Option[ Link[ S ]] = sourceRef.get

      def source_=( link: Option[ Link[ S ]])( implicit tx: S#Tx ) {
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

         val con = targets.nonEmpty
         sourceRef.set( link )
         old match {
            case Some( Link.Scan( peer )) =>
               peer.removeSink( this )
            case Some( Link.Grapheme( peer )) if( con ) =>
               peer.changed -/-> this
            case _ =>
         }
         if( con ) link match {
            case Some( Link.Grapheme( peer )) =>
               peer.changed ---> this
            case _ =>
         }
         fire( Scan.SourceChanged( this, link ))
         true
      }

      def changed: Event[ S, Scan.Update[ S ], Scan[ S ]] = this

      def connect()( implicit tx: S#Tx ) {
         source match {
            case Some( Scan.Link.Grapheme( peer )) => peer.changed ---> this
            case _ =>
         }
      }

      def disconnect()(implicit tx: S#Tx) {
         source match {
            case Some( Scan.Link.Grapheme( peer )) => peer.changed -/-> this
            case _ =>
         }
      }

      def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ Scan.Update[ S ]] = {
         if( pull.parents( this ).isEmpty ) {
            pull.resolve[ Scan.Update[ S ]]
         } else {
            source.flatMap {
               case Scan.Link.Grapheme( peer ) =>
                  peer.changed.pullUpdate( pull ).map( Scan.SourceUpdate( this, _ ))
               case _ => None
            }
         }
      }

      // called in the implementation from addSink( Link.Scan( _ )). the difference
      // to source_= is that this method should not establish the opposite connection
      // by calling addSink on the source, as this would result in an infinite feedback.
      // still, this method should fire an Scan.SourceChanged event.
      private[proc] def setScanSource( source: Scan[ S ])( implicit tx: S#Tx ) {
         setSource( Some( source: Link[ S ]) )
      }

      protected def writeData( out: DataOutput ) {
         sourceRef.write( out )
         sinkMap.write( out )
         sinkList.write( out )
      }

      protected def disposeData()(implicit tx: S#Tx) {
         sourceRef.dispose()
         sinkMap.dispose()
         sinkList.dispose()
      }

      protected def reader: evt.Reader[ S, Scan[ S ]] = serializer
   }
}
