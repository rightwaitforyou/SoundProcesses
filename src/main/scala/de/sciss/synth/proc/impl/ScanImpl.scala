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

import de.sciss.lucre.{stm, data, event => evt}
import data.SkipList
import stm.Sys
import evt.Event

object ScanImpl {
   import Scan.Link

   sealed trait Update[ S ]

   def apply[ S <: Sys[ S ]]( implicit tx: S#Tx ) : Scan[ S ] = ??? // new Impl[ S ]

   implicit def serializer[ S <: Sys[ S ]] : evt.Serializer[ S, Scan[ S ]] = ???

   private abstract class Impl[ S <: Sys[ S ]]
   extends Scan[ S ] {
      protected def sourceRef : S#Var[ Option[ Link[ S ]]]
      protected def sinkSet : SkipList.Set[ S, Link[ S ]]

      final def sinks( implicit tx: S#Tx ) : data.Iterator[ S#Tx, Link[ S ]] = sinkSet.iterator

      final def addSink( sink: Link[ S ])( implicit tx: S#Tx ) : Boolean = {
         val res = sinkSet.add( sink )
         if( res ) ??? // TODO: fire
         res
      }
      final def removeSink( sink: Link[ S ])( implicit tx: S#Tx ) : Boolean = {
         val res = sinkSet.remove( sink )
         if( res ) ??? // TODO: fire
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
         ??? // TODO: fire
         true
      }

      final def changed: Event[ S, Scan.Update[ S ], Scan[ S ]] = ???

      // called in the implementation from addSink( Link.Scan( _ )). the difference
      // to source_= is that this method should not establish the opposite connection
      // by calling addSink on the source, as this would result in an infinite feedback.
      // still, this method should fire an Scan.SourceChanged event.
      private[proc] def setScanSource( source: Scan[ S ])( implicit tx: S#Tx ) {
         setSource( Some( source: Link[ S ]) )
      }
   }
}
