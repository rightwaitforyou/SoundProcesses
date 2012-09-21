/*
 *  KeyMapImpl.scala
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
import stm.Sys
import data.SkipList
import evt.EventLike
import collection.immutable.{IndexedSeq => IIdxSeq}

object KeyMapImpl {
   trait ValueInfo[ S <: Sys[ S ], Key, Value, ValueUpd ] {
      def valueEvent( value: Value ) : EventLike[ S, ValueUpd, Value ]
      def keySerializer : stm.Serializer[ S#Tx, S#Acc, Key ]
      def valueSerializer : stm.Serializer[ S#Tx, S#Acc, Value ]
   }

   implicit def entrySerializer[ S <: Sys[ S ], Key, Value, ValueUpd ]( implicit info: ValueInfo[ S, Key, Value, ValueUpd ])
   : evt.Serializer[ S, Entry[ S, Key, Value, ValueUpd ]] = new EntrySer

   private final class EntrySer[ S <: Sys[ S ], Key, Value, ValueUpd ]( implicit info: ValueInfo[ S, Key, Value, ValueUpd ])
   extends evt.NodeSerializer[ S, Entry[ S, Key, Value, ValueUpd ]] {
      def read( in: DataInput, access: S#Acc, targets: evt.Targets[ S ])( implicit tx: S#Tx ) : Entry[ S, Key, Value, ValueUpd ] = {
         val key     = info.keySerializer.read( in, access )
         val value   = info.valueSerializer.read( in, access )
         new Entry( targets, key, value )
      }
   }

   final class Entry[ S <: Sys[ S ], Key, Value, ValueUpd ]( protected val targets: evt.Targets[ S ], val key: Key,
                                                             val value: Value )( implicit info: ValueInfo[ S, Key, Value, ValueUpd ])
   extends evt.StandaloneLike[ S, (Key, ValueUpd), Entry[ S, Key, Value, ValueUpd ]] {
      protected def reader: evt.Reader[ S, Entry[ S, Key, Value, ValueUpd ]] = entrySerializer

      def connect()( implicit tx: S#Tx ) {
         info.valueEvent( value ) ---> this
      }

      def disconnect()( implicit tx: S#Tx ) {
         info.valueEvent( value ) -/-> this
      }

      protected def writeData( out: DataOutput ) {
         info.keySerializer.write( key, out )
         info.valueSerializer.write( value, out )
      }

      protected def disposeData()( implicit tx: S#Tx ) {}

      def pullUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Option[ (Key, ValueUpd) ] =
         info.valueEvent( value ).pullUpdate( pull ).map( key -> _ )
   }
}

/**
 * Common building block for implementing reactive maps where the key is a constant element
 * (that is, it does not require updating such as an `S#ID`).
 *
 * @tparam S         the system used
 * @tparam Key       the type of key, such as `String`
 * @tparam Value     the value type, which has an event attached to it (found via `valueInfo`)
 * @tparam ValueUpd  the value updates fired
 */
trait KeyMapImpl[ S <: Sys[ S ], Key, Value, ValueUpd ] {
   _: evt.VirtualNodeSelector[ S ] =>

   protected type Entry = KeyMapImpl.Entry[ S, Key, Value, ValueUpd ]
   protected type Info  = KeyMapImpl.ValueInfo[ S, Key, Value, ValueUpd ]

   /**
    * The underlying non-reactive map
    */
   protected def map: SkipList.Map[ S, Key, Entry ]

   /**
    * Whether the underlying selector is currently connected or not
    */
   protected def isConnected( implicit tx: S#Tx ) : Boolean

   /**
    * Wrap the given set of added and removed keys in an appropriate update message
    * and dispatch it.
    */
   protected def fire( added: Set[ Key ], removed: Set[ Key ])( implicit tx: S#Tx ) : Unit

   /**
    * A helper object providing key and value serialization and an event view of the value.
    */
   protected implicit def valueInfo: Info

   final def get( key: Key )( implicit tx: S#Tx ) : Option[ Value ] = map.get( key ).map( _.value )
   final def keys( implicit tx: S#Tx ): Set[ Key ] = map.keysIterator.toSet

   final def add( key: Key, value: Value )( implicit tx: S#Tx ) {
      val con  = isConnected
      val tgt  = evt.Targets[ S ]   // XXX TODO : partial?
      val n    = new KeyMapImpl.Entry( tgt, key, value )
      val setRemoved: Set[ Key ] = map.add( key -> n ) match {
         case Some( oldNode ) =>
            if( con ) this -= oldNode
            Set( key )
         case _ => Set.empty
      }
      if( con ) {
         this += n
         fire( Set( key ), setRemoved )
      }
   }

   final def remove( key: Key )( implicit tx: S#Tx ) : Boolean = {
      map.remove( key ) match {
         case Some( oldNode ) =>
            val con = isConnected
            if( con ) {
               this -= oldNode
               fire( Set.empty, Set( key ))
            }
            true

         case _ => false
      }
   }

   @inline private def +=( entry: Entry )( implicit tx: S#Tx ) {
      entry ---> this
   }

   @inline private def -=( entry: Entry )( implicit tx: S#Tx ) {
      entry -/-> this
   }

   final def connect()( implicit tx: S#Tx ) {
      map.iterator.foreach { case (_, node) => this += node }
   }
   final def disconnect()( implicit tx: S#Tx ) {
      map.iterator.foreach { case (_, node) => this -= node }
   }

   final protected def foldUpdate( pull: evt.Pull[ S ])( implicit tx: S#Tx ) : Map[ Key, IIdxSeq[ ValueUpd ]] = {
      pull.parents( this ).foldLeft( Map.empty[ Key, IIdxSeq[ ValueUpd ]]) { case (map, sel) =>
         val entryEvt = sel.devirtualize[ (Key, ValueUpd), Entry ]( KeyMapImpl.entrySerializer )
         entryEvt.pullUpdate( pull ) match {
            case Some( (key, upd) ) => map + (key -> (map.getOrElse( key, IIdxSeq.empty ) :+ upd))
            case None => map
         }
      }
   }
}
