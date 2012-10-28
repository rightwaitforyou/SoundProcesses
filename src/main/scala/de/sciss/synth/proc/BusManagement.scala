/*
 *  BusManagement.scala
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

import collection.immutable.{SortedMap => ISortedMap}
import de.sciss.synth.{AudioBus, AudioRated, Bus, ControlBus, ControlRated, Rate}
import concurrent.stm.{Ref => ScalaRef}

sealed trait RichBus {
   def server : RichServer
   def numChannels : Int
   def rate : Rate
}

object RichAudioBus {
   /**
    *    A consumer reading or writing from an audio bus.
    *    Since a RichAudioBus is a meta structure, the
    *    underlying audio bus may change due to optimization.
    *    In this case the consumer is asked to update its
    *    data. Also initial bus allocation is lazy, therefore
    *    when adding the user as reader or writer, the
    *    bus implementation will push its initial allocation
    *    information to the user.    
    */
   trait User /* extends RichBus.User */ {
      def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) : Unit
//      def migrateTo( newBus: RichAudioBus )( implicit tx: ProcTxn ) : Unit
   }
//   type User = RichBus.User[ AudioBus ]
}

trait RichAudioBus extends RichBus with AudioRated {
   import RichAudioBus._

   def busOption( implicit tx: ProcTxn ): Option[ AudioBus ]

   /**
    *    Adds a reading consumer to the bus. Note that
    *    the readers are kept in a Set and this method doesn't
    *    currently check whether the set already contains
    *    the reader. Adding the same reader more than once
    *    will cause malbehaviour.
    *
    *    As a consequence, the user's busChanged method is
    *    invoked with the current bus. The current bus may
    *    change due to the addition. In this case, busChanged
    *    is called on all other currently registered users.
    */
   def addReader( u: User )( implicit tx: ProcTxn ) : Unit

   /**
    *    Adds a writing consumer to the bus. Note that
    *    the writers are kept in a Set and this method doesn't
    *    currently check whether the set already contains
    *    the writer. Adding the same writer more than once
    *    will cause malbehaviour.
    *
    *    As a consequence, the user's busChanged method is
    *    invoked with the current bus. The current bus may
    *    change due to the addition. In this case, busChanged
    *    is called on all other currently registered users.
    */
   def addWriter( u: User )( implicit tx: ProcTxn ) : Unit

   /**
    *    Removes a reading consumer from the bus. It is
    *    safe to call this method, passing in a user which
    *    has already been previously removed.
    *
    *    The current bus may change due to the removal.
    *    In this case, busChanged is called on all
    *    remaining registered users.
    */
   def removeReader( u: User )( implicit tx: ProcTxn ) : Unit

   /**
    *    Removes a writing consumer from the bus. It is
    *    safe to call this method, passing in a user which
    *    has already been previously removed.
    *
    *    The current bus may change due to the removal.
    *    In this case, busChanged is called on all
    *    remaining registered users.
    */
   def removeWriter( u: User )( implicit tx: ProcTxn ) : Unit
}

object RichControlBus {
   trait User /* extends RichBus.User */ {
      def busChanged( bus: ControlBus )( implicit tx: ProcTxn ) : Unit
   }
//   type User = RichBus.User[ ControlBus ]
}

trait RichControlBus extends RichBus with ControlRated {
   import RichControlBus._

   def busOption( implicit tx: ProcTxn ): Option[ ControlBus ]

   /**
    *    Adds a reading consumer to the bus. Note that
    *    the readers are kept in a Set and this method doesn't
    *    currently check whether the set already contains
    *    the reader. Adding the same reader more than once
    *    will cause malbehaviour.
    *
    *    As a consequence, the user's busChanged method is
    *    invoked with the current bus.
    */
   def addReader( u: User )( implicit tx: ProcTxn ) : Unit

   /**
    *    Adds a writing consumer to the bus. Note that
    *    the writers are kept in a Set and this method doesn't
    *    currently check whether the set already contains
    *    the writer. Adding the same writer more than once
    *    will cause malbehaviour.
    *
    *    As a consequence, the user's busChanged method is
    *    invoked with the current bus.
    */
   def addWriter( u: User )( implicit tx: ProcTxn ) : Unit

   /**
    *    Removes a reading consumer from the bus. It is
    *    safe to call this method, passing in a user which
    *    has already been previously removed.
    */
   def removeReader( u: User )( implicit tx: ProcTxn ) : Unit

   /**
    *    Removes a writing consumer from the bus. It is
    *    safe to call this method, passing in a user which
    *    has already been previously removed.
    */
   def removeWriter( u: User )( implicit tx: ProcTxn ) : Unit
}

object RichBus {
//   trait User {
//      def busChanged( bus: Bus )( implicit tx: ProcTxn ) : Unit
//   }

   /**
    *    Constructs a new audio bus proxy for use in a shared environment, where
    *    there can be situations of semi-orphaned buses (only one reader or
    *    only one writer left).
    */
   def audio(   server: RichServer, numChannels: Int ) : RichAudioBus   = new AudioImpl( server, numChannels )
   def control( server: RichServer, numChannels: Int ) : RichControlBus = new ControlImpl( server, numChannels )
   /**
    *    Constructs a new audio bus proxy for use in a short-term temporary fashion.
    *    The implementation does not maintain dummy and empty buses for the case that
    *    there is only one reader or only one writer. As a consequence, it should not
    *    be used in such a scenario, as precious bus indices will be occupied. On the
    *    other hand, this method is useful for internal temporary buses, because when
    *    both a reader and a writer release the resource, there are no spurious
    *    bus re-assignments causing further busChanged notifications (which would go
    *    to concurrently freed nodes).
    */
   def tmpAudio( server: RichServer, numChannels: Int ) : RichAudioBus = new TempAudioImpl( server, numChannels )

   def soundIn( server: RichServer, numChannels: Int, offset: Int = 0 ) : RichAudioBus = {
      val o = server.peer.config
      require( offset +  numChannels <= o.inputBusChannels, "soundIn - offset is beyond allocated hardware channels" )
      FixedImpl( server, AudioBus( server.peer, index = o.outputBusChannels + offset, numChannels = numChannels ))
   }

   def soundOut( server: RichServer, numChannels: Int, offset: Int = 0 ) : RichAudioBus = {
      val o = server.peer.config
      require( offset + numChannels <= o.outputBusChannels, "soundOut - offset is beyond allocated hardware channels" )
      FixedImpl( server, AudioBus( server.peer, index = offset, numChannels = numChannels ))
   }

   def wrap( server: RichServer, bus: AudioBus ) : RichAudioBus = {
      require( server.peer == bus.server )
      FixedImpl( server, bus )
   }

//   trait User {
//      def busChanged( bus: AudioBus )( implicit tx: ProcTxn ) : Unit
//   }

   var verbose = false

   private sealed trait BusHolder[ T <: Bus ] {
      def peer: T
//      def server: RichServer

      private val useCount = ScalaRef( 0 ) // Ref.withCheck( 0 ) { case 0 => peer.free() }

      // increments use count
      final def alloc()( implicit tx: ProcTxn ) {
         implicit val itx = tx.peer
         useCount += 1
         if( verbose ) println( peer.toString + ".alloc -> " + useCount.get )
      }

      // decrements use count and calls `remove` if that count reaches zero
      final def free()( implicit tx: ProcTxn ) {
         implicit val itx = tx.peer
         val cnt = useCount.get - 1
         if( verbose ) println( peer.toString + ".free -> " + cnt )
         require( cnt >= 0 )
         useCount.set( cnt )
         if( cnt == 0 ) {
            remove()
         }
      }

      final def index : Int = peer.index
      final def numChannels : Int = peer.numChannels

      protected def remove()( implicit tx: ProcTxn ) : Unit
   }

   private type AudioBusHolder   = BusHolder[ AudioBus ]
   private type ControlBusHolder = BusHolder[ ControlBus ]

   private type ABusHolderMap = Map[ RichServer, ISortedMap[ Int, AudioBusHolder ]]

   private final class PlainAudioBusHolder( server: RichServer, val peer: AudioBus )
   extends BusHolder[ AudioBus ] {
      protected def remove()( implicit tx: ProcTxn ) {
         server.freeAudioBus( peer.index, peer.numChannels )
      }
   }

   private final class PlainControlBusHolder( server: RichServer, val peer: ControlBus )
   extends BusHolder[ ControlBus ] {
      protected def remove()( implicit tx: ProcTxn ) {
         server.freeControlBus( peer.index, peer.numChannels )
      }
   }

   private final class RichAudioBusHolder( val server: RichServer, val peer: AudioBus, mapScalaRef: ScalaRef[ ABusHolderMap ])
   extends AudioBusHolder {
      def add()( implicit tx: ProcTxn ) {
         mapScalaRef.transform( map => map +
            (server -> (map.getOrElse( server, ISortedMap.empty[ Int, AudioBusHolder ]) + (numChannels -> this)))
         )( tx.peer )
      }

      protected def remove()( implicit tx: ProcTxn ) {
         server.freeAudioBus( peer.index, peer.numChannels )
         mapScalaRef.transform( map => {
            val newMap = map( server ) - numChannels
            if( newMap.isEmpty ) {
               map - server
            } else {
               map + (server -> newMap)
            }
         })( tx.peer )
      }
   }

   // XXX TODO
   private val readOnlyBuses  = ScalaRef( Map.empty[ RichServer, ISortedMap[ Int, AudioBusHolder ]])
   private val writeOnlyBuses = ScalaRef( Map.empty[ RichServer, ISortedMap[ Int, AudioBusHolder ]])

   private def createReadOnlyBus( server: RichServer, numChannels: Int )( implicit tx: ProcTxn ) : AudioBusHolder =
      createRichAudioBus( server, numChannels, readOnlyBuses )

   private def createWriteOnlyBus( server: RichServer, numChannels: Int )( implicit tx: ProcTxn ) : AudioBusHolder =
      createRichAudioBus( server, numChannels, writeOnlyBuses )

   private def createRichAudioBus( server: RichServer, numChannels: Int,
                                   mapScalaRef: ScalaRef[ Map[ RichServer, ISortedMap[ Int, AudioBusHolder ]]])
                                 ( implicit tx: ProcTxn ) : AudioBusHolder = {
      val chanMapO = mapScalaRef.get( tx.peer ).get( server )
      val bus: AudioBusHolder = chanMapO.flatMap( _.from( numChannels ).headOption.map( _._2 )).getOrElse {
         val index   = server.allocAudioBus( numChannels )
         val peer    = AudioBus( server.peer, index = index, numChannels = numChannels )
         val res     = new RichAudioBusHolder( server, peer, mapScalaRef )
         res.add()
         res
      }
      bus
   }

   private def createAudioBus( server: RichServer, numChannels: Int )( implicit tx: ProcTxn ) : AudioBusHolder = {
      val index   = server.allocAudioBus( numChannels )
      val peer    = AudioBus( server.peer, index = index, numChannels = numChannels )
      new PlainAudioBusHolder( server, peer )
   }

   private def createControlBus( server: RichServer, numChannels: Int )( implicit tx: ProcTxn ) : ControlBusHolder = {
      val index   = server.allocControlBus( numChannels )
      val peer    = ControlBus( server.peer, index = index, numChannels = numChannels )
      new PlainControlBusHolder( server, peer )
   }

   private abstract class AbstractAudioImpl extends RichAudioBus {
      import RichAudioBus.{ User => AU }

      final protected val readers   = ScalaRef( Set.empty[ AU ])
      final protected val writers   = ScalaRef( Set.empty[ AU ])
   }

   private final case class FixedImpl( server: RichServer, bus: AudioBus )
   extends AbstractAudioImpl {
      import RichAudioBus.{ User => AU }

      def numChannels   = bus.numChannels

      def busOption( implicit tx: ProcTxn ) : Option[ AudioBus ] = Some( bus )

      def addReader( u: AU )( implicit tx: ProcTxn ) { add( readers, u )}
      def addWriter( u: AU )( implicit tx: ProcTxn ) { add( writers, u )}

      private def add( users: ScalaRef[ Set[ AU ]], u: AU )( implicit tx: ProcTxn ) {
         users.transform( _ + u )( tx.peer )
         u.busChanged( bus  )
      }

      def removeReader( u: AU )( implicit tx: ProcTxn ) { remove( readers, u )}
      def removeWriter( u: AU )( implicit tx: ProcTxn ) { remove( writers, u )}

      private def remove( users: ScalaRef[ Set[ AU ]], u: AU )( implicit tx: ProcTxn ) {
         users.transform( _ - u )( tx.peer )
      }

      override def toString = "h-abus(" + bus + ")"
   }

   private abstract class BasicAudioImpl extends AbstractAudioImpl {
      final protected val bus = ScalaRef.make[ AudioBusHolder ]

      final def busOption( implicit tx: ProcTxn ) : Option[ AudioBus ] = {
         val bh = bus.get( tx.peer )
         if( bh != null ) Some( bh.peer ) else None
      }
   }

   private final class AudioImpl( val server: RichServer, val numChannels: Int ) extends BasicAudioImpl {
      import RichAudioBus.{ User => AU }

      override def toString = "sh-abus(numChannels=" + numChannels + ")@" + hashCode

      def addReader( u: AU )( implicit tx: ProcTxn ) {
         implicit val itx = tx.peer
         val rs   = readers.get
         require( !rs.contains( u ))
         val bh   = if( rs.isEmpty ) {
            val ws = writers.get
            if( ws.isEmpty ) { // no bus yet, create an empty shared one
               val res = createReadOnlyBus( server, numChannels )
               bus.set( res )
//println( "addReader : " + this + " ; allocReadOnlyBus " + res )
               res
            } else { // dispose old dummy bus, create new bus
               val res        = createAudioBus( server, numChannels )
               val newBus     = res.peer // AudioBus( server.peer, index = res.index, numChannels = numChannels )
               val oldHolder  = bus.swap( res )
               rs.foreach { r =>
                  oldHolder.free()
                  r.busChanged( newBus )
                  res.alloc()
               }
               ws.foreach { w =>
                  oldHolder.free()
                  w.busChanged( newBus )
                  res.alloc()
               }
//println( "addReader : " + this + " ; allocAudioBus " + res )
               res
            }
         } else { // re-use existing bus
            bus.get
//            val res = new AudioBus( server, bh.index, numChannels )
//println( "addReader : " + this + " ; re-alloc " + res )
//            res
         }
         readers.set( rs + u )
         // always perform this on the newly added
         // reader no matter if the bus is new:
         bh.alloc()
         val newBus = bh.peer // AudioBus( server.peer, index = bh.index, numChannels = numChannels )
         u.busChanged( newBus )
      }

      def addWriter( u: AU )( implicit tx: ProcTxn ) {
         implicit val itx = tx.peer
         val ws   = writers.get
         require( !ws.contains( u ))
         val bh   = if( ws.isEmpty ) {
            val rs = readers.get
            if( rs.isEmpty ) { // no bus yet, create an empty shared one
               val res = createWriteOnlyBus( server, numChannels )
               bus.set( res )
               res
            } else { // dispose old dummy bus, create new bus
               val res        = createAudioBus( server, numChannels )
               val newBus     = res.peer // AudioBus( server.peer, index = res.index, numChannels = numChannels )
               val oldHolder  = bus.swap( res )
               rs foreach { r =>
                  oldHolder.free()
                  r.busChanged( newBus )
                  res.alloc()
               }
               ws foreach { w =>
                  oldHolder.free()
                  w.busChanged( newBus )
                  res.alloc()
               }
               res
            }
         } else { // re-use existing bus
            bus.get
         }
         writers.set( ws + u )
         // always perform this on the newly added
         // reader no matter if the bus is new:
         bh.alloc()
         val newBus = bh.peer // new AudioBus( server, bh.index, numChannels )
         u.busChanged( newBus )
      }

      def removeReader( u: AU )( implicit tx: ProcTxn ) {
         implicit val itx = tx.peer
         val rs0        = readers()
         if( !rs0.contains( u )) return
         val rs         = rs0 - u
         readers.set( rs )
         val oldHolder  = bus()
         oldHolder.free()
         if( rs.isEmpty ) {
            val ws = writers()
            if( ws.nonEmpty ) { // they can all go to write only
               val bh = createWriteOnlyBus( server, numChannels )
               bus.set( bh )
               val newBus = bh.peer // new AudioBus( server, bh.index, numChannels )
               ws foreach { w =>
                  oldHolder.free()
                  w.busChanged( newBus )
                  bh.alloc()
               }
            }
         }
      }

      def removeWriter( u: AU )( implicit tx: ProcTxn ) {
         implicit val itx = tx.peer
         val ws0        = writers.get
         if( !ws0.contains( u )) return
         val ws         = ws0 - u
         writers.set( ws )
         val oldHolder  = bus.get
         oldHolder.free()
         if( ws.isEmpty ) {
            val rs = readers.get
            if( rs.nonEmpty ) { // they can all go to write only
               val bh = createReadOnlyBus( server, numChannels )
               bus.set( bh )
               val newBus = bh.peer // new AudioBus( server, bh.index, numChannels )
               rs foreach { r =>
                  oldHolder.free()
                  r.busChanged( newBus )
                  bh.alloc()
               }
            }
         }
      }
   }

   private final class TempAudioImpl( val server: RichServer, val numChannels: Int ) extends BasicAudioImpl {
      import RichAudioBus.{ User => AU }

      override def toString = "tmp-abus(numChannels=" + numChannels + ")@" + hashCode

      def addReader( u: AU )( implicit tx: ProcTxn ) { add( readers, writers, u )}
      def addWriter( u: AU )( implicit tx: ProcTxn ) { add( writers, readers, u )}

      private def add( users: ScalaRef[ Set[ AU ]], others: ScalaRef[ Set[ AU ]], u: AU )( implicit tx: ProcTxn ) {
         implicit val itx = tx.peer
         val us = users.get
         require( !us.contains( u ))
         // do _not_ check for null
         // because we might have a disposed
         // bus there, so we must make sure to
         // re-allocate a new bus each time
         // the users count goes to 1!
         val bh   = if( us.isEmpty && others.get.isEmpty ) {
            val res = createAudioBus( server, numChannels )
            bus.set( res )
            res
         } else { // re-use existing bus
            bus.get
         }
         users.set( us + u )
         // always perform this on the newly added
         // reader no matter if the bus is new:
         bh.alloc()
         val newBus = bh.peer // new AudioBus( server, bh.index, numChannels )
         u.busChanged( newBus )
      }

      def removeReader( u: AU )( implicit tx: ProcTxn ) { remove( readers, u )}
      def removeWriter( u: AU )( implicit tx: ProcTxn ) { remove( writers, u )}

      private def remove( users: ScalaRef[ Set[ AU ]], u: AU )( implicit tx: ProcTxn ) {
         implicit val itx = tx.peer
         val rw = users.get
         if( !rw.contains( u )) return
         users.set( rw - u )
         bus.get.free()
      }
   }

   private final class ControlImpl( val server: RichServer, val numChannels: Int ) extends RichControlBus {
      import RichControlBus.{ User => CU }

      private val bus      = ScalaRef.make[ ControlBusHolder ]
      private val readers  = ScalaRef( Set.empty[ CU ])
      private val writers  = ScalaRef( Set.empty[ CU ])

      override def toString = "cbus(numChannels=" + numChannels + ")@" + hashCode

      def busOption( implicit tx: ProcTxn ) = {
         val bh = bus.get( tx.peer )
         if( bh != null ) Some( bh.peer ) else None
      }

      def addReader( u: CU )( implicit tx: ProcTxn ) { add( readers, writers, u )}
      def addWriter( u: CU )( implicit tx: ProcTxn ) { add( writers, readers, u )}

      private def add( users: ScalaRef[ Set[ CU ]], others: ScalaRef[ Set[ CU ]], u: CU )( implicit tx: ProcTxn ) {
         implicit val itx = tx.peer
         val us = users.get
         require( !us.contains( u ))
         // do _not_ check for null
         // because we might have a disposed
         // bus there, so we must make sure to
         // re-allocate a new bus each time
         // the users count goes to 1!
         val bh   = if( us.isEmpty && others.get.isEmpty ) {
            val res = createControlBus( server, numChannels )
            bus.set( res )
            res
         } else { // re-use existing bus
            bus.get
         }
         users.set( us + u )
         // always perform this on the newly added
         // reader no matter if the bus is new:
         bh.alloc()
         val newBus = bh.peer // new ControlBus( server, bh.index, numChannels )
         u.busChanged( newBus )
      }

      def removeReader( u: CU )( implicit tx: ProcTxn ) { remove( readers, u )}
      def removeWriter( u: CU )( implicit tx: ProcTxn ) { remove( writers, u )}

      private def remove( users: ScalaRef[ Set[ CU ]], u: CU )( implicit tx: ProcTxn ) {
         implicit val itx = tx.peer
         val rw = users.get
         if( !rw.contains( u )) return
         users.set( rw - u )
         bus.get.free()
      }
   }
}