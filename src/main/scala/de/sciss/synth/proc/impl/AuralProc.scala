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

package de.sciss.synth
package proc
package impl

import concurrent.stm.Ref
import de.sciss.lucre.stm

object AuralProc {
//   implicit object Serializer extends stm.Serializer[ AuralProc ] {
//      def write( v: AuralProc, out: DataOutput ) { v.write( out )}
//      def read( in: DataInput ) : AuralProc = {
//         val name = in.readString()
//         new Impl( name )
//      }
//   }

   def apply( synth: RichSynth, outBuses: Map[ String, RichAudioBus ], busUsers: Iterable[ DynamicBusUser ]) : AuralProc = {
      new Impl( synth, outBuses, busUsers )
   }

   /*
    * The possible differentiation of groups for an aural process. The minimum configuration is one main
    * group. If synths need to play before the main process, a pre group will be established, if they need
    * to play after the main process, a post group will be established. If multiple synths participate in
    * the main process, a core group may be established. A back group will hold 'forgotten' synths, once
    * they have been marked to fade out and be removed by themselves.
    */
   private final case class AllGroups( main: RichGroup, pre: Option[ RichGroup ] = None,
                                       core: Option[ RichGroup ] = None,
                                       post: Option[ RichGroup ] = None, back: Option[ RichGroup ] = None )

   private final class Impl( synth: RichSynth, outBuses: Map[ String, RichAudioBus ], busUsers: Iterable[ DynamicBusUser ])
   extends AuralProc {

      private val groupsRef   = Ref[ Option[ AllGroups ]]( None )
      
//      private val groupRef    = Ref( Option.empty[ RichGroup ])
//      private val synthRef    = Ref( Option.empty[ RichSynth ])
//      private val nameRef     = Ref( name0 )
//      private val graphRef    = Ref( graph0 )
//      private val synthDefRef = Ref( Option.empty[ RichSynthDef ])

//      private val freqRef     = Ref( freq0 )
//      private val entriesRef  = Ref( entries0 )

//      def group( implicit tx: ProcTxn ) : Option[ RichGroup ] = groupRef.get( tx.peer )

//      def graph( implicit tx: ProcTxn ) : SynthGraph          = graphRef.get( tx.peer )
//      def graph_=( g: SynthGraph )( implicit tx: ProcTxn ) {
//         graphRef.set( g )( tx.peer )
//         if( playing ) {
//            stop()
//            play()
//         }
//      }

//      private def runningTarget()( implicit tx: ProcTxn ) : (RichNode, AddAction) = {
//         groupsRef.get( tx.peer ) map { all =>
//            all.core map { core =>
//               core -> addToHead
//            } getOrElse { all.pre map { pre =>
//               pre -> addAfter
//            } getOrElse { all.post map { post =>
//               post -> addBefore
//            } getOrElse {
//               all.main -> addToTail
//            }}}
//         } getOrElse {
//            RichGroup.default( server ) -> addToHead
//         }
//      }

      override def toString = "AuralProc(" + synth + ", " + outBuses + ")"

      def server = synth.server

      def groupOption( implicit tx: ProcTxn ) : Option[ RichGroup ] = groupsRef.get( tx.peer ).map( _.main )

      def group()( implicit tx: ProcTxn ) : RichGroup = {
         groupOption.getOrElse {
            val res = RichGroup( server )
            res.play( server.defaultGroup )
            group_=( res )
            res
         }
      }

      private def group_=( newGroup: RichGroup )( implicit tx: ProcTxn ) {
         implicit val itx = tx.peer
         groupsRef.transform( _ map { all =>
            moveAllTo( all, newGroup )
            all.main.free( audible = true ) // que se puede...?
            all.copy( main = newGroup )
         } orElse {
            val all = AllGroups( main = newGroup )
//            runningRef().foreach( _.setGroup( newGroup ))

//            synthRef().foreach { synth =>
               synth.moveToHead( audible = true, group = newGroup )
//            }
            Some( all )
         })
      }

      @inline private def preGroupOption( implicit tx: ProcTxn ) : Option[ RichGroup ] = groupsRef.get( tx.peer ).flatMap( _.pre )

      def preGroup()( implicit tx: ProcTxn ) : RichGroup = {
         implicit val itx = tx.peer
         preGroupOption.getOrElse {
            val res     = RichGroup( server )
            /* val main = */ group()      // creates group if necessary
            val all     = groupsRef().get
val target = anchorNode
val addAction = addBefore
//            val (target, addAction) = anchorNodeOption map { core =>
//               core -> addBefore
//            } getOrElse { all.post map { post =>
//               post -> addBefore
//            } getOrElse {
//               main -> addToTail
//            }}
            res.play( target, addAction )
            groupsRef.set( Some( all.copy( pre = Some( res ))))
            res
         }
      }

      private def anchorNode( implicit tx: ProcTxn ) : RichNode = {
         implicit val itx = tx.peer
//         groupsRef().flatMap( _.core ) orElse synthRef() // runningRef().map( _.anchorNode )
         groupsRef().flatMap( _.core ) getOrElse synth // runningRef().map( _.anchorNode )
      }

      private def moveAllTo( all: AllGroups, newGroup: RichGroup )( implicit tx: ProcTxn ) {
         val core = anchorNode
//         anchorNodeOption map { core =>
            core.moveToTail( audible = true, group = newGroup )
            all.pre.foreach(  _.moveBefore( audible = true, target = core ))
            all.post.foreach( _.moveAfter(  audible = true, target = core ))
//         } getOrElse {
//            all.post.map { post =>
//               post.moveToTail( audible = true, group = newGroup )
//               all.pre.foreach( _.moveBefore( audible = true, target = post ))
//            } getOrElse {
//               all.pre.foreach( _.moveToTail( audible = true, group = newGroup ))
//            }
//         }
         all.back.foreach( _.moveToHeadIfOnline( newGroup )) // ifOnline !
      }

//      def name( implicit tx: ProcTxn ) : String = nameRef.get( tx.peer )
//      def name_=( n: String )( implicit tx: ProcTxn ) { nameRef.set( n )( tx.peer )}

//      def play()( implicit tx: ProcTxn ) {
////         val gr         = graph
////         val df         = ProcDemiurg.getSynthDef( server, gr )
//
//         implicit val itx = tx.peer
//         val (target, addAction) = runningTarget()
////         val args: Seq[ ControlSetMap ] = entriesRef.get.map( tup => tup: ControlSetMap )( breakOut )
//         val bufs       = Seq.empty[ RichBuffer ]
//
//         val synth      = df.play( target, args, addAction, bufs )
//
//         val old        = synthRef.swap( Some( synth ))
//         old.foreach( _.free() )
//      }

      def stop()( implicit tx: ProcTxn ) {
//         val synth = synthRef.swap( None )( tx.peer )
//         synth.foreach( _.free() )
         synth.free()
         busUsers.foreach( _.remove() )
      }

//      def playing( implicit tx: ProcTxn ) : Boolean = synthRef.get( tx.peer ).map( _.isOnline.get ).getOrElse( false )
//      def playing_=( p: Boolean )( implicit tx: ProcTxn ) {
//         if( p ) play() else stop()
//      }

      // XXX if they stay static that way, we can remove the tx argument
      def getBus( key: String )( implicit tx: ProcTxn ) : Option[ RichAudioBus ] = outBuses.get( key )
//      def setBus( key: String, bus: Option[ RichAudioBus ]) { ?? }

//      def addParams( map: Map[ String, Param ])( implicit tx: ProcTxn ) {
//         if( map.nonEmpty ) {
//            implicit val itx = tx.peer
//            entriesRef.transform( _ ++ map )
//            synthRef.get.foreach( _.set( true, map.map( tup => tup: ControlSetMap )( breakOut ): _* ))
//         }
//      }
   }
}
sealed trait AuralProc /* extends Writer */ {
   def server : RichServer
//   def name( implicit tx: ProcTxn ) : String
//   def name_=( n: String )( implicit tx: ProcTxn ) : Unit

   /**
    *    Retrieves the main group of the Proc, or
    *    returns None if a group has not yet been assigned.
    */
   def groupOption( implicit tx: ProcTxn ) : Option[ RichGroup ]
   /**
    *    Retrieves the main group of the Proc. If this
    *    group has not been assigned yet, this method will
    *    create a new group.
    */
   def group()( implicit tx: ProcTxn ) : RichGroup
   def preGroup()( implicit tx: ProcTxn ) : RichGroup

//   def play()( implicit tx: ProcTxn ) : Unit
//   def playing( implicit tx: ProcTxn ) : Boolean
//   def playing_=( p: Boolean )( implicit tx: ProcTxn ) : Unit
   def stop()( implicit tx: ProcTxn ) : Unit

//   def addParams( map: Map[ String, Param ])( implicit tx: ProcTxn ) : Unit

//   def graph( implicit tx: ProcTxn ) : SynthGraph
//   def graph_=( g: SynthGraph )( implicit tx: ProcTxn ) : Unit

   def getBus( key: String )( implicit tx: ProcTxn ) : Option[ RichAudioBus ]
//   def setBus( key: String, bus: Option[ RichAudioBus ])

//   def freq( implicit tx: ProcTxn ) : Double
//   def freq_=( f: Double )( implicit tx: ProcTxn ) : Unit
}

// final case class AuralView[ S <: stm.Sys[ S ]]( proc: stm.Source[ S#Tx, Proc[ S ]])