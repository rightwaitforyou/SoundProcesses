/*
 *  RichNode.scala
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

import collection.immutable.{IndexedSeq => IIdxSeq}
import concurrent.stm.{Ref => ScalaRef}
import ProcTxn.{FilterMode, IfChanges}
import de.sciss.synth.{ControlABusMap, ControlKBusMap, ControlSetMap, Node}

object RichNode {
   private val EmptyOnEnd = new OnEnd( IIdxSeq.empty, IIdxSeq.empty )
   private final case class OnEnd( direct: IIdxSeq[ () => Unit ], inTxn: IIdxSeq[ ProcTxn => Unit ]) {
      def nonEmpty = direct.nonEmpty || inTxn.nonEmpty
   }
}
abstract class RichNode( val initOnline : Boolean ) /* extends RichObject */ {
   import RichNode._

   final val isOnline: RichState = new RichState( this, "isOnline", initOnline )
//   private val onEndFuns   = Ref( IQueue.empty[ Function1[ ProcTxn, Unit ]])
   private val onEndFuns   = ScalaRef( EmptyOnEnd )

   // ---- constructor ----
//   node.onEnd {
//      ProcTxn.atomic { implicit tx =>
//         isOnline.set( false )
//         val e = onEndFuns.swap( EmptyOnEnd )
//         if( e.nonEmpty ) {
//            tx.afterCommit { _ =>
//               if( e.inTxn.nonEmpty ) ProcTxn.spawnAtomic { implicit tx =>
//                  e.inTxn.foreach { f => try {
//                     f( tx )
//                  } catch {
//                     case ex => ex.printStackTrace()
//                  }}
//               }
//               if( e.direct.nonEmpty ) {
//                  e.direct.foreach { f => try {
//                     f()
//                  } catch {
//                     case ex => ex.printStackTrace()
//                  }}
//               }
//            }
//         }
//      }
//   }

   def onEndTxn( fun: ProcTxn => Unit )( implicit tx: ProcTxn ) {
      onEndFuns.transform( e => e.copy( inTxn = e.inTxn :+ fun ))( tx.peer )
   }

   def onEnd( code: => Unit )( implicit tx: ProcTxn ) {
      onEndFuns.transform( e => e.copy( direct = e.direct :+ (() => code) ))( tx.peer )
   }

////   def onEnd( fun: ProcTxn => Unit )( implicit tx: ProcTxn )
//   def onEnd( code: => Unit )( implicit tx: ProcTxn ) {
//      onEndFuns.transform { queue =>
////         if( queue.isEmpty ) node.onEnd {
////            ProcTxn.spawnAtomic { implicit tx =>
////// since we are now executing the txn only when there are client
////// onEnd functions, it doesn't make sense to re-set the isOnline.
////// i don't think it should be used anyways, as nodes are
////// better created anew each time instead of reusing old ids.
//////               val wasOnline  = isOnline.swap( false )
////               val funs       = onEndFuns.swap( IQueue.empty )
////               funs.foreach( f => try {
////                  f( tx )
////               } catch { case e => e.printStackTrace })
////            }
////         }
//         queue.enqueue( () => code )
//      }
//   }

   def node: Node

   final def server = node.server

   final def read( assoc: (RichAudioBus, String) )( implicit tx: ProcTxn ) : AudioBusNodeSetter = {
      val (rb, name) = assoc
      val reader = BusNodeSetter.reader( name, rb, this )
      registerSetter( reader )
      reader
   }

   final def read( assoc: (RichControlBus, String) )( implicit tx: ProcTxn ) : ControlBusNodeSetter = {
      val (rb, name) = assoc
      val reader = BusNodeSetter.reader( name, rb, this )
      registerSetter( reader )
      reader
   }

   final def write( assoc: (RichAudioBus, String) )( implicit tx: ProcTxn ) : AudioBusNodeSetter = {
      val (rb, name) = assoc
      val writer = BusNodeSetter.writer( name, rb, this )
      registerSetter( writer )
      writer
   }

   final def write( assoc: (RichControlBus, String) )( implicit tx: ProcTxn ) : ControlBusNodeSetter = {
      val (rb, name) = assoc
      val writer = BusNodeSetter.writer( name, rb, this )
      registerSetter( writer )
      writer
   }

   final def readWrite( assoc: (RichAudioBus, String) )( implicit tx: ProcTxn ) : AudioBusNodeSetter = {
      val (rb, name) = assoc
      val rw = BusNodeSetter.readerWriter( name, rb, this )
      registerSetter( rw )
      rw
   }

   final def readWrite( assoc: (RichControlBus, String) )( implicit tx: ProcTxn ) : ControlBusNodeSetter = {
      val (rb, name) = assoc
      val rw = BusNodeSetter.readerWriter( name, rb, this )
      registerSetter( rw )
      rw
   }

   final def map( assoc: (RichAudioBus, String) )( implicit tx: ProcTxn ) : AudioBusNodeSetter = {
      val (rb, name) = assoc
      val mapper = BusNodeSetter.mapper( name, rb, this )
      registerSetter( mapper )
      mapper
   }

   final def map( assoc: (RichControlBus, String) )( implicit tx: ProcTxn ) : ControlBusNodeSetter = {
      val (rb, name) = assoc
      val mapper = BusNodeSetter.mapper( name, rb, this )
      registerSetter( mapper )
      mapper
   }

   private def registerSetter( bns: BusNodeSetter )( implicit tx: ProcTxn ) {
      bns.add
      onEndTxn { tx0 => bns.remove( tx0 )}
   }

   final def free( audible: Boolean = true )( implicit tx: ProcTxn ) {
      tx.add( node.freeMsg, Some( (IfChanges, isOnline, false) ), audible, Map( isOnline -> true ))
   }

   final def set( audible: Boolean, pairs: ControlSetMap* )( implicit tx: ProcTxn ) {
      tx.add( node.setMsg( pairs: _* ), None, audible, Map( isOnline -> true ))
   }

   final def setIfOnline( pairs: ControlSetMap* )( implicit tx: ProcTxn ) {
      // XXX eventually this should be like set with different failure resolution
      if( isOnline.get ) tx.add( node.setMsg( pairs: _* ), None, true, noErrors = true )
//      if( isOnline.get ) tx.add( OSCBundle(
//         OSCMessage( "/error", -1 ), node.setMsg( pairs: _* ), OSCMessage( "/error", -2 )), true )
   }

   final def mapn( audible: Boolean, pairs: ControlKBusMap* )( implicit tx: ProcTxn ) {
      tx.add( node.mapnMsg( pairs: _* ), None, audible, Map( isOnline -> true ))
   }

   final def mapan( audible: Boolean, pairs: ControlABusMap* )( implicit tx: ProcTxn ) {
      tx.add( node.mapanMsg( pairs: _* ), None, audible, Map( isOnline -> true ))
   }

   final def moveToHead( audible: Boolean, group: RichGroup )( implicit tx: ProcTxn ) {
      tx.add( node.moveToHeadMsg( group.group ), None, audible, Map( isOnline -> true, group.isOnline -> true ))
   }

   final def moveToHeadIfOnline( group: RichGroup )( implicit tx: ProcTxn ) {
      if( isOnline.get ) tx.add( node.moveToHeadMsg( group.group ), None, true, Map( group.isOnline -> true ), true )
   }

   final def moveToTail( audible: Boolean, group: RichGroup )( implicit tx: ProcTxn ) {
      tx.add( node.moveToTailMsg( group.group ), None, audible, Map( isOnline -> true, group.isOnline -> true ))
   }

   final def moveBefore( audible: Boolean, target: RichNode )( implicit tx: ProcTxn ) {
      tx.add( node.moveBeforeMsg( target.node ), None, audible, Map( isOnline -> true, target.isOnline -> true ))
   }

   final def moveAfter( audible: Boolean, target: RichNode )( implicit tx: ProcTxn ) {
      tx.add( node.moveAfterMsg( target.node ), None, audible, Map( isOnline -> true, target.isOnline -> true ))
   }
}